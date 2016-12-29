package toolc
package analyzer

import toolc.analyzer.Symbols._
import toolc.ast.Trees
import toolc.ast.Trees._
import toolc.utils._

import scala.collection.mutable.{Set => MSet}

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def collectSymbols(prog: Program): GlobalScope = {

      val global = new GlobalScope

      val mcSym = new MainSymbol(prog.main.id.value)
      global.mainClass = mcSym
      prog.main.setSymbol(mcSym)
      prog.main.id.setSymbol(mcSym)

      /** Create empty symbols for all classes, checking that their names are unique, and that
        * they aren't named as Object
        */
      for (c <- prog.classes) {
        if (c.id.value == "Object")
          ctx.reporter.error("A class cannot be named: Object")
        else if (c.id.value == prog.main.id.value)
          ctx.reporter.error("A class can't have the same name as program")

        val newClass = c match {
          case cd: ClassDecl => new ClassSymbol(cd.id.value).setPos(cd.file, cd.line)
          case vcd: ValueClassDecl =>
            if(vcd.vars.size != 1) fatal("Value class has exactly one field")
            new ValueClassSymbol(vcd.id.value, vcd.vars.head.id.value).setPos(vcd.file, vcd.line)
        }

        global.lookupClass(c.id.value) match {
          case Some(_: AbstractClassSymbol) => ctx.reporter.error("Duplicated class name !", newClass)
          case None =>
              global.classes = global.classes + (c.id.value -> newClass)
              c.setSymbol(newClass)
              c.id.setSymbol(newClass)
        }
      }

      // Set parent Symbols
      for {
        cls <- prog.classes
        clSym = global.classes(cls.id.value)
        par <- cls.parent
      } yield {
        global.lookupClass(par.value) match {
          case  None =>
            error(s"Class ${clSym.name} extends class ${par.value} which is not defined.", par)
          case Some(_: ValueClassSymbol) => ctx.reporter.error("A value class cannot be extended")
          case Some(parSym: ClassSymbol) =>
            clSym.parent = Some(parSym)
            par.setSymbol(parSym)
          case _ => ctx.reporter.error("A value class cannot extend anything")
        }
      }

      // Check there are no cycles in the inheritance graph
      prog.classes foreach { cls =>
        val clsSym = cls.getSymbol

        def mkChain(curr: AbstractClassSymbol): List[AbstractClassSymbol] = {
          curr.parent match {
            case None => List(curr)
            case Some(`clsSym`) => List(curr, clsSym)
            case Some(p) => curr :: mkChain(p)
          }
        }

        val chain = mkChain(clsSym)

        if (chain.size > 1 && chain.head == chain.last) {
          fatal("Cyclic inheritance: " + chain.map(_.name).mkString(" -> "))
        }
      }

      // mutable Set used to check that we don't collect arguments and methods of a class multiple times
      val alreadyDoneClasses = MSet[String]()

      // We now know that every class is unique and the inheritance graph is
      // correct. We proceed to check the contents of these classes.
      prog.classes.foreach(collectInClass)

      def collectInClass(c: Class): Unit = {
        //Recursively collect parents
        if(c.parent.isDefined) collectInClass(prog.classes.find(_.id == c.parent.get).get)

        // Make sure that we don't collects methods and variables for a class that has already been done.
        // (e.g., a parent that was done previously but his position in prog.classes is after a class that extend it)
        // I used a mutable Set in order to have constant time lookup complexity, and avoid doing some useless computations.
        if (!alreadyDoneClasses.contains(c.id.value)) {
          alreadyDoneClasses += c.id.value
          global.lookupClass(c.id.value) match {
            case None => sys.error("A symbol was not declared for this class.")
            case Some(cl: AbstractClassSymbol) =>
              //Define the methods of the class
              for (function <- c.methods) collectMethods(cl, function)
              //Define the variables of the class
              for (variable <- c.vars) collectFields(cl, variable)
          }
        }
      }

      /**
        * Collect and set symbols for methods inside the class and check if the method is overriding a parent method.
        * It also collect and set the symbols of the arguments and local variables of the method.
        */
      def collectMethods(myClass: Symbols.AbstractClassSymbol, function: Trees.MethodDecl): Unit = {
        val newMethod = new MethodSymbol(function.id.value, myClass)

        //Look if the parent have this method
        myClass.lookupMethod(function.id.value) match {
          // New method
          case None =>
            myClass.methods = myClass.methods + (function.id.value -> newMethod)
            function.setSymbol(newMethod)
            function.id.setSymbol(newMethod)
            collectVariableInMethods(myClass, function)
            collectArgsInMethods(myClass, function)
          // Already define in parent or inside the class
          case Some(m) =>
            if (m.classSymbol.name == myClass.name)
              ctx.reporter.error(s"Two function with the same name inside ${myClass.name}")
            else if (m.argList.size == function.args.size) {
              //If parent have a method with the same name and same number of parameters, we override it
              newMethod.overridden = Some(m)
              myClass.methods = myClass.methods + (function.id.value -> newMethod)
              function.setSymbol(newMethod)
              function.id.setSymbol(newMethod)
              collectVariableInMethods(myClass, function)
              collectArgsInMethods(myClass, function)
            } else {
              //Otherwise it's just a function with the same name but different parameters
              ctx.reporter.error(s"Overloading ${function.id.value} is not permitted in Tool")
            }
        }
      }

      /**
        * Collect and set symbols for variables inside the class and check that there is no duplication or overload.
        */
      def collectFields(myClass: Symbols.AbstractClassSymbol, variable: Trees.VarDecl): Unit = {
        val newVar = new VariableSymbol(variable.id.value)

        //Look if the parent already have this field
        myClass.lookupVar(variable.id.value) match {
          case None =>
            myClass.members = myClass.members + (variable.id.value -> newVar)
            variable.setSymbol(newVar)
            variable.id.setSymbol(newVar)
          case Some(_) =>
            ctx.reporter.error(s"Two fields ${variable.id.value} with the same name in ${myClass.name}!")
        }
      }

      /**
        * Collect and set symbols of variables inside the given method
        */
      def collectVariableInMethods(myClass: Symbols.AbstractClassSymbol, method: Trees.MethodDecl): Unit = {
        myClass.lookupMethod(method.id.value) match {
          case None => sys.error("Method was not declared !")
          case Some(methodSymbol) =>
            for (variable <- method.vars) {
              val newVar = new VariableSymbol(variable.id.value)

              methodSymbol.lookupVar(variable.id.value) match {
                // New variable
                case None =>
                  methodSymbol.members = methodSymbol.members + (variable.id.value -> newVar)
                  variable.setSymbol(newVar)
                  variable.id.setSymbol(newVar)
                // Variable might have the same name as an argument or being duplicate inside the method.
                case Some(v) =>
                  if (methodSymbol.params.contains(v.name) || methodSymbol.members.contains(v.name))
                    ctx.reporter.error("An argument and a local variable can't have the same name !")
                  else {
                    // Shadowing a class variable
                    methodSymbol.members = methodSymbol.members + (variable.id.value -> newVar)
                    variable.setSymbol(newVar)
                    variable.id.setSymbol(newVar)
                  }
              }
            }

        }
      }

      /**
        * Collect and set symbols of the arguments of the given method
        */
      def collectArgsInMethods(myClass: Symbols.AbstractClassSymbol, method: Trees.MethodDecl): Unit = {
        myClass.lookupMethod(method.id.value) match {
          case None => sys.error("Method was not declared !")
          case Some(methodSymbol) =>
            for (args <- method.args) {
              val newArg = new VariableSymbol(args.id.value)

              methodSymbol.lookupVar(args.id.value) match {
                // New argument
                case None =>
                  methodSymbol.params = methodSymbol.params + (args.id.value -> newArg)
                  methodSymbol.argList = methodSymbol.argList :+ newArg
                  args.setSymbol(newArg)
                  args.id.setSymbol(newArg)
                // Argument might have the same name as a variable or two arguments have the same name
                case Some(a) =>
                  if (methodSymbol.params.contains(a.name) || methodSymbol.members.contains(a.name))
                    ctx.reporter.error("An argument and a local variable can't have the same name !")
                  else {
                    // Shadowing a class variable
                    methodSymbol.params = methodSymbol.params + (args.id.value -> newArg)
                    methodSymbol.argList = newArg :: methodSymbol.argList
                    args.setSymbol(newArg)
                    args.id.setSymbol(newArg)
                  }
              }
            }

        }
      }

      global
    }

    def setPSymbols(prog: Program, gs: GlobalScope): Unit = {
      // Set statements in program
      for (stat <- prog.main.stats) {
        setSSymbols(stat)(gs, None)
      }

      for (c <- prog.classes) {
        gs.lookupClass(c.id.value) match {
          case Some(_) => setCSymbols(c, gs)
          case None => sys.error("Class is not declared !")
        }
      }
    }

    def setCSymbols(klass: Class, gs: GlobalScope): Unit = {
      // Make sure we start by setting the type of the parent class (to check overriding correctly)
      if(klass.parent.isDefined) setCSymbols(prog.classes.find(_.id == klass.parent.get).get, gs)

      val classSym = gs.lookupClass(klass.id.value).get
      for (varDecl <- klass.vars) {
        setTypeSymbol(varDecl.tpe, gs)
        // Set their type
        classSym.lookupVar(varDecl.id.value) match {
          case Some(x) => x.setType(varDecl.tpe.getType)
          case None =>
        }
      }

      klass.methods.foreach(setMSymbols(_, gs, classSym))
    }

    def setMSymbols(meth: MethodDecl, gs: GlobalScope, cs: AbstractClassSymbol): Unit = {
      val methodSymbol = cs.lookupMethod(meth.id.value)

      // Set return expression and his type
      setTypeSymbol(meth.retType, gs)
      setESymbols(meth.retExpr)(gs, methodSymbol)

      // Set type method
      methodSymbol.get.setType(meth.retType.getType)

      // Set type of the arguments
      for(variable <- meth.vars) {
        setTypeSymbol(variable.tpe, gs)
        methodSymbol.get.lookupVar(variable.id.value) match {
          case Some(v) => v.setType(variable.tpe.getType)
          case None =>
        }
      }

      // Set type of the variable
      for(arg <- meth.args) {
        setTypeSymbol(arg.tpe, gs)
        methodSymbol.get.lookupVar(arg.id.value) match {
          case Some(a) => a.setType(arg.tpe.getType)
          case None =>
        }
      }

      // Set the statements
      meth.stats.foreach(stat => setSSymbols(stat)(gs, methodSymbol))

      // TypeCheck Override if it exists
      methodSymbol.get.overridden match {
        case Some(ms) =>
          if(ms.getType != methodSymbol.get.getType) ctx.reporter.error(s"Overloading ${methodSymbol.get.name} is not permitted in Tool")
          val listTupleArgs = methodSymbol.get.params.values.map(arg => arg.getType).zip(ms.params.values.map(x => x.getType))
          val matchArgs = listTupleArgs.forall(tpe => tpe._1 == tpe._2)
          if(!matchArgs) ctx.reporter.error(s"Overloading ${methodSymbol.get.name} is not permitted in Tool")
        case None =>
      }

    }

    def setSSymbols(stat: StatTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = {
      stat match {
        case Block(stats: List[StatTree]) =>
          stats.foreach(stat => setSSymbols(stat))
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
          setESymbols(expr)
          setSSymbols(thn)
          if(els.isDefined) setSSymbols(els.get)
        case While(expr: ExprTree, stat: StatTree) =>
          setESymbols(expr)
          setSSymbols(stat)
        case Println(expr: ExprTree) =>
          setESymbols(expr)
        case Assign(id: Identifier, expr: ExprTree) =>
          setISymbol(id)
          setESymbols(expr)
          ms match {
            case Some(methodSymbol) =>
              methodSymbol.classSymbol match {
                case vcs: ValueClassSymbol if id.getSymbol.id == vcs.getField.get.id =>
                  ctx.reporter.error(s"Cannot reassign the field of value class, in ${vcs.name}")
                case _ =>
              }
            case _ =>
          }

        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
          setISymbol(id)
          setESymbols(index)
          setESymbols(expr)
        case DoExpr(e: ExprTree) =>
          setESymbols(e)
      }
    }

    def setISymbol(id: Identifier)(implicit ms: Option[MethodSymbol]) = {
      // in this context, it will always be an expression (variable)
      ms.flatMap(_.lookupVar(id.value)) match {
        case None =>
          error("Undeclared identifier: " + id.value + ".", id)
        case Some(sym) =>
          id.setSymbol(sym)
      }
    }

    def setESymbols(expr: ExprTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = {
      expr match {
        case And(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Or(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Plus(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Minus(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Times(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Div(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case LessThan(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Equals(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case ArrayRead(arr: ExprTree, index: ExprTree) =>
          setESymbols(arr)
          setESymbols(index)
        case ArrayLength(arr: ExprTree) =>
          setESymbols(arr)
        case MethodCall(obj: ExprTree, _: Identifier, args: List[ExprTree]) =>
          setESymbols(obj)
          args.foreach(arg => setESymbols(arg))
        case Variable(id: Identifier) =>
          setISymbol(id)
        case x@This() => ms match {
          case None => sys.error("Error on This()")
          case Some(methodSymbol) => x.setSymbol(methodSymbol.classSymbol)
        }
        case NewIntArray(size: ExprTree) =>
          setESymbols(size)
        case New(tpe: Identifier) =>
          gs.lookupClass(tpe.value) match {
            case None => ctx.reporter.error("The object does not exists")
            case Some(classSymbol: ClassSymbol) => tpe.setSymbol(classSymbol)
            case Some(vcs: ValueClassSymbol) =>
              ctx.reporter.error(s"Cannot instantiate ${vcs.name} without specifying the field")
           }

          /* Project extension */
        case NewValueClass(tpe, expr: ExprTree) =>
          gs.lookupClass(tpe.value) match {
            case None => ctx.reporter.error("The object does not exists")
            case Some(cs: ClassSymbol) =>
              ctx.reporter.error(s"You cannot instantiate ${cs.name} as a value class")
            case Some(vcs: ValueClassSymbol) =>
              tpe.setSymbol(vcs)
          }
          setESymbols(expr)

        case Not(expr: ExprTree) =>
          setESymbols(expr)
        case _ =>
      }
    }

    def setTypeSymbol(tpe: TypeTree, gs: GlobalScope): Unit = {
      tpe match {
        case ClassType(id: Identifier) =>
          gs.lookupClass(id.value) match {
            case None => ctx.reporter.error("Not the expected class")
            case Some(cs: ClassSymbol) => id.setSymbol(cs)
            case Some(vcs: ValueClassSymbol) => id.setSymbol(vcs)
          }

        case _ =>
      }
    }

    val gs = collectSymbols(prog)

    terminateIfErrors()

    setPSymbols(prog, gs)

    prog
  }
}
