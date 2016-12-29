package toolc
package code

import cafebabe.AbstractByteCodes.{New => _, _}
import cafebabe.ByteCodes._
import cafebabe._
import toolc.analyzer.Symbols._
import toolc.analyzer.Types._
import toolc.ast.Trees.{IntLit, StringLit, _}
import toolc.utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  // A mapping from a parameter/local variable name to the index of this parameter/variable
  // in the fields of a method
  type LocalsPosMapping = Map[String,Int]

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /**** Helper methods ****/

    def generateClassFile(ct: Class, shortFileName: String, outDir: String): Unit = {
      val cs = ct.getSymbol
      val cf = new ClassFile(cs.name, cs.parent.map(_.name))
      cf.setSourceFile(shortFileName)

      /* Project Extension, no default constructor for value classes*/
      cs match {
        case _: ClassSymbol => cf.addDefaultConstructor
        case _ =>
      }

      // Add class fields
      for((name, symbol) <- cs.members) cf.addField(typeToDescr(symbol.getType), name)
      // Add class methods and generate code for them
      for(method <- ct.methods) addMethod(cf, method.getSymbol, method)

      writeClassFile(cf, outDir, cs.name)
    }

    def addMethod(cf: ClassFile, method: MethodSymbol, decl: MethodDecl): Unit = {

      def buildMethodHandler(args: List[String]): MethodHandler = {
        cf.addMethod(typeToDescr(decl.retType.getType), method.name, args)
      }

      /* Projet Extension
        Force value class methods to have the field as an argument. Since they are declared
        as static methods, they need to have access to the field.
       */
      val mh = method.classSymbol match {
        case vcs: ValueClassSymbol =>
          val tmp = buildMethodHandler(typeToDescr(vcs.getField.get.getType) :: method.argList.map(a => typeToDescr(a.getType)))
          tmp.setFlags(Flags.METHOD_ACC_STATIC)
          tmp
        case _ => buildMethodHandler(method.argList.map(a => typeToDescr(a.getType)))
      }

      cGenMethod(mh.codeHandler, decl)
    }

    def generateMainClassFile(main: MainObject, sourceFileName: String, outDir: String): Unit = {
      // Main class has a special handling
      val cs = main.getSymbol
      val mainClassFile = new ClassFile(cs.name, None)
      mainClassFile.setSourceFile(sourceFileName)
      mainClassFile.addDefaultConstructor

      cGenMain(
        mainClassFile.addMainMethod.codeHandler,
        prog.main.stats,
        cs.name
      )

      writeClassFile(mainClassFile, outDir, cs.name)
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def writeClassFile(cf: ClassFile, dir: String, className: String) = {
      try {
        cf.writeToFile(dir + className + ".class")
      } catch {
        case e: Exception => fatal(e.getMessage)
      }
    }


    def cGenMethod(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      /*
        Project Extension, add the field to the argument of the method
        declaration and method symbol if it is a value class.
       */
      val argMappings = methSym.classSymbol match {
        case vcs: ValueClassSymbol =>
          (mt.args.zipWithIndex.map { case (arg, index) =>
            arg.id.getSymbol.name -> (index+1)} :+ (vcs.getField.get.name -> (0) )).toMap
        case _ =>
          mt.args.zipWithIndex.map { case (arg, index) =>
          arg.id.getSymbol.name -> (index + 1)}.toMap
      }

      // Maps each variable to one local variable index position
      val variableMappings = mt.vars.map( v => v.getSymbol.name -> ch.getFreshVar).toMap

      val mapping = argMappings ++ variableMappings

      // Generate code for statements
      for(stat <- mt.stats) cGenStat(stat)(ch, mapping, methSym.classSymbol.name)
      // Generate code for the return expression
      cGenExpr(mt.retExpr)(ch, mapping, methSym.classSymbol.name)
      // Return with the correct opcode, based on the type of the return expression
      mt.retExpr.getType match {
        case TInt | TBoolean=> ch << IRETURN
        case _ => ch << ARETURN
      }

      ch.freeze
    }

    // Generates code for the main method
    def cGenMain(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      // Generate code for main method
      for(stmt <- stmts) cGenStat(stmt)(ch, Map.empty, cname)
      ch << RETURN

      ch.freeze
    }


    // Generates code for a statement
    def cGenStat(statement: StatTree)
                (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      statement match {
        case Block(stats) =>
          stats foreach cGenStat

        case If(expr, thn, els) =>
          val afterLabel: String = ch.getFreshLabel("after")

          cGenExpr(expr)

          if(els.isDefined) {
            val elseLabel: String = ch.getFreshLabel("else")

            ch << IfEq(elseLabel)
            cGenStat(thn)
            ch << Goto(afterLabel)
            ch << Label(elseLabel)
            cGenStat(els.get)
            ch << Label(afterLabel)
          }else{
            ch << IfEq(afterLabel)
            cGenStat(thn)
            ch << Label(afterLabel)
          }

        case While(expr, stat) =>
          val whileLabel = ch.getFreshLabel("while")
          val afterLabel = ch.getFreshLabel("after")

          ch << Label(whileLabel)
          cGenExpr(expr)
          ch << IfEq(afterLabel)
          cGenStat(stat)
          ch << Goto(whileLabel)
          ch << Label(afterLabel)

        case Println(expr) =>
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          cGenExpr(expr)
          expr.getType match {
            case TString => ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
            case TInt => ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")
            case TBoolean => ch << InvokeVirtual("java/io/PrintStream", "println", "(Z)V")
            case _ => sys.error(s"Printing wrong type in generation code, match ${expr.getType}");
          }
        case Assign(id, expr) =>
          mapping.get(id.value) match {
            case Some(pos) =>
              cGenExpr(expr)

              findValueClassType(id.getType) match {
                case TInt | TBoolean => ch << IStore(pos)
                case TIntArray | TString | TClass(_) => ch << AStore(pos)
                case _ =>
              }

            case None =>
              id.getType match {
                case TValueClass(_) => sys.error("Value class field not found !")
                case _ =>
                  ch << ALOAD_0
                  cGenExpr(expr)
                  ch << PutField(cname, id.value, typeToDescr(id.getType))
              }
          }

        case ArrayAssign(id, index, expr) =>
          mapping.get(id.value) match {
              //Local variable
            case Some(pos) =>
              ch << ALoad(pos)
              cGenExpr(index)
              cGenExpr(expr)
              ch << IASTORE

              //Field variable
            case None =>
              ch << ALOAD_0
              ch << GetField(cname, id.value, "[I")
              cGenExpr(index)
              cGenExpr(expr)
              ch << IASTORE
          }

        case DoExpr(e) =>
          cGenExpr(e)
          ch << POP //Remove return value

        case _ =>
      }
    }


    // Generates code for an expression
    def cGenExpr(expr: ExprTree)
                (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      expr match {
        case And(lhs,rhs) =>
          ch << ICONST_0
          cGenExpr(lhs)

          val theLabel = ch.getFreshLabel("alreadyFalse")
          ch << IfEq(theLabel)

          // Only care about the right hand side value
          ch << POP
          cGenExpr(rhs)

          ch << Label(theLabel)

        case Or(lhs, rhs) =>
          ch << ICONST_1
          cGenExpr(lhs)
          val theLabel = ch.getFreshLabel("alreadyTrue")
          ch << IfNe(theLabel)

          ch << POP
          cGenExpr(rhs)

          ch << Label(theLabel)

        case Not(e) =>
          val trueCase = ch.getFreshLabel("trueCase")
          val afterLabel = ch.getFreshLabel("falseCase")

          cGenExpr(e)
          ch << IfEq(trueCase)
          ch << ICONST_0
          ch << Goto(afterLabel)
          ch << Label(trueCase)
          ch << ICONST_1
          ch << Label(afterLabel)

        case Plus(lhs, rhs) =>
          (lhs.getType,rhs.getType) match {
            case (TInt, TInt) =>
              cGenExpr(lhs)
              cGenExpr(rhs)
              ch << IADD
            case (TInt, TString) =>
              //Use default constructor to create a string builder
              ch << DefaultNew("java/lang/StringBuilder")
              cGenExpr(lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
              cGenExpr(rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;");

            case (TString, TInt) =>
              //Use default constructor to create a string builder
              ch << DefaultNew("java/lang/StringBuilder")
              cGenExpr(lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              cGenExpr(rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
            case (TString, TString) =>
              //Use default constructor to create a string builder
              ch << DefaultNew("java/lang/StringBuilder")
              cGenExpr(lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              cGenExpr(rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;");

            case _ => sys.error("Error in code generation while adding !")
          }

        case Minus(lhs, rhs) =>
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << ISUB
        case Times(lhs, rhs) =>
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << IMUL
        case Div(lhs, rhs) =>
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << IDIV
        case LessThan(lhs, rhs) =>
          val trueCase = ch.getFreshLabel("trueCase")
          val afterLabel = ch.getFreshLabel("afterLabel")

          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << If_ICmpLt(trueCase)
          ch << ICONST_0
          ch << Goto(afterLabel)
          ch << Label(trueCase)
          ch << ICONST_1
          ch << Label(afterLabel)

        case Equals(lhs, rhs) =>
          val trueCase = ch.getFreshLabel("trueCase")
          val afterLabel = ch.getFreshLabel("afterLabel")

          cGenExpr(lhs)
          cGenExpr(rhs)
          (lhs.getType, rhs.getType) match {
            case (TInt, TInt) | (TBoolean, TBoolean) =>
              ch << If_ICmpEq(trueCase)
              ch << ICONST_0
              ch << Goto(afterLabel)
              ch << Label(trueCase)
              ch << ICONST_1
              ch << Label(afterLabel)
            case (TString, TString) | (TIntArray, TIntArray) | (TClass(_), TClass(_)) =>
              ch << If_ACmpEq(trueCase)
              ch << ICONST_0
              ch << Goto(afterLabel)
              ch << Label(trueCase)
              ch << ICONST_1
              ch << Label(afterLabel)
            case (TValueClass(_), TValueClass(_)) =>
              //TODO
            case _ => sys.error("Error on equals in code generation")
          }
        case ArrayRead(arr, index) =>
          cGenExpr(arr)
          cGenExpr(index)
          ch << IALOAD
        case ArrayLength(arr) =>
          cGenExpr(arr)
          ch << ARRAYLENGTH
        case NewIntArray(size: ExprTree) =>
          cGenExpr(size)
          ch << NewArray.primitive("T_INT")

        case t@This() =>
          t.getType match {
            case TClass(_) => ch << ALOAD_0
            case TValueClass(vcs) =>
              findValueClassType(vcs.getField.get.getType) match {
                case TInt | TBoolean => ILoad(mapping.get(vcs.getField.get.name).get)
                case TIntArray | TString | TClass(_) => ch << ALoad(mapping.get(vcs.getField.get.name).get)
                case _ => sys.error("Cannot find this type")
              }
          }

        case x@MethodCall(obj, meth, args) =>
          obj.getType match {
            case TClass(cs) =>
              //Create return type string of the method
              val returnString = new StringBuilder
              returnString.append("(")
              args.foreach(a => returnString.append(typeToDescr(a.getType)))
              returnString.append(")")
              returnString.append(typeToDescr(x.getType))

              //Start pushing on stack
              cGenExpr(obj)
              args foreach cGenExpr
              ch << InvokeVirtual(cs.name, meth.value, returnString.toString())
            case TValueClass(vcs) =>
              //Create return type string of the method
              val returnString = new StringBuilder
              returnString.append("(")
              returnString.append(typeToDescr(vcs.getField.get.getType)) // Ajoute le type du field en dernier argument
              args.foreach(a => returnString.append(typeToDescr(a.getType)))
              returnString.append(")")
              returnString.append(typeToDescr(x.getType))

              //Start pushing on stack
              args foreach cGenExpr
              cGenExpr(obj)
              ch << InvokeStatic(vcs.name, meth.value, returnString.toString())

            case _ => sys.error("Error on method symbol in code generation")
          }

        case NewValueClass(tpe, e) =>
          cGenExpr(e)

        case New(tpe) =>
          ch << DefaultNew(tpe.value)
        case IntLit(value) =>
          ch << Ldc(value)
        case StringLit(value) =>
          ch << Ldc(value)
        case True() =>
          ch << ICONST_1
        case False() =>
          ch << ICONST_0
        case Variable(id) =>
          mapping.get(id.value) match {
            case Some(pos) =>
              id.getType match {
                case TInt | TBoolean => ch << ILoad(pos)
                case TIntArray | TString | TClass(_) => ch << ALoad(pos)
                case TValueClass(vcs) =>
                  findValueClassType(vcs.getField.get.getType) match {
                    case TInt | TBoolean => ch << ILoad(pos)
                    case TIntArray | TString | TClass(_) => ch << ALoad(pos)
                    case _ =>
                  }
                case _ =>
              }
            case None =>
              id.getType match {
                case TValueClass(_) => sys.error("Value class field not found !")
                case _ =>
                  ch << ALOAD_0
                  ch << GetField(cname, id.value, typeToDescr(id.getType))
              }
          }
        case _ =>
      }

    }

    // Transforms a Tool type to the corresponding JVM type description
    def typeToDescr(t: Type): String = t match {
      case TInt => "I"
      case TIntArray => "[I"
      case TBoolean => "Z"
      case TString => "Ljava/lang/String;"
      case TClass(cs) => "L" + cs.name + ";"
      case TValueClass(vcs) => typeToDescr(vcs.getField.get.getType)
      case _ => sys.error("Error on type matching in code generation")
    }


    def findValueClassType(tpe: Type): Type = {
      tpe match {
        case TValueClass(vcs) => findValueClassType(vcs.getField.get.getType)
        case _ => tpe
      }
    }

        /**** Main code ****/

    // Make output directory
    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")
    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    // Name of source file (to track positions)
    val sourceName = ctx.files.head.getName

    // output class code
    prog.classes foreach {
      generateClassFile(_, sourceName, outDir)
    }

    // output main object code
    generateMainClassFile(prog.main, sourceName, outDir)

  }

}

