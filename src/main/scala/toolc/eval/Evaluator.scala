package toolc
package eval

import ast.Trees._
import toolc.analyzer.Symbols.ValueClassSymbol
import utils._

import scala.annotation.tailrec

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    val ectx = new MainContext
    prog.main.stats.foreach(evalStatement(_)(ectx))
  }

  def evalStatement(stmt: StatTree)(implicit ectx: EvaluationContext): Unit = stmt match {
    case Block(stats) => stats.foreach(stat => evalStatement(stat))
    case If(expr, thn, els) =>
      if(evalExpr(expr).asBool) {
        evalStatement(thn)
      }else{
        els match {
            case Some(stat) => evalStatement(stat)
            case None =>
        }
      }

    case While(expr, stat) =>
      while(evalExpr(expr).asBool) {
        evalStatement(stat)
      }

    case Println(expr) =>
      val exp = evalExpr(expr)
      exp match {
          case IntValue(i1) => println(i1)
          case BoolValue(b1) => println(b1)
          case StringValue(s1) => println(s1)
          case _ => fatal(s"This type cannot be printed !")
      }

    case Assign(id, expr) => ectx.setVariable(id.value, evalExpr(expr))
    case ArrayAssign(id, index, expr) =>
      ectx.getVariable(id.value).asArray.setIndex(evalExpr(index).asInt, evalExpr(expr).asInt)
    case DoExpr(expr) => evalExpr(expr)
  }

  def evalExpr(e: ExprTree)(implicit ectx: EvaluationContext): Value = e match {
    case IntLit(value) => IntValue(value)
    case StringLit(value) => StringValue(value)
    case True() => BoolValue(true)
    case False() => BoolValue(false)
    case And(lhs, rhs) => BoolValue(evalExpr(lhs).asBool && evalExpr(rhs).asBool)
    case Or(lhs, rhs) => BoolValue(evalExpr(lhs).asBool || evalExpr(rhs).asBool)
    case Plus(lhs, rhs) =>
      val l = evalExpr(lhs)
      val r = evalExpr(rhs)
      (l, r) match {
        case (IntValue(i1), IntValue(i2)) => IntValue(i1 + i2)
        case (IntValue(i1), StringValue(s2)) => StringValue(Integer.toString(i1) + s2)
        case (StringValue(s1), IntValue(i2)) => StringValue(s1 + Integer.toString(i2))
        case (StringValue(s1), StringValue(s2)) => StringValue(s1 + s2);
        case _ => fatal(s"You cannot add this types together !")
      }

    case Minus(lhs, rhs) => IntValue(evalExpr(lhs).asInt - evalExpr(rhs).asInt)
    case Times(lhs, rhs) => IntValue(evalExpr(lhs).asInt * evalExpr(rhs).asInt)
    case Div(lhs, rhs) => IntValue(evalExpr(lhs).asInt / evalExpr(rhs).asInt)
    case LessThan(lhs, rhs) => BoolValue(evalExpr(lhs).asInt < evalExpr(rhs).asInt)
    case Not(expr) => BoolValue(! evalExpr(expr).asBool)
    case Equals(lhs, rhs) =>
      val l = evalExpr(lhs)
      val r = evalExpr(rhs)
      (l, r) match {
        case (IntValue(i1), IntValue(i2)) => BoolValue(i1 == i2)
        case (BoolValue(i1), BoolValue(i2)) => BoolValue(i1 == i2)
        case (_: StringValue, _: StringValue) => BoolValue(l == r)
        case (_: ArrayValue, _: ArrayValue) => BoolValue(l eq r)
        case (ObjectValue(_: ClassDecl), ObjectValue(_: ClassDecl)) => BoolValue(l eq r)
        case (o1@ObjectValue(vcl1: ValueClassDecl), o2@ObjectValue(vcl2: ValueClassDecl)) =>
          (vcl1.getSymbol, vcl2.getSymbol) match {
            case (cs1: ValueClassSymbol, cs2: ValueClassSymbol) => valueClassEquals(o1, cs1, o2, cs2)
            case _ => fatal("A value class doesn't have a value class symbol")
          }
        case _ => fatal(s"Can't compare these two objects")
      }

    case ArrayRead(arr, index) =>
      val array = evalExpr(arr).asArray
      val i = evalExpr(index).asInt
      IntValue(array.getIndex(i))

    case ArrayLength(arr) => IntValue(evalExpr(arr).asArray.length)
    case MethodCall(obj, meth, args) =>
      val objct = evalExpr(obj).asObject
      val method = findMethod(objct.cd, meth.value)
      val newctx = new MethodContext(objct)

      if(method.args.size != args.size) fatal(s"The number of arguments is incorrect !")

      //Déclare les arguments et les initialise
      val tupples = method.args.zip(args)
      method.args.foreach(arg => newctx.declareVariable(arg.id.value))
      tupples.foreach(t => newctx.setVariable(t._1.id.value, evalExpr(t._2)))
      //Déclare les variables locales à la méthode (Sans valeur)
      method.vars.foreach(v => newctx.declareVariable(v.id.value))
      //Evalue les expressions
      method.stats.foreach(stat => evalStatement(stat)(newctx))
      //Evalue le return
      evalExpr(method.retExpr)(newctx)

    case Variable(Identifier(name)) => ectx.getVariable(name)
    case New(tpe) =>
      //Trouve la classe
      val cl = findClass(tpe.value)
      //Crée object associé à la classe
      val objct = ObjectValue(cl)
      //Instancie les attributs de l'objet si il y en a
      fieldsOfClass(cl).foreach(att => objct.declareField(att))
      objct

    case NewValueClass(tpe, fieldExpr) =>
      //Trouve la classe
      val cl = findClass(tpe.value)
      //Crée object associé à la classe
      val objct = ObjectValue(cl)
      //Instancie les attributs de l'objet si il y en a
      fieldsOfClass(cl).foreach(att => objct.declareField(att))
      //Set field of the value class
      objct.setField(cl.vars.head.id.value, evalExpr(fieldExpr))
      objct

    case This() =>
      ectx match {
        case ctx: MethodContext => ctx.obj
        case _ =>  fatal(s"Cannot call This outside a MethodContext")
      }

    case NewIntArray(size) =>
      val sizeArray = evalExpr(size).asInt
      ArrayValue(new Array[Int](sizeArray))
  }

  /*
    This method compare two value class equality by comparing the name of the class and then the value of
    the unique field of a value class.
   */
  @tailrec
  final def valueClassEquals(objVal1: ObjectValue, vcs1: ValueClassSymbol, objVal2: ObjectValue, vcs2: ValueClassSymbol): BoolValue = {
    (objVal1.getField(vcs1.fieldId), objVal2.getField(vcs2.fieldId)) match {
      case (IntValue(i1), IntValue(i2)) => BoolValue((vcs1.name == vcs2.name) && i1 == i2)
      case (BoolValue(b1), BoolValue(b2)) => BoolValue((vcs1.name == vcs2.name) && b1 == b2)
      case (StringValue(s1), StringValue(s2)) => BoolValue((vcs1.name == vcs2.name) && (s1 == s2))
      case (ArrayValue(a1), ArrayValue(a2)) => BoolValue((vcs1.name == vcs2.name) && (a1 eq a2))
      case (obj1@ObjectValue(_: ClassDecl), obj2@ObjectValue(_: ClassDecl)) => BoolValue((vcs1.name == vcs2.name) && (obj1 eq obj2))
      case (obj1@ObjectValue(vcl1: ValueClassDecl), obj2@ObjectValue(vcl2: ValueClassDecl)) =>
        (vcl1.getSymbol, vcl2.getSymbol) match {
          case (cs1: ValueClassSymbol, cs2: ValueClassSymbol) => valueClassEquals(obj1, cs1, obj2, cs2)
          case _ => fatal(s"Value class ${vcs1.name} or ${vcs2.name} doesn't have a value class symbol")
        }
      case _ => BoolValue(false)
    }
  }

  abstract class EvaluationContext {
    def getVariable(name: String): Value
    def setVariable(name: String, v: Value): Unit
    def declareVariable(name: String): Unit
  }

  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '"+name+"'"))
        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) = {
      if (vars contains name) vars += name -> Some(v) else obj.setField(name, v)
    }

    def declareVariable(name: String) = vars += name -> None
  }

  class MainContext extends EvaluationContext {
    private def unavailable = fatal("The main object contains no variables and/or fields")
    def getVariable(name: String): Value          = unavailable
    def setVariable(name: String, v: Value): Unit = unavailable
    def declareVariable(name: String): Unit       = unavailable
  }

  def findMethod(cd: Class, name: String): MethodDecl = {
    cd.methods.find(_.id.value == name).orElse(
      cd.parent.map(p => findMethod(findClass(p.value), name))
    ).getOrElse(fatal("Unknown method "+cd.id+"."+name))
  }

  def findClass(name: String): Class = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '"+name+"'"))
  }

  def fieldsOfClass(cl: Class): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }

  sealed abstract class Value {
    private def expected(tp: String) = fatal(s"Unexpected value: found $this, expected $tp")

    def asInt: Int            = expected("Int")
    def asString: String      = expected("String")
    def asBool: Boolean       = expected("Boolean")
    def asObject: ObjectValue = expected("Object")
    def asArray: ArrayValue   = expected("Array")
  }

  case class ObjectValue(cd: Class) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) = {
      if (fields contains name) fields += name -> Some(v) else fatal(s"Unknown field '$name'")
    }

    def getField(name: String) = {
      fields.get(name) match {
        case Some(Some(v)) => v
        case Some(None) => fatal(s"Field '$name' has not been initialized")
        case None => fatal(s"Unknown field '$name'")
      }
    }

    def declareField(name: String) = fields += name -> None

    override def asObject = this
  }

  case class ArrayValue(entries: Array[Int]) extends Value {
    val length = entries.length

    private def checkBounds(index: Int) = {
      if (index < 0 || index >= length) {
        fatal(s"Index '$index' out of bounds (0 .. ${length-1})")
      }
    }

    def setIndex(i: Int, v: Int) {
      checkBounds(i)
      entries(i) = v
    }

    def getIndex(i: Int) = {
      checkBounds(i)
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(v: String) extends Value {
    override def asString = v
  }

  case class IntValue(v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(v: Boolean) extends Value {
    override def asBool = v
  }
}
