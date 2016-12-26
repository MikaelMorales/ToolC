//package toolc
//package analyzer
//
//import ast.Trees._
//
//import Types._
//import utils._
//
//object TypeChecking extends Pipeline[Program, Program] {
//
//  /** Typechecking does not produce a value, but has the side effect of
//   * attaching types to trees and potentially outputting error messages. */
//  def run(ctx: Context)(prog: Program): Program = {
//    import ctx.reporter._
//
//    def tcClass(klass: ClassDecl): Unit = klass.methods.foreach(tcMethod)
//
//    /** Type checks statements and return expression of the method */
//    def tcMethod(meth: MethodDecl): Unit = {
//      tcExpr(meth.retExpr, meth.retType.getType)
//      meth.stats.foreach(stat => tcStat(stat))
//    }
//
//    /** Checks that the expression is a subtype of the ones in expectedTps.
//      * If it's not, prints an error message and returns the error type.
//      * Also adds missing symbols to methods in MethodCalls
//      */
//    def tcExpr(expr: ExprTree, expectedTps: Type*): Unit = {
//      expr match {
//        case And(lhs, rhs) =>
//          tcExpr(lhs, TBoolean)
//          tcExpr(rhs, TBoolean)
//        case Or(lhs, rhs) =>
//          tcExpr(lhs, TBoolean)
//          tcExpr(rhs, TBoolean)
//        case Not(expr) =>
//          tcExpr(expr, TBoolean)
//        case Plus(lhs, rhs) =>
//          tcExpr(lhs, TInt, TString)
//          tcExpr(rhs, TInt, TString)
//        case Minus(lhs, rhs) =>
//          tcExpr(lhs, TInt)
//          tcExpr(rhs, TInt)
//        case Times(lhs, rhs) =>
//          tcExpr(lhs, TInt)
//          tcExpr(rhs, TInt)
//        case Div(lhs, rhs) =>
//          tcExpr(lhs, TInt)
//          tcExpr(rhs, TInt)
//        case LessThan(lhs, rhs) =>
//          tcExpr(lhs, TInt)
//          tcExpr(rhs, TInt)
//        case Equals(lhs, rhs) =>
//          if(lhs.getType.isSubTypeOf(TObject)) {
//            tcExpr(lhs, TObject)
//            tcExpr(rhs, TObject)
//          }else {
//            tcExpr(lhs, TInt, TString, TBoolean, TIntArray)
//            tcExpr(rhs, lhs.getType)
//          }
//        case ArrayRead(arr, index) =>
//          tcExpr(arr, TIntArray)
//          tcExpr(index, TInt)
//        case ArrayLength(arr) =>
//          tcExpr(arr, TIntArray)
//        case NewIntArray(size) =>
//          tcExpr(size, TInt)
//        case MethodCall(obj, meth, args) =>
//          obj.getType match {
//            case TClass(classSymbol) =>
//              classSymbol.lookupMethod(meth.value) match {
//                case Some(ms) =>
//                  meth.setSymbol(ms)
//                  meth.getSymbol.setType(ms.getType)
//                case None =>
//              }
//            case _ => ctx.reporter.error("A method is call with a non class object")
//          }
//
//        case _ =>
//      }
//
//      if (!expectedTps.toList.exists(expr.getType.isSubTypeOf)) {
//        error("Type error: Expected: " + expectedTps.mkString(" or ") + s", found: ${expr.getType}", expr)
//      }
//
//    }
//
//    /** Invokes tcExpr as needed in the expressions of stat */
//    def tcStat(stat: StatTree): Unit = {
//      stat match {
//        case Block(stats: List[StatTree]) =>
//          stats.foreach(tcStat(_))
//        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
//          tcExpr(expr, TBoolean)
//          tcStat(thn)
//          els foreach tcStat // Because foreach treats directly the Some case and ignore the None case
//        case While(expr: ExprTree, stat: StatTree) =>
//          tcExpr(expr, TBoolean)
//          tcStat(stat)
//        case Println(expr: ExprTree) =>
//          tcExpr(expr, TInt, TString, TBoolean)
//        case Assign(id: Identifier, expr: ExprTree) =>
//          tcExpr(expr, id.getType)
//        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
//          tcExpr(Variable(id), TIntArray)
//          tcExpr(index, TInt)
//          tcExpr(expr, TInt)
//        case DoExpr(expr: ExprTree) =>
//          tcExpr(expr, TInt, TString, TBoolean, TIntArray, TObject)
//      }
//    }
//
//    prog.main.stats.foreach(tcStat)
//    prog.classes.foreach(tcClass)
//
//    prog
//  }
//}
