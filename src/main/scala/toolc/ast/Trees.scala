package toolc
package ast

import utils._
import analyzer.Symbols._
import analyzer.Types._

object Trees {
  sealed trait Tree extends Positioned

  // Identifiers represent names in Tool. When a unique symbol gets attached to them,
  // they become unique
  case class Identifier(value: String) extends Tree with Symbolic[Symbol] with Typed {
    override def getType: Type = getSymbol match {
      case cs: ClassSymbol =>
        TClass(cs)

      /* Project Extension */
      case vcs: ValueClassSymbol =>
        TValueClass(vcs)

      case ms: MethodSymbol =>
        sys.error("Requesting type of a method identifier.")

      case ms: MainSymbol =>
        sys.error("Requesting type of main object")

      case vs: VariableSymbol =>
        vs.getType
    }
    override def toString = value
  }


  // Definitions
  sealed trait DefTree extends Tree
  /* Project extension */
  sealed trait Class extends DefTree with Symbolic[AbstractClassSymbol] {
    val id: Identifier
    val methods: List[MethodDecl]
    val parent: Option[Identifier]
    val vars: List[VarDecl]
  }

  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
    extends Class

  /* Project extension */
  case class ValueClassDecl(id: Identifier, vars: List[VarDecl], methods: List[MethodDecl])
    extends Class {
    override val parent = None
  }

  case class Program(main: MainObject, classes: List[Class])
  extends DefTree
  case class MainObject(id: Identifier, stats: List[StatTree])
  extends DefTree with Symbolic[MainSymbol]

  case class VarDecl(tpe: TypeTree, id: Identifier)
    extends DefTree with Symbolic[VariableSymbol]
  case class MethodDecl(id: Identifier,
                        args: List[Formal],
                        retType: TypeTree,
                        vars: List[VarDecl],
                        stats: List[StatTree],
                        retExpr: ExprTree)
    extends DefTree with Symbolic[MethodSymbol]
  case class Formal(tpe: TypeTree, id: Identifier)
    extends DefTree with Symbolic[VariableSymbol]

  // Types
  sealed trait TypeTree extends Tree with Typed
  case class IntArrayType() extends TypeTree {
    override def getType = TIntArray
  }
  case class IntType() extends TypeTree {
    override def getType = TInt
  }
  case class BooleanType() extends TypeTree {
    override def getType = TBoolean
  }
  case class StringType() extends TypeTree {
    override def getType = TString
  }
  case class ClassType(id: Identifier) extends TypeTree {
    override def getType = id.getType
  }

  // Statements
  sealed trait StatTree extends Tree
  case class Block(stats: List[StatTree]) extends StatTree
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(expr: ExprTree, stat: StatTree) extends StatTree
  case class Println(expr: ExprTree) extends StatTree
  case class Assign(id: Identifier, expr: ExprTree) extends StatTree
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) extends StatTree
  case class DoExpr(e: ExprTree) extends StatTree

  // Expressions
  sealed trait ExprTree extends Tree with Typed

  // Boolean operators
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    val getType = TBoolean
  }
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    val getType = TBoolean
  }
  case class Not(expr: ExprTree) extends ExprTree {
    val getType = TBoolean
  }
  // Arithmetic operators (Plus works on any combination of Int/String)
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    def getType = (lhs.getType,rhs.getType) match {
      case (TInt,TInt) => TInt
      case (TString, TString) => TString
      case (TInt, TString) => TString
      case (TString, TInt) => TString
      case _ => TError
    }
  }
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    val getType = TInt
  }
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    val getType = TInt
  }
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    val getType = TInt
  }
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    val getType = TBoolean
  }
  // Equality
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    val getType = TBoolean
  }
  // Array expressions
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree {
    val getType = TInt
  }
  case class ArrayLength(arr: ExprTree) extends ExprTree {
    val getType = TInt
  }
  case class NewIntArray(size: ExprTree) extends ExprTree {
    val getType = TIntArray
  }
  // Object-oriented expressions
  case class This() extends ExprTree with Symbolic[AbstractClassSymbol] {
    def getType = getSymbol.getType
  }
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree {
    override def getType = {
      obj.getType match {
        case TClass(cs) =>
      }
    }
//    def getType = {
//      obj.getType match {
//        case TClass(cs) => getTypeHelper(cs)
//        case TValueClass(vcs) => getTypeHelper(vcs)
//        case _ => TError
//      }
//    }
//
//    def getTypeHelper(acs: AbstractClassSymbol) = {
//      acs.lookupMethod(meth.value) match {
//        case Some(ms) =>
//          if (args.size != ms.argList.size) TError
//          else {
//            val listTupleArgs = args.map(arg => arg.getType).zip(ms.argList.map(x => x.getType))
//            val matchArgs = listTupleArgs.forall(tpe => tpe._1.isSubTypeOf(tpe._2))
//            if (matchArgs) ms.getType else TError
//          }
//        case None => TError
//      }
//    }
  }

  case class New(tpe: Identifier) extends ExprTree {
    def getType = tpe.getType match {
      case t@TClass(_) => t
      case other => TError
    }
  }

  /* Project Extension */
  case class NewValueClass(tpe: Identifier, expr: ExprTree) extends ExprTree{
    def getType = tpe.getType match {
      case t@TValueClass(_) => t
      case other => TError
    }
  }

  // Literals
  case class IntLit(value: Int) extends ExprTree {
    val getType = TInt
  }
  case class StringLit(value: String) extends ExprTree {
    val getType = TString
  }
  case class True() extends ExprTree {
    val getType = TBoolean
  }
  case class False() extends ExprTree {
    val getType = TBoolean
  }
  // Variables
  case class Variable(id: Identifier) extends ExprTree {
    def getType = id.getType
  }

}
