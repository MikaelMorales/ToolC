package toolc
package ast

import Trees._

object Printer {

  abstract class Document {

    def <:>(other: Document) = Lined(List(this, other))

    def print: String = {
      val sb = new StringBuffer()

      def rec(d: Document)(implicit ind: Int): Unit = d match {
        case Raw(s) =>
          sb append ("  " * ind)
          sb append s
        case Indented(doc) =>
          rec(doc)(ind + 1)
        case Lined(Nil, _) => // skip
        case Lined(docs, sep) =>
          sb append ("  " * ind)
          // Hack: We don't want to reprint the indentation, so we pass 0
          docs.init foreach { d =>
            rec(d)(0)
            rec(sep)(0)
          }
          rec(docs.last)(0)
        case Stacked(Nil) => // skip
        case Stacked(docs) =>
          docs.init foreach { d =>
            rec(d)
            sb append "\n"
          }
          rec(docs.last)
      }

      rec(this)(0)
      sb.toString
    }
  }
  case class Indented(content: Document) extends Document
  case class Stacked(docs: List[Document]) extends Document
  case class Lined(docs: List[Document], separator: Document = Raw("")) extends Document
  case class Raw(s: String) extends Document

  object Stacked {
    def apply(docs: Document*): Stacked = Stacked(docs.toList)
  }

  implicit def stringToDoc(s: String): Raw = Raw(s)

  def apply(t: Tree, printUniqueIDs: Boolean = false) = {
    def binOp(e1: ExprTree, op: String, e2: ExprTree) = "(" <:> rec(e1) <:> " " + op + " " <:> rec(e2) <:> ")"

    def rec(t: Tree): Document = t match {
      case id@Identifier(value) =>
        val tail = {
          if (!printUniqueIDs) ""
          else id.optSymbol match {
            case None => "#??"
            case Some(sym) => "#" + sym.id
          }
        }
        value + tail
      case Program(main, classes) =>
        Stacked(rec(main) :: (classes map rec))
      case MainObject(id, stats) =>
        Stacked(
          "program " <:> rec(id) <:> " {",
          Indented(Stacked(stats map rec)),
          "}",
          ""
        )
      case ClassDecl(id, parent, vars, methods) =>
        val optP: Document = parent map { " extends " <:> rec(_) } getOrElse ""
        Stacked(
          "class " <:> rec(id) <:> optP <:> " {",
          Indented(Stacked(
            (vars map rec) ++ (Raw("") :: (methods map rec))
          )),
          "}",
          ""
        )
      case ValueClassDecl(id, vars, methods) =>
        Stacked(
          "@value class " <:> rec(id) <:> " {",
          Indented(Stacked(
            (vars map rec) ++ (Raw("") :: (methods map rec))
          )),
          "}",
          ""
        )

      case VarDecl(tpe, id) =>
        s"var " <:> rec(id) <:> ": " <:> rec(tpe) <:> ";"
      case MethodDecl(id, args, retType, vars, stats, retExpr) =>
        Stacked(
          "def " <:> rec(id) <:> "(" <:> Lined(args map rec, ", ") <:> "): " <:> rec(retType) <:> " = {",
          Indented(Stacked(
            (vars map rec) ++
            (stats map rec) :+
            ("return " <:> rec(retExpr) <:> ";")
          )),
          "}",
          ""
        )
      case Formal(tpe, id) =>
        rec(id) <:> ": " <:> rec(tpe)

      case IntArrayType() => "Int[]"
      case IntType() => "Int"
      case BooleanType() => "Bool"
      case StringType() => "String"
      case ClassType(id) => rec(id)

      case Block(stats) =>
        Stacked(
          "{",
          Indented(Stacked(stats map rec)),
          "}"
        )
      case If(expr, thn, Some(els)) =>
        Stacked(
          "if (" <:> rec(expr) <:> ")",
          Indented(rec(thn)),
          "else ",
          Indented(rec(els))
        )
      case If(expr, thn, None) =>
        Stacked(
          "if (" <:> rec(expr) <:> ")",
          Indented(rec(thn))
        )
      case While(expr, stat) =>
        Stacked(
          "while (" <:> rec(expr) <:> ")",
          Indented(rec(stat))
        )
      case Println(expr) =>
        "println(" <:> rec(expr) <:> ");"
      case Assign(id, expr) =>
        rec(id) <:> " = " <:> rec(expr) <:> ";"
      case ArrayAssign(id, index, expr) =>
        rec(id) <:> "[" <:> rec(index) <:> "] = " <:> rec(expr) <:> ";"
      case DoExpr(e) =>
        "do(" <:> rec(e) <:> ");"

      case And(lhs, rhs) =>
        binOp(lhs, "&&", rhs)
      case Or(lhs, rhs) =>
        binOp(lhs, "||", rhs)
      case Plus(lhs, rhs) =>
        binOp(lhs, "+", rhs)
      case Minus(lhs, rhs) =>
        binOp(lhs, "-", rhs)
      case Times(lhs, rhs) =>
        binOp(lhs, "*", rhs)
      case Div(lhs, rhs) =>
        binOp(lhs, "/", rhs)
      case LessThan(lhs, rhs) =>
        binOp(lhs, "<", rhs)
      case Equals(lhs, rhs) =>
        binOp(lhs, "==", rhs)
      case ArrayRead(arr, index) =>
        rec(arr) <:> "[" <:> rec(index) <:> "]"
      case ArrayLength(arr) =>
        rec(arr) <:> ".length"
      case MethodCall(obj, meth, args: List[ExprTree]) =>
        rec(obj) <:> "." <:> rec(meth) <:> "(" <:> Lined(args map rec, ", ") <:> ")"
      case IntLit(value) =>
        value.toString
      case StringLit(value) =>
        '"' + value + '"'
      case Variable(id) =>
        rec(id)
      case True() =>
        "true"
      case False() =>
        "false"
      case This() =>
        "this"
      case NewIntArray(size) =>
        "(new Int[" <:> rec(size) <:> "])"
      case New(tpe) =>
        "(new " <:> rec(tpe) <:> "())"

      /* Project Extension */
      case NewValueClass(tpe, expr) =>
        "(new " <:> rec(tpe) <:> "(" <:> rec(expr) <:> "))"

      case Not(expr) =>
        "(!(" <:> rec(expr) <:> "))"
    }

    rec(t).print
  }

}
