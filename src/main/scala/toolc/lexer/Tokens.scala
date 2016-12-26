package toolc
package lexer

import grammarcomp.grammar.CFGrammar.TerminalClass
import utils._

sealed class Token extends Positioned

object Tokens {
  /* Basic Tokens */
  case class BAD()        extends Token // represents incorrect tokens.
  case class EOF()        extends Token // Signifies end-of-file
  case class COLON()      extends Token // :
  case class SEMICOLON()  extends Token // ;
  case class DOT()        extends Token // .
  case class COMMA()      extends Token // ,
  case class EQSIGN()     extends Token // =
  case class EQUALS()     extends Token // ==
  case class BANG()       extends Token // !
  case class LPAREN()     extends Token // (
  case class RPAREN()     extends Token // )
  case class LBRACKET()   extends Token // [
  case class RBRACKET()   extends Token // ]
  case class LBRACE()     extends Token // {
  case class RBRACE()     extends Token // }
  case class AND()        extends Token // &&
  case class OR()         extends Token // ||
  case class LESSTHAN()   extends Token // <
  case class PLUS()       extends Token // +
  case class MINUS()      extends Token // -
  case class TIMES()      extends Token // *
  case class DIV()        extends Token // /
  case class PROGRAM()    extends Token // program
  case class CLASS()      extends Token // class
  case class DEF()        extends Token // def
  case class VAR()        extends Token // var
  case class STRING()     extends Token // string
  case class EXTENDS()    extends Token // extends
  case class INT()        extends Token // int
  case class BOOLEAN()    extends Token // boolean
  case class WHILE()      extends Token // while
  case class IF()         extends Token // if
  case class ELSE()       extends Token // else
  case class RETURN()     extends Token // return
  case class LENGTH()     extends Token // length
  case class TRUE()       extends Token // true
  case class FALSE()      extends Token // false
  case class THIS()       extends Token // this
  case class NEW()        extends Token // new
  case class PRINTLN()    extends Token // println
  case class DO()         extends Token // do(expr)
  /* Extension of the project */
  case class VALUE()      extends Token // value

  // Identifiers
  case class ID(val value: String) extends Token with TerminalClass

  // Integer literals
  case class INTLIT(val value: Int) extends Token with TerminalClass

  // String literals
  case class STRINGLIT(val value: String) extends Token with TerminalClass

  val IDSENT = ID("")
  val INTLITSENT = INTLIT(0)
  val STRINGLITSENT = STRINGLIT("")  
}
