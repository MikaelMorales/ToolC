package toolc
package ast

import ast.Trees._
import lexer.Token
import lexer.Tokens._
import grammarcomp.parsing._

class ASTConstructorLL1 extends ASTConstructor {

  //Override the construction of the three for Type
  override def constructType(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('Type ::= List(INT(), 'TypeSeq), List(Leaf(i @ INT()), typeseq)) =>
        constructTypeSeq(typeseq, i)
      case Node('Type ::= _, List(Leaf(b @ BOOLEAN()))) =>
        BooleanType().setPos(b)
      case Node('Type ::= _, List(Leaf(s @ STRING()))) =>
        StringType().setPos(s)
      case Node('Type ::= List('Identifier), List(id)) =>
        val pid = constructId(id)
        ClassType(pid).setPos(pid)
    }
  }

  def constructTypeSeq(ptree: NodeOrLeaf[Token], i: Token): TypeTree = {
    ptree match {
      case Node('TypeSeq ::= List(LBRACKET(), RBRACKET()), List(_, _)) =>
        IntArrayType().setPos(i)
      case Node('TypeSeq ::= _, List()) =>
        IntType().setPos(i)
    }
  }

  //Override the construction of an expression, the methods above are chained together
  //It matches what was written in the ll1Grammar inside Parser.scala
  override def constructExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('Expression ::= _, List(orexpr)) =>
        constructOrExpr(orexpr)
    }
  }

  def constructOrExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('OrExpr ::= List('AndExpr, 'OrExprSeq), List(and, orSeq)) =>
        var lhs = constructAndExpr(and)
        constructExprLeftAssociative(lhs, orSeq).setPos(lhs)
    }
  }

  def constructAndExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('AndExpr ::= List('CompExpr, 'AndExprSeq), List(comp, andSeq)) =>
        var lhs = constructCompExpr(comp)
        constructExprLeftAssociative(lhs, andSeq).setPos(lhs)
    }
  }

  def constructCompExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('CompExpr ::= List('SumExpr, 'CompExprSeq), List(sum, compSeq)) =>
        var lhs = constructSumExpr(sum)
        constructExprLeftAssociative(lhs, compSeq).setPos(lhs)
    }
  }

  def constructSumExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('SumExpr ::= List('TimesDivExpr, 'SumExprSeq), List(timesdiv, sumSeq)) =>
        var lhs = constructTimesDivExpr(timesdiv)
        constructExprLeftAssociative(lhs, sumSeq).setPos(lhs)
    }
  }

  def constructTimesDivExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('TimesDivExpr ::= List('BangExpr, 'TimesDivExprSeq), List(bang, timesdivSeq)) =>
        var lhs = constructBangExpr(bang)
        constructExprLeftAssociative(lhs, timesdivSeq).setPos(lhs)
    }
  }

  def constructBangExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('BangExpr ::= List(BANG(), 'BangExpr), List(_, bangexpr)) =>
        var exprTree = constructBangExpr(bangexpr)
        Not(exprTree).setPos(exprTree)
      case Node('BangExpr ::= List('BracketExpr), List(bracketExpr)) =>
        constructBracketExpr(bracketExpr)
    }
  }

  def constructBracketExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('BracketExpr ::= List('NewExpr, 'BracketSeq), List(newexpr, bracketSeq)) =>
        var array = constructNewExpr(newexpr)
        constructBracketSeq(array, bracketSeq)
    }
  }

  def constructBracketSeq(arr: ExprTree, ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('BracketSeq ::= List(LBRACKET(), 'Expression, RBRACKET()), List(_, expr, _)) =>
        var expression = constructExpr(expr)
        ArrayRead(arr, expression).setPos(arr)
      case Node('BracketSeq ::= List('DotExpr), List(dotexpr)) =>
        constructDotExpr(arr, dotexpr)
    }
  }

  def constructDotExpr(arr: ExprTree, ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('DotExpr ::= List(DOT(), 'DotSeq), List(_, dotSeq)) =>
        dotSeq match {
          case Node('DotSeq ::= List(LENGTH()), _) =>
            ArrayLength(arr).setPos(arr)
          case Node('DotSeq ::= List('Identifier, LPAREN(), 'Args, RPAREN(), 'BracketSeq), List(id, _, args, _, bracketSeq)) =>
            var newArray = MethodCall(arr,
              constructId(id),
              constructList(args, constructExpr, hasComma = true)).setPos(arr)
            constructBracketSeq(newArray, bracketSeq)
        }
      case _ => arr.setPos(arr)
    }
  }

  def constructNewExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('NewExpr ::= List(NEW(), 'NewSeq), List(Leaf(nt), newSeq)) =>
        newSeq match {
          case Node('NewSeq ::= List(INT(), _, 'Expression, _), List(_, _, expr, _)) =>
            NewIntArray(constructExpr(expr)).setPos(nt)
          /* Project Extension */
          case Node('NewSeq ::= List('Identifier, _, 'NewClassSeq), List(id, _, newClassSeq)) =>
            constructNewClass(nt, id, newClassSeq)
        }
      case Node('NewExpr ::= List('Factor), List(factor)) =>
        constructFactor(factor)
    }
  }

  /* Project Extension */
  def constructNewClass(newToken: Token, id: NodeOrLeaf[Token], ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('NewClassSeq ::= _, _) =>
        New(constructId(id)).setPos(newToken)
      case Node('NewClassSeq ::= List('Expression, _), List(expr, _)) =>
        NewValueClass(constructId(id), constructExpr(expr)).setPos(newToken)
    }
  }

  def constructFactor(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('Factor ::= _, List(Leaf(pos @ INTLIT(i)))) =>
        IntLit(i).setPos(pos)
      case Node('Factor ::= _, List(Leaf(pos @ STRINGLIT(s)))) =>
        StringLit(s).setPos(pos)
      case Node('Factor ::= _, List(Leaf(pos @ TRUE()))) =>
        True().setPos(pos)
      case Node('Factor ::= _, List(Leaf(pos @ FALSE()))) =>
        False().setPos(pos)
      case Node('Factor ::= _, List(Leaf(pos @ THIS()))) =>
        This().setPos(pos)
      case Node('Factor ::= List('Identifier), List(id)) =>
        Variable(constructId(id))
      case Node('Factor ::= List(_, 'Expression, _), List(_, expr, _)) =>
        constructExpr(expr)
    }
  }

  //Make sure that when we have several times the same operation, we have left associativity, since
  //it can't be tested in the ll1Grammar
  def constructExprLeftAssociative(lhs: ExprTree, ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('OrExprSeq ::= List(OR(), 'OrExpr), List(_, orexpr)) =>
        orexpr match {
          case Node('OrExpr ::= List('AndExpr, 'OrExprSeq), List(andexpr, orSeq)) =>
            var left = constructAndExpr(andexpr)
            constructExprLeftAssociative(Or(lhs, left).setPos(lhs), orSeq)
        }

      case Node('AndExprSeq ::= List(AND(), 'AndExpr), List(_, andexpr)) =>
        andexpr match {
          case Node('AndExpr ::= List('CompExpr, 'AndExprSeq), List(compexpr, andSeq)) =>
            var left = constructCompExpr(compexpr)
            constructExprLeftAssociative(And(lhs, left).setPos(lhs), andSeq)
        }

      case Node('CompExprSeq ::= List(EQUALS(), 'CompExpr), List(_, compexpr)) =>
        compexpr match {
          case Node('CompExpr ::= List('SumExpr, 'CompExprSeq), List(sumexpr, compSeq)) =>
            var left = constructSumExpr(sumexpr)
            constructExprLeftAssociative(Equals(lhs, left).setPos(lhs), compSeq)
        }
      case Node('CompExprSeq ::= List(LESSTHAN(), 'CompExpr), List(_, compexpr)) =>
        compexpr match {
          case Node('CompExpr ::= List('SumExpr, 'CompExprSeq), List(sumexpr, compSeq)) =>
            var left = constructSumExpr(sumexpr)
            constructExprLeftAssociative(LessThan(lhs, left).setPos(lhs), compSeq)
        }

      case Node('SumExprSeq ::= List(MINUS(), 'SumExpr), List(_, sumexpr)) =>
        sumexpr match {
          case Node('SumExpr ::= List('TimesDivExpr, 'SumExprSeq), List(timesdiv, sumSeq)) =>
            var left = constructTimesDivExpr(timesdiv)
            constructExprLeftAssociative(Minus(lhs, left).setPos(lhs), sumSeq)
        }
      case Node('SumExprSeq ::= List(PLUS(), 'SumExpr), List(_, sumexpr)) =>
        sumexpr match {
          case Node('SumExpr ::= List('TimesDivExpr, 'SumExprSeq), List(timesdiv, sumSeq)) =>
            var left = constructTimesDivExpr(timesdiv)
            constructExprLeftAssociative(Plus(lhs, left).setPos(lhs), sumSeq)
        }

      case Node('TimesDivExprSeq ::= List(DIV(), 'TimesDivExpr), List(_, timesdivexpr)) =>
        timesdivexpr match {
          case Node('TimesDivExpr ::= List('BangExpr, 'TimesDivExprSeq), List(bangexpr, timesdivSeq)) =>
            var left = constructBangExpr(bangexpr)
            constructExprLeftAssociative(Div(lhs, left).setPos(lhs), timesdivSeq)
        }
      case Node('TimesDivExprSeq ::= List(TIMES(), 'TimesDivExpr), List(_, timesdivexpr)) =>
        timesdivexpr match {
          case Node('TimesDivExpr ::= List('BangExpr, 'TimesDivExprSeq), List(bangexpr, timesdivSeq)) =>
            var left = constructBangExpr(bangexpr)
            constructExprLeftAssociative(Times(lhs, left).setPos(lhs), timesdivSeq)
        }
      case _ => lhs.setPos(lhs)
    }
  }

}
