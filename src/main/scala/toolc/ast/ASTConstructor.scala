package toolc
package ast

import ast.Trees._
import lexer.Token
import lexer.Tokens._
import grammarcomp.parsing._

class ASTConstructor {

  def constructProgram(ptree: NodeOrLeaf[Token]): Program = {
    ptree match {
      case Node('Program ::= _, List(mainObj, classDefs, eof)) =>
        val mo = constructMain(mainObj)
        Program(
          mo,
          constructList(classDefs, constructClass)
        ).setPos(mo)
    }
  }
  def constructMain(ptree: NodeOrLeaf[Token]): MainObject = {
    ptree match {
      case Node('MainObject ::= _, List(Leaf(prog), objid, _, stmts, _)) =>
        MainObject(constructId(objid), constructList(stmts, constructStatement)).setPos(prog)
    }
  }

  def constructClass(ptree: NodeOrLeaf[Token]): Class = {
    ptree match {
      case Node(
        'ClassDeclaration ::= List(CLASS(), _, _, _),
        List(Leaf(cls), id, optextends, Node('ClassBody ::= _, List(_, vardecls, methoddecls, _)))
      ) =>
        ClassDecl(
          constructId(id),
          constructOption(optextends, constructId),
          constructList(vardecls, constructVarDecl),
          constructList(methoddecls, constructMethodDecl)
        ).setPos(cls)

      case Node(
      'ClassDeclaration ::= List(VALUE(), _, _, _),
      List(Leaf(value), _, id, Node('ClassBody ::= _, List(_, vardecls, methoddecls, _)))
      ) =>
        ValueClassDecl(
          constructId(id),
          constructList(vardecls, constructVarDecl),
          constructList(methoddecls, constructMethodDecl)
        ).setPos(value)
    }
  }

  def constructVarDecl(ptree: NodeOrLeaf[Token]): VarDecl = ptree match {
    case Node('VarDeclaration ::= _, List(Leaf(vr), param, _)) =>
      // Use the parser for parameters which we already have
      val Formal(tpe, id) = constructParam(param)
      VarDecl(tpe, id).setPos(vr)
  }

  def constructMethodDecl(ptree: NodeOrLeaf[Token]): MethodDecl = ptree match {
    case Node('MethodDeclaration ::= _, List(Leaf(meth), id, _, params, _, _, tpe, _, _, vardecs, stmts, _, expr, _, _)) =>
      MethodDecl(
        constructId(id),
        constructList(params, constructParam, hasComma = true),
        constructType(tpe),
        constructList(vardecs, constructVarDecl),
        constructList(stmts, constructStatement),
        constructExpr(expr)
      ).setPos(meth)
  }

  def constructParam(ptree: NodeOrLeaf[Token]): Formal = {
    ptree match {
      case Node('Param ::= _, List(id, _, tpe)) =>
        val pid = constructId(id)
        Formal(constructType(tpe), pid).setPos(pid)
    }
  }

  def constructId(ptree: NodeOrLeaf[Token]): Identifier = {
    ptree match {
      case Node('Identifier ::= _, List(Leaf(id@ID(name)))) =>
        Identifier(name).setPos(id)
    }
  }

  def constructType(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('Type ::= _, List(Leaf(i@INT()))) =>
        IntType().setPos(i)
      case Node('Type ::= List(INT(), LBRACKET(), RBRACKET()), List(Leaf(i@INT()), _, _)) =>
        IntArrayType().setPos(i)
      case Node('Type ::= _, List(Leaf(b@BOOLEAN()))) =>
        BooleanType().setPos(b)
      case Node('Type ::= _, List(Leaf(s@STRING()))) =>
        StringType().setPos(s)
      case Node('Type ::= List('Identifier), List(id)) =>
        val pid = constructId(id)
        ClassType(pid).setPos(pid)
    }
  }


  def constructStatement(ptree: NodeOrLeaf[Token]): StatTree = {
    ptree match {
      case Node('Statement ::= IF() :: _, List(Leaf(iff), _, expr, _, matchif, eopt)) =>
        If(constructExpr(expr), constructStatement(matchif), constructOption(eopt, constructStatement)).setPos(iff)
      case Node('Statement ::= IF() :: _, List(Leaf(iff), _, expr, _, thenif, _, eif)) =>
        If(constructExpr(expr), constructStatement(thenif), Some(constructStatement(eif))).setPos(iff)
      case Node(_ ::= List('SimpleStat), List(simpstat)) =>
        constructStatement(simpstat)
      case Node('SimpleStat ::= LBRACE() :: _, List(Leaf(lbr), stmts, _)) =>
        Block(constructList(stmts, constructStatement)).setPos(lbr)
      case Node('SimpleStat ::= WHILE() :: _, List(Leaf(whl), _, expr, _, stmt)) =>
        While(constructExpr(expr), constructStatement(stmt)).setPos(whl)
      case Node('SimpleStat ::= PRINTLN() :: _, List(Leaf(prln), _, expr, _, _)) =>
        Println(constructExpr(expr)).setPos(prln)
      case Node('SimpleStat ::= DO() :: _, List(Leaf(d), _, expr, _, _)) =>
        DoExpr(constructExpr(expr)).setPos(d)
      case Node('SimpleStat ::= rhs, List(id, idstat)) =>
        val pid = constructId(id)
        idstat match {
          case Node(_ ::= EQSIGN() :: _, List(_, expr, _)) =>
            Assign(pid, constructExpr(expr)).setPos(pid)
          case Node(_, List(_, index, _, _, expr, _)) =>
            ArrayAssign(pid, constructExpr(index), constructExpr(expr)).setPos(pid)
        }
    }
  }

  def constructOp(ptree: NodeOrLeaf[Token]): (ExprTree, ExprTree) => ExprTree = {
    ptree match {
      case Node(_, List(Leaf(t))) => (t: @unchecked) match {
        case AND()      => And
        case OR()       => Or
        case EQUALS()   => Equals
        case LESSTHAN() => LessThan
        case PLUS()     => Plus
        case MINUS()    => Minus
        case TIMES()    => Times
        case DIV()      => Div
      }
    }
  }

  def constructExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('Expression ::= List('Expression, 'Op, 'Expression), List(e1, op, e2)) =>
        val pe1 = constructExpr(e1)
        val pe2 = constructExpr(e2)
        constructOp(op)(pe1, pe2).setPos(pe1)
      case Node('Expression ::= List('Expression, LBRACKET(), 'Expression, RBRACKET()), List(e1, _, e2, _)) =>
        val pe1 = constructExpr(e1)
        val pe2 = constructExpr(e2)
        ArrayRead(pe1, pe2).setPos(pe1)
      case Node('Expression ::= List('Expression, DOT(), LENGTH()), List(e, _, _)) =>
        val pe = constructExpr(e)
        ArrayLength(pe).setPos(pe)
      case Node('Expression ::= List('Expression, DOT(), 'Identifier, LPAREN(), 'Args, RPAREN()), List(e, _, id, _, as, _)) =>
        val pe = constructExpr(e)
        MethodCall(pe, constructId(id), constructList(as, constructExpr, hasComma = true)).setPos(pe)
      case Node('Expression ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) =>
        IntLit(i).setPos(it)
      case Node('Expression ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
        StringLit(s).setPos(st)
      case Node('Expression ::= _, List(Leaf(tt@TRUE()))) =>
        True().setPos(tt)
      case Node('Expression ::= _, List(Leaf(tf@FALSE()))) =>
        False().setPos(tf)
      case Node('Expression ::= List('Identifier), List(id)) =>
        val pid = constructId(id)
        Variable(pid).setPos(pid)
      case Node('Expression ::=  _, List(Leaf(tt@THIS()))) =>
        This().setPos(tt)
      case Node('Expression ::= List(NEW(), INT(), LBRACKET(), 'Expression, RBRACKET()), List(Leaf(nt), _, _, e, _)) =>
        NewIntArray(constructExpr(e)).setPos(nt)
      case Node('Expression ::= List(NEW(), 'Identifier, LPAREN(), RPAREN()), List(Leaf(nt), id, _, _)) =>
        New(constructId(id)).setPos(nt)
      /*Project extension */
      case Node('Expression ::= List(NEW(), 'Identifier, LPAREN(), 'Expression, RPAREN()), List(Leaf(nt), id, _ ,e, _)) =>
        NewValueClass(constructId(id), constructExpr(e)).setPos(nt)
      case Node('Expression ::= List(BANG(), 'Expression), List(Leaf(bt), e)) =>
        Not(constructExpr(e)).setPos(bt)
      case Node('Expression ::= List(LPAREN(), 'Expression, RPAREN()), List(Leaf(lp), e, _)) =>
        constructExpr(e).setPos(lp)
    }
  }

  /** Extracts a List of elements of a generic type A, possibly separated by commas,
    * from a parse tree, by repeating a given parser.
    *
    * The form of the parse tree has to be specific: (t, ts) if there is no
    * comma, and (COMMA(), t, ts) if there is a comma, where t is the tree corresponding
    * to the first element and ts to the rest. Thankfully, this is the case every time
    * we need to parse a List in Tool.
    *
    * @param ptree The input parse tree
    * @param constructor A transformer for an individual object
    * @param hasComma Whether the elements of the list are separated by a COMMA()
    * @tparam A The type of List elements
    * @return A list of parsed elements of type A
    */
  def constructList[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A, hasComma: Boolean = false): List[A] = {
    ptree match {
      case Node(_, List()) => List()
      case Node(_, List(t, ts)) =>
        constructor(t) :: constructList(ts, constructor, hasComma)
      case Node(_, List(Leaf(COMMA()), t, ts)) if hasComma =>
        constructor(t) :: constructList(ts, constructor, hasComma)
    }
  }

  /** Optionally extract an element from a parse tree.
    *
    * The parse tree has to have a specific form: empty production will result in None,
    * and an operator (which will be ignored) followed by the element we need to extract
    * in case of Some.
    *
    * @param ptree The input parse tree
    * @param constructor The extractor of the element if it is present
    * @tparam A The type of the element
    * @return The element wrapped in Some(), or None if the production is empty.
    */
  def constructOption[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A): Option[A] = {
    ptree match {
      case Node(_, List()) => None
      case Node(_, List(_, t)) =>
        Some(constructor(t))
    }
  }

}
