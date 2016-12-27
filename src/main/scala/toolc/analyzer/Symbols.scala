package toolc
package analyzer

import utils._

import Types._

object Symbols {

  /** An object uniquely identified by a symbol */
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }

    def optSymbol = _sym
  }

  private object UniqueCounters {
    private var c: Int = -1

    def next: Int = {
      c = c + 1
      c
    }
  }

  /** Uniquely identifies a [[Symbolic]] object */
  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = UniqueCounters.next
    val name: String

    private var tpe_ : Type = TUntyped
    def getType = tpe_
    def setType(tpe: Type) = tpe_ = tpe
  }

  /** The global scope contains symbols of the main object and classes */
  class GlobalScope {
    var mainClass: MainSymbol = _
    var classes = Map[String,AbstractClassSymbol]()

    def lookupClass(n: String): Option[AbstractClassSymbol] = classes.get(n)
  }

  class MainSymbol(val name: String) extends Symbol

  /* Project extension */
  sealed trait AbstractClassSymbol extends Symbol {
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()
    var parent: Option[ClassSymbol] = None

    def lookupMethod(n: String): Option[MethodSymbol]

    def lookupVar(n: String): Option[VariableSymbol]
  }

  class ClassSymbol(val name: String) extends AbstractClassSymbol {
    override def getType = TClass(this)
    override def setType(t: Type) = sys.error("Cannot set the symbol of a ClassSymbol")

    override def lookupMethod(n: String): Option[MethodSymbol] = {
      methods.get(n) orElse parent.flatMap(_.lookupMethod(n))
    }

    override def lookupVar(n: String): Option[VariableSymbol] = {
      members.get(n) orElse parent.flatMap(_.lookupVar(n))
    }
  }

  class ValueClassSymbol(val name: String, val fieldId: String) extends AbstractClassSymbol {
    override def getType = TValueClass(this)
    override def setType(t: Type) = sys.error("Cannot set the symbol of a ValueClassSymbol")

    val field: Option[VariableSymbol] = members.get(fieldId)

    override def lookupMethod(n: String): Option[MethodSymbol] = methods.get(n)

    override def lookupVar(n: String): Option[VariableSymbol] = members.get(n)
  }

  class MethodSymbol(val name: String, val classSymbol: AbstractClassSymbol) extends Symbol {
    var params = Map[String,VariableSymbol]()
    var members = Map[String,VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden : Option[MethodSymbol] = None

    def overrides(ms : MethodSymbol) : Boolean = overridden match {
      case None => false
      case Some(pms) if pms == ms => true
      case Some(pms) => pms.overrides(ms)
    }

    def lookupVar(n: String): Option[VariableSymbol] = {
      members.get(n) orElse
      params.get(n) orElse
      classSymbol.lookupVar(n)
    }
  }

  class VariableSymbol(val name: String) extends Symbol
}
