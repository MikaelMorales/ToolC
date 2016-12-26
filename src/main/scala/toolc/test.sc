

trait Class {
  val id: String
  val methods: List[String]
  val parent: Option[String]
  val vars: List[String]
}

case class ClassDecl(id: String, parent: Option[String], vars: List[String], methods: List[String]) extends Class

case class ClassValueDecl(id: String, field: String, methods: List[String]) extends Class {
  val parent = None
  val vars = List(field)
}

class A(val x: Int) extends AnyVal {
  def foo(): A = this
}

print(new A(5).foo())

