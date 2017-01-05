program ArgumentNameCheck {
  println(new A(5).foo(10));
}
 
@value class A {
  var x: Int;

  def foo(x: Int): Int = {
    return x;
  }
}