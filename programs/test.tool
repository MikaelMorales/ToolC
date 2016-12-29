program Test {
    println(new A().sayHello());
}

class A {
    var x : Int;

	def sayHello(): String = {
		return "Hello" + x;
	}
}