program Main {
    println(new A(2).sayHello());
}

@value class A {
    var x : Int;

	def sayHello(): String = {
		return "Hello" + x;
	}
}