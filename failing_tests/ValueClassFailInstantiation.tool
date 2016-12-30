program ValueClassFailInstantiation {
	println(new A().sayHello());
}

@value class A {
    var x : Int;

	def sayHello(): String = {
		return "Hello";
	}
}