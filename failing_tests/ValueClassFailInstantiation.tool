program ValueClassFailInstantiation {
	println(new A().sayHello());
}

cvalue class A {
    var x : Int;

	def sayHello(): String = {
		return "Hello";
	}
}