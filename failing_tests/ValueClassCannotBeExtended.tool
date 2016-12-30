program ValueClassCannotExtends {
	println(new B().foo());
}

@value class A {
    var x : Int;

	def sayHello(): String = {
		return "Hello";
	}
}

class B extends A {
	var s: Int;

	def foo(): Bool = {
		return true;
	}
}