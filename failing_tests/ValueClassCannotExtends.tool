program ValueClassCannotExtends {
	println(new B().foo());
}

@value class A extends B {
    var x : Int;

	def sayHello(): String = {
		return "Hello";
	}
}

class B {

	def foo(): Bool = {
		return true;
	}
}