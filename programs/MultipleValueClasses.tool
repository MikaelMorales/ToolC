program MultipleValueClasses {
    println("Expected: 9");
	println(new B(new A(5)).compute(3));
}

@value class A {
    var x : Int;

	def sayHello(): String = {
		return "Hello";
	}
}

@value class B {
	var s: A;

	def compute(num: Int): Int = {
		return num * 3;
	}

	def foo(): Bool = {
		return true;
	}
}