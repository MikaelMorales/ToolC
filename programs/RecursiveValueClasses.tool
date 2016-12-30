program RecursiveValueClasses {
    println("Expected: hi!5");
	println(new A(new B(5)).foo().foo().foo().foo().foo().compute());
}

@value class A {
	var s: B;

	def compute(): String = {
		return s.hi();
	}

	def foo(): A = {
		return this;
	}
}

@value class B {
	var x: Int;

	def hi(): String = {
		return "hi!" + x;
	}
}