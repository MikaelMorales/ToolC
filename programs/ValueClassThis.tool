program ValueClassThis {
    println("Expected: 0");
	println(new A(0).foo().foo().foo().foo().foo().compute());
}

@value class A {
	var x: Int;

	def compute(): Int = {
		return x;
	}

	def foo(): A = {
		return this;
	}
}