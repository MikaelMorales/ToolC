program Main {
	println(new A(5).foo().foo().foo().foo().compute());
}

@value class A {
	var s: Int;

	def compute(): Int = {
		return s;
	}

	def foo(): A = {
		return this;
	}
}
