program ValueClassThis {
	println(new A(0).foo().foo().foo().foo().foo().compute());
}

cvalue class A {
	var x: Int;

	def compute(): Int = {
		return x;
	}

	def foo(): A = {
		return this;
	}
}