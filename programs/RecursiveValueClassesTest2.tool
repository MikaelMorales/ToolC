program RecursiveValueClassesTest2 {
	println(new A(5).foo().foo().foo().foo().foo().compute());
}

cvalue class A {
	var s: Int;

	def compute(): Int = {
		return s;
	}

	def foo(): A = {
		return new A(s + 10);
	}
}
