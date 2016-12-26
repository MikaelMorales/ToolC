program RecursiveValueClasses {
	println(new A(new B(5)).foo().foo().foo().foo().foo().compute());
}

cvalue class A {
	var s: B;

	def compute(): String = {
		return s.hi();
	}

	def foo(): A = {
		return this;
	}
}

cvalue class B {
	var x: Int;

	def hi(): String = {
		return "hi!" + x;
	}
}