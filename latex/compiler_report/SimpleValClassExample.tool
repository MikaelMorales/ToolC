program Main {
	println(new A(new B(new C())).get());
}

@value class A {
	var b: B;
	def get(): Int = { 
		return b.get(); 
	}
}

@value class B {
	var c: C;
	def get(): Int = { 
		return c.get(); 
	} 
}

class C {
	def get(): Int = { 
		return 2; 
	}
}
