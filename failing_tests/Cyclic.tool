program Cyclic {
	println(new B(new A(5)).compute(3));
}

class A extends C {
    var x : Int;

	def sayHello(): String = {
		return "Hello";
	}
}

class B extends A {
	var s: Int;

	def compute(num: Int): Int = {
		return num * 3;
	}

	def foo(): Bool = {
		return true;
	}
}

class C extends B {
	var z: Int;

	def compute(num: Int): Int = {
	    return 1;
	}
}