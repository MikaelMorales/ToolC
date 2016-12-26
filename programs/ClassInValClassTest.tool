program ValueClassTest {
	println(new B(new A().init(3, 4+5)).sum(5) + new B(new A().init(1, 2*2)).product(2));
}
//expected 27

class A {
	var x: Int;
	var y: Int;

	def init(a: Int, b: Int): Int = {
		x = a;
		y = b;
		return this;
	}

	def compute(): Int = {
	    return x + y;
	}
}

cvalue class B {
	var x: A;

	def sum(y: Int): Int = {
	    return x.compute() + y;
	}

	def product(z: Int): Int = {
	    return x.compute() * z;
	}
}