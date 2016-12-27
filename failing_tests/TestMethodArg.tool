program TestMethodArg {
	println(new A().compute("a"));
}

class A {
    var x : Int;

	def compute(x: Int): Int = {
		return x;
	}
}
