program ValueClassTest {
    println("Expected: 37");
	println(new Test(10+4*3).sum(5) + new Test(4-2).product(5));
}

@value class Test {
	var x: Int;

	def sum(y: Int): Int = {
	    return x + y;
	}

	def product(z: Int): Int = {
	    return x * z;
	}
}