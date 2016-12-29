program ThisValClassTest {
	println(new Test(5).get().getField());
}

cvalue class Test {
	var x: Int;

	def get(): Test = {
	    return this;
	}

	def getField(): Int = {
	    return x;
	}
}