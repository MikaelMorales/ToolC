program ValueClassTest {
    println("Expected: Hello2");
    println(new Test(2).sayHello());
}

cvalue class Test {
    var x : Int;

	def sayHello(): String = {
		return "Hello" + x;
	}
}