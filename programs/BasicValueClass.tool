program ValueClassTest {
    println(new Test(2).sayHello()); //Expected Hello2
}

cvalue class Test {
    var x : Int;

	def sayHello(): String = {
		return "Hello" + x;
	}
}