program InitValueClassTest {
	println(new Test(5+6).sayHello());
}

cvalue class Test {
	var x: Int;

	def sayHello(): String = {
		return "Hello";
	}
}