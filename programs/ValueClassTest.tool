program ValueClassTest {
	println(new A().compute(20));
}

class A {
	def compute(num : Int): Int = {
		return num;
	}
}

value class Test {
	var x: Int;

	def sayHello(): String = {
		return "Hello";
	}
}