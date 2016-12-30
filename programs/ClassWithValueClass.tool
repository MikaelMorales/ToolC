program ClassWithValueClass {
    println("Expected: Hi ! ");
    println(new A().init().print());
}

class A {
    var x: B;

	def init(): B = {
		x = new B("Hi ! ");
		return x;
	}
}

@value class B {
    var y: String;

    def print(): String = {
        return y;
    }
}