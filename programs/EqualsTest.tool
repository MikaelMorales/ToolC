program EqualsTest {
    println("Expected: true");
	println(new A(5) == new A(5));
}

@value class A {
	var x: Int;
}