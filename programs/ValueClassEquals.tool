program ValueClassTest {
    println("Result should be true :");
    println(new A(2) == new A(2));
    println("**********************");
    println("Result should be false :");
    println(new A(3) == new A(6));
    println("**********************");
    println("Result should be false :");
    println(new C(new A(3)) == new A(3));
    println("**********************");
    println("Result should be false :");
    println(new C(new A(3)) == new B(3));
}

cvalue class A {
    var x : Int;
}

cvalue class B {
    var x: Int;
}

cvalue class C {
    var x: A;
}
