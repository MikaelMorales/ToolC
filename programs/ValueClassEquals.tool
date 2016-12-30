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
    println("**********************");
    println("Result should be false :");
    println(new D("hi") == new D("ho"));
    println("**********************");
    println("Result should be true :");
    println(new D("hi") == new D("hi"));
    println("**********************");
    println("Result should be false :");
    println(new E(new Int[2]) == new E(new Int[2]));

}

@value class A {
    var x : Int;
}

@value class B {
    var x: Int;
}

@value class C {
    var x: A;
}

@value class D {
    var x: String;
}

@value class E {
    var x: Int[];
}