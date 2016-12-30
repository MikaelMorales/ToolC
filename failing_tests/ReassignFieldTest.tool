program ReassignFieldTest {
    println(new Test(5).reassign(7));
}

@value class Test {
    var x : Int;

    def reassign(newX: Int): Int = {
        x = newX;
        return x;
    }
}