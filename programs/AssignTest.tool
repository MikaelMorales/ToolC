program AssignTest {
    println("Expected: 7");
    println(new Test(5).reassign(7));
}

cvalue class Test {
    var x : Int;

    def reassign(newX: Int): Int = {
        var x: Int;
        x = newX;
        return x;
    }
}