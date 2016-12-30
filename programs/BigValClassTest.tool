program BigValClassTest {
    println("Expected: hi! 5 I'm always true My field is world 3");
	println(new A(new B(5)).foo().compute() + new C(true).getThis().hi() + new D("world").getThis().getThis().hi() + new E(new Int[5+3]).setItem(2, 3).getItem(2));
    println("***************");
    println("hi! 5 I'm always true My field is world 3" == new A(new B(5)).foo().compute() + new C(true).getThis().hi() + new D("world").getThis().getThis().hi() + new E(new Int[5+3]).setItem(2, 3).getItem(2));
}

@value class A {
	var s: B;

	def compute(): String = {
		return s.hi();
	}

	def foo(): A = {
		return this;
	}
}

@value class B {
	var x: Int;

	def hi(): String = {
		return "hi! " + x + " ";
	}
}

@value class C {
	var x: Bool;

	def hi(): String = {
		var ret: String;
		if(x)
		    ret =  "true";
		else
		    ret = "false";
		return "I'm always " + ret + " ";
	}

	def getThis(): C = {
	    return this;
	}
}

@value class D {
	var x: String;

	def hi(): String = {
		return "My field is " + x + " ";
	}

	def getThis(): D = {
	    return this;
	}
}

@value class E {
	var x: Int[];

	def getItem(pos: Int): Int = {
	    var ret: Int;
	    if(0-1 < pos && pos < x.length)
	        ret = x[pos];
	    else
	        ret = 0-1;
	    return ret;
	}

	def setItem(pos: Int, newElem: Int): E = {
	    if(0-1 < pos && pos < x.length)
	        x[pos] = newElem;
	    return this;
	}
}