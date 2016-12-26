program Main {
   println(new Foo().bar(1));
}

class Foo {
   var x: Int;
   
   def bar(i: Int): Int = {
   	if(true || (1/0 == 1))
   		x = 42;
   	else
   		x = 1;
    return (x + i);
   }
}