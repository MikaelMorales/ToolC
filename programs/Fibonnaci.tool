program Fibonnaci {
	println("Fib 20 = " + new Fib().computeFibonnaci(20));
}

class Fib {
/*    Program that
compute
the
fibonnaci */
	def computeFibonnaci(num : Int): Int = {
		var num_aux : Int; 
		if(num < 2) {
			num_aux = num;
		}else{
			num_aux = (this.computeFibonnaci(num-1)) + (this.computeFibonnaci(num-2));
		}
		return num_aux;
	}
}