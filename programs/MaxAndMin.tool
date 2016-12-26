program MaxAndMin {
	do(new MaximumAndMinimum().Start(30));
}

class MaximumAndMinimum {
	var number: Int[];
	var size: Int;

	def Start(sz: Int): Int = {
		do(this.Init(sz));
		do(this.Print());
		do(this.max());
		do(this.min());
		return 0;
	}

	def Print() : Int = {
        var j : Int;
        println("The array is: ");
        j = 0 ;
        while (j < (size)) {
            println(number[j]);
            j = j + 1 ;
        }
        return 0 ;
    }

    def max(): Int = {
    	var max: Int;
    	var i: Int;
    	max = number[0];
    	i = 1;
    	while (i < (size)) {
    		if(max < number[i]) {
    			max = number[i];
    		}
    		i = i + 1;
    	}
    	println("Max Value is: " + max);
    	return max;
    }

    def min(): Int = {
    	var min: Int;
    	var i: Int;
    	min = number[0];
    	i = 1;
    	while (i < (size)) {
    		if(number[i] < min) {
    			min = number[i];
    		}
    		i = i + 1;
    	}
    	println("Min Value is: " + min);
    	return min;
    }

	def Init(sz: Int): Int = {
		size = sz;
		number = new Int[sz];

		number[0] = 20 ;
        number[1] = 7  ; 
        number[2] = 12 ;
        number[3] = 18 ;
        number[4] = 2  ; 
        number[5] = 11 ;
        number[6] = 6  ; 
        number[7] = 9  ; 
        number[8] = 19 ; 
        number[9] = 5  ;
        number[10] = 30 ;
        number[11] = 70  ; 
        number[12] = 120 ;
        number[13] = 0 ;
        number[14] = 24  ; 
        number[15] = 115 ;
        number[16] = 62  ; 
        number[17] = 94  ; 
        number[18] = 192 ; 
        number[19] = 53  ;
        number[20] = 203 ;
        number[21] = 71  ; 
        number[22] = 12 ;
        number[23] = 187 ;
        number[24] = 25  ; 
        number[25] = 112 ;
        number[26] = 63  ; 
        number[27] = 95  ; 
        number[28] = 19 ; 
        number[29] = 55  ;

        return 0;
	}
}