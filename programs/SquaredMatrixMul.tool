program SquaredMatrixMul{
	do(new Start().Init(10));
}

class Start {
	var matrix1: SquaredMatrix;
	var matrix2: SquaredMatrix;
	var result: SquaredMatrix;
	
	def Init(sz: Int): Bool = {
		matrix1 = new SquaredMatrix().create(sz);
		matrix2 = new SquaredMatrix().create(sz);
		do(this.FillMatrix(matrix1, 0)); //On choisit la valeur que l'on va ajouter a la matrice en deuxieme argument
		do(this.FillMatrix(matrix2, 100));
		result = matrix1.mul(matrix2);
		println("On multiplie ces deux matrices : ");
		do(matrix1.Print());
		println("");
		do(matrix2.Print());
		println("");
		println("Et le résultat est :");
		println("");
		do(result.Print());

		return true;
	}

	def FillMatrix(m: SquaredMatrix, startingValue: Int): Int = {
		var r: Int;
		var c: Int;
		var value: Int;

		value = 0 + startingValue;
		r = 0;

		while(r < m.size()) {
			c = 0;
			while(c < m.size()) {
				do(m.set(r,c,value));
				value = value + 1;
				c = c + 1;
			}
			r = r + 1;
		}
		return 0;
	}
}

class SquaredMatrix {
	var matrix: Int[];
	var rows: Int;
	var cols: Int;

	def create(size: Int): SquaredMatrix = {
		matrix = new Int[size*size];
		rows = size;
		cols = size;
		return this;
	}

	def size(): Int = {
		return rows;
	}

	def get(r: Int, c: Int): Int = {
		var i: Int;
		i = r*cols + c;
		return matrix[i];
	}

	def set(r: Int, c: Int, value: Int): Bool = {
		matrix[r*cols + c] = value;
		return true;
	}

	def mul(m: SquaredMatrix): SquaredMatrix = {
		var result: SquaredMatrix;
		var r: Int;
		var c: Int;
		var x: Int;
		var sum: Int;

		result = new SquaredMatrix().create(m.size());

		r = 0;
		while(r < rows) {
			c = 0;
			while(c < cols) {
				x = 0;
				sum = 0;
				while(x < rows) {
					sum = sum + this.get(r,x) * m.get(x,c);
					x = x + 1;
				}
				do(result.set(r,c, sum));
				c = c + 1;
			}
			r = r + 1;
		}
		return result;
	}

	def Print(): Bool = {
		var s: String;
		var c: Int;
		var r: Int;

		r = 0;
		while(r < rows) {
			c = 0;
			s = "| ";
			while(c < cols) {
				s = s + this.get(r,c) + " ";
				c = c + 1;
			}
			r = r + 1;
			s = s + "|";
			println(s);
		}
		return true;
	}
}