class Main inherits IO {
	a : Int <- 0;
	lol( b : Int , c : Int):Int {
		b - c
	};
	main() : Object {
		out_int(lol(a <- a-4, let g : Int <- a - 2 in g))
	} ;
} ;

