class Main inherits IO {
	x : X <- new X;
	a : Int <- 0;
	main() : Object {
		--lol(a)
		out_string("Hello world\n")
	} ;
	(*
		lol(x : Int) {
			if x < 9001 then lol(x+1) else out_string("It's OVER 9000!") fi 
		};

	  *)
};

class X {
	y : Y <- new Y;
};

class Y {
	x : X <- new X;
};
