class Main inherits IO {
	a : Int <- 2147483647;
	main() : Object {
		lol(a)
	} ;
	lol(x : Int) : Int {
		if x = 0 then 3 else lol(x-1) fi
	} ;
} ;

