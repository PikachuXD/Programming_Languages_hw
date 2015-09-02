class Main inherits IO {
	a : String <- "a";
	main() : Object {
		{a <- a.concat("B");
		out_string(a);}
	} ;
} ;

