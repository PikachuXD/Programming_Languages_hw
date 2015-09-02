class Main inherits IO {
	main() : Object {
		let a : A <- new A in let b : A <- a.copy() in if a = b then out_int(2) else out_int(3) fi
	} ;
};

class A {
};
