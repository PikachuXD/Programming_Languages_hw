class Main inherits IO {
	main() : Object {
		let x : X <- new X, y : Y <- new Y in
			{
				if x < y then
					out_string("Nope this is wrong\n")
				else
					{
						out_string("Sup\t yo\n");
						out_string("This is correct\n");
						out_int(~2147 + 124);
						out_string("\\\\\n");
					}
				fi;
			}
	} ;
};

class X {

};

class Y {

};
