class Main inherits IO {
	x : X <- (new Y)@X.init();
	main() : Object {
		case x of a : X => out_string("Hello_world\n");
			  b : Y => out_string("Sup_world\n");
		esac
	} ;
};

class X {
	init() : SELF_TYPE {
		self
	};
};

class Y inherits X {

};
