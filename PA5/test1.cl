class Main inherits IO {
	x : Point <- new Point;
	main() : Object {
		let y : Point <- x.copy() in
			{
				y.setX(8);
				x.setX(7);
				out_int(x.getX());
				out_int(y.getX());
				x.setY(1);
				y.setY(2);
				out_int(x.getY());
				out_int(y.getY());
				let z : Point <- x.copy() in
				{
					out_int(z.getX());
					x.setX(19);
					out_int(x.getX());
					out_int(z.getX());
				};
			}
	} ;
};

class Point {
	x : Int;
	y : Int;
	
	getX() : Int { x };
	getY() : Int { y };
	setX(a : Int) : Object { x <- a };
	setY(b : Int) : Object { y <- b };
};
