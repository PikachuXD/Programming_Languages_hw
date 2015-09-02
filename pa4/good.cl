Class Main inherits IO {
	main() : 
		Object { 
             let 
		done : Bool <- false,
		l : Foo <- new Foo,
		a : String <- ""
	     in {
               while not done loop {
                 let s : String <- in_string () in 
                 if s = "" then
                   done <- true 
                 else 
                   a <- l.insert(s)
                 fi ;
               } pool ; 
               l.print_list () ;
             }
	};
};

Class Foo inherits IO {
	xcar : String;        
	
	print_list() : Object {
		{
		     out_string(xcar);
		}
	};
	insert(ss : String) : String {
		ss
	};

} ;
