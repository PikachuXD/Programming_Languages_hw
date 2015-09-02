-- just... ugh. You have better things to do than to read this.

Class Node inherits Nil {

	label : String;
	inDeg : Int;
	outList : List; 

	init(newLabel : String) : Node{
	  {
	    label <- newLabel;
	    inDeg <- 0;
		outList <- new Nil;
	    self;
	  }
	};

	get_label() : String { label };
	get_inDeg() : Int { inDeg };
	set_inDeg( deg : Int ) : Int { inDeg <- deg };
	get_outList() : List { outList };
	set_outList(l : List) : List { outList <- l }; 

};

Class Util {

	getInstance( label : String, graph : List ) : Node {
		let
			iter : Iterator <- (new Iterator).init(graph),
			curr_Node : Node <- iter.step(),
			instance : Node
		in {
			while not isvoid curr_Node loop{

				if curr_Node.get_label() = label then
					instance <- curr_Node
				else true fi;

				curr_Node <- iter.step();

			} pool;
			instance;
		}
	};

	topo( graph : List, nEdges : Int) : List {
	
		let
			s : List <- new Nil,
			l : List <- new Nil
		in {
			
			let 
				g_iter : Iterator <- (new Iterator).init(graph),
				curr_Node : Node <- g_iter.step()
			in {
				while not isvoid curr_Node loop {

					if curr_Node.get_inDeg() = 0 then
						s <- s.insert(curr_Node)
					else true fi;
					
					curr_Node <- g_iter.step();

				} pool;
			};

			let 
				n : Node <- s.pop()
			in {
				while not isvoid n loop{
					l <- l.push(n);

					let
						out_iter : Iterator <- (new Iterator).init(n.get_outList()), 
						curr_Node : Node <- out_iter.step()
					in{
						while not isvoid curr_Node loop{
							curr_Node.set_inDeg( curr_Node.get_inDeg() - 1 );
							nEdges <- nEdges - 1;

							if curr_Node.get_inDeg() = 0 then
								s <- s.insert(curr_Node)
							else true fi;

							curr_Node <- out_iter.step();
						} pool;
					};
					n <- s.pop();
				} pool;
			};

			if not (nEdges = 0) then 			
				let void : List in { void; }	
			else 
				l
			fi;
		}
	};
};


Class Main inherits IO {

	main() : Object {
		let 
			u : Util <- new Util,
			graph : List <- new Nil,
			done : Bool <- false,
			nEdges : Int <- 0,
			l : List
		in {
			while not done loop {
				
				let 
					child : String <- in_string (),
					parent : String <- in_string (),
					child_inst : Node,
					parent_inst : Node
				in{
					if child = "" then
						done <- true 
					else{ 

						child_inst <- u.getInstance(child, graph);  
						if isvoid child_inst then {
							child_inst <- (new Node).init(child);
							graph <- graph.push( child_inst );
						}
						else true fi;

						parent_inst <- u.getInstance(parent, graph);
						if isvoid parent_inst then {
							parent_inst <- (new Node).init(parent);
							graph <- graph.push( parent_inst );
						}
						else true fi;

						parent_inst.set_outList( parent_inst.get_outList().push( child_inst ) );
						child_inst.set_inDeg(child_inst.get_inDeg() + 1);
						nEdges <- nEdges + 1;

					}
					fi;
				};
			}pool;

			l <- u.topo( graph, nEdges );

			if isvoid l then
				out_string("cycle\n")
			else
				l.print_list()
			fi;
		}
	};
};


Class List inherits IO{ 

	insert(i : Node) : List { self };
	push(i : Node) : List { self };
	pop() : Node { let void : Node in { void; } };
	get_hd() : Node { let void : Node in { void; } };
	get_tl() : List { let void : List in { void; } };
	print_list() : Object { abort() };

} ;

Class Cons inherits List {

	hd : Node;
	tl : List;

	init(new_hd : Node, new_tl : List) : Cons {
	  {
		hd <- new_hd;
		tl <- new_tl;
	    self;
	  }
	};
	  
	insert(n : Node) : List {

		if isvoid hd then
			(new Nil).insert(n)
		else
			if ( n.get_label() < hd.get_label() ) then
				(new Cons).init(n,self)
			else
				(new Cons).init(hd, tl.insert(n))
			fi
		fi
	};

	pop() : Node {
		let return : Node <- hd
		in {
			if not isvoid return then {
				hd <- tl.get_hd();
				tl <- tl.get_tl();
			}
			else true fi;
			return;
		}
	};

	push(n : Node) : List {
		if isvoid hd then
			(new Nil).insert(n)
		else
			if isvoid tl.get_hd() then
				(new Cons).init(hd, (new Nil).push(n))
			else
				(new Cons).init(hd, tl.push(n))
			fi
		fi
	};

	print_list() : Object {
		{
		     out_string(hd.get_label());
		     out_string("\n");
		     tl.print_list();
		}
	};

	get_hd() : Node { hd };
	get_tl() : List { tl };

} ;

Class Nil inherits List {
	insert(n : Node) : List { (new Cons).init(n,self) }; 
	push(n : Node) : List { insert(n) };
	print_list() : Object { true };
};

Class Iterator {

	list : List;
	hd : Node;

	init( newList : List) : Iterator {
		{
			list <- newList;
			hd <- newList.get_hd();
			self;
		}
	};

	step() : Node {
		let return : Node
		in {
			if not isvoid hd then 
				{
					return <- hd;
					list <- list.get_tl();
					hd <- list.get_hd();	
				}
			else true fi;
			return;
		}
	};

};
