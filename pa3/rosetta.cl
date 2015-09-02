Class Sorter {
  getInst( lbl : String, g : List) : Node {
    let
      it : Iter <- (new Iter).init(g),
      n : Node <- it.step(),
      i : Node
    in {
      while not isvoid n loop{
        if n.getN() = lbl then
          i <- n
        else true fi;
          n <- it.step();
    } pool;
      i;
    }
  };

  topS( g : List, nE : Int) : List {
    let
      s : List <- new Nil,
      l : List <- new Nil
    in {
      let
        gIt : Iter <- (new Iter).init(g),
        c : Node <- gIt.step()
      in {
        while not isvoid c loop {
          if c.getIE() = 0 then
            s <- s.insert(c)
          else true fi;
          c <- gIt.step();
        } pool;
      };

      let
        --take incoming edge with lowest alphabetic val
        n : Node <- s.pop()
      in {
        while not isvoid n loop{
          --push the node into loop
          l <- l.push(n);
          let
            
            oIt : Iter <- (new Iter).init(n.getDL()),
            c : Node <- oIt.step()
          in{
            while not isvoid c loop{
              c.setIE(c.getIE() - 1);
              nE <- nE - 1;
              if c.getIE() = 0 then
                s <- s.insert(c)
              else true fi;

              c <- oIt.step();
            } pool;
          };
          n <- s.pop();
        } pool;
      };

    if not (nE = 0) then
      let void: List in {void; }
    else
      l
    fi;
  }
};
};
Class Node inherits Nil {
  lbl : String;
  inEd : Int;
  dstList : List;

  init(newLbl : String) : Node{
    {
      lbl <- newLbl;
      inEd <- 0;
      dstList <- new Nil;
      self;
    }
  };

  getN() : String {lbl};
  getIE() : Int {inEd};
  setIE( dg : Int ) : Int {inEd <- dg};
  getDL() : List {dstList};
  setDL(dl : List) : List {dstList <- dl};
};

Class Main -- Main is where the program starts
  inherits IO { -- inheriting from IO allows us to read and write strings

	main() : Object { -- this method is invoked when the program starts
             let
                 nE : Int <- 0,
                 l : List,
                 done : Bool <- false,
                 s : Sorter <- new Sorter,
                 g : List <- new Nil
             in {
               while not done loop {
                 let
                  c : String <- in_string(),
                  p : String <- in_string(),
                  c_n : Node,
                  p_n : Node
                in{
                  if c = "" then
                    done <- true
                  else{
                    c_n <- s.getInst(c, g);
                    if isvoid c_n then {
                      c_n <- (new Node).init(c);
                      g <- g.push(c_n);
                    }
                    else true fi;

                    p_n <- s.getInst(p, g);
                    if isvoid p_n then {
                      p_n <- (new Node).init(p);
                      g <- g.push(p_n);
                    }
                    else true fi;

                    p_n.setDL(p_n.getDL().push(c_n));
                    c_n.setIE(c_n.getIE() + 1);
                    nE <- nE + 1;
                  }
                  fi;
                };
               } pool ; -- loop/pool deliniates a while loop body
               --l.print_list () ; -- print out the result
             l <- s.topS(g, nE);

             if isvoid l then
              out_string("cycle\n")
             else
              l.print_list()
              fi;
      }
	};
};

Class List inherits IO {
	insert(i : Node) : List { self };
  push(i : Node) : List { self };
  pop() : Node {let void : Node in {void; }};
  getHead() : Node {let void : Node in {void; }};
  getTail() : List {let void : List in {void; }};
	print_list() : Object { abort() };
} ;

Class Cons inherits List { -- a Cons cell is a non-empty list
	xcar : Node;          -- xcar is the contents of the list head
	xcdr : List;            -- xcdr is the rest of the list

	init(hd : Node, tl : List) : Cons {
	  {
	    xcar <- hd;
	    xcdr <- tl;
	    self;
	  }
	};

	insert(s : Node) : List {
		if isvoid xcar then
      (new Nil).insert(s)
    else
      if (s.getN() < xcar.getN()) then
        (new Cons).init(s, self)
      else
        (new Cons).init(xcar, xcdr.insert(s))
      fi
    fi
	};

	print_list() : Object {
		{
		     out_string(xcar.getN());
		     out_string("\n");
		     xcdr.print_list();
		}
	};

  push(n: Node) : List {
      if isvoid xcar then
        (new Nil).insert(n)
      else
        if isvoid xcdr.getHead() then
          (new Cons).init(xcar, (new Nil).push(n))
        else
          (new Cons).init(xcar, xcdr.push(n))
        fi
      fi
  };

  pop() : Node {
    let ret : Node <- xcar
    in {
      if not isvoid ret then {
        xcar <- xcdr.getHead();
        xcdr <- xcdr.getTail();
      }
      else true fi;
      ret;
    }
  };
  getHead() : Node {xcar};
  getTail() : List {xcdr};
} ;

Class Iter {
  list : List;
  xcar : Node;

  init(nList : List) : Iter {
      {
          list <- nList;
          xcar <- nList.getHead();
          self;
      }
  };

  step() : Node {
    let ret : Node
    in {
      if not isvoid xcar then
        {
          ret <- xcar;
          list <- list.getTail();
          xcar <- list.getHead();
        }
      else true fi;
      ret;
    }
  };
};

Class Nil inherits List { -- Nil is an empty list

	insert(n : Node) : List { (new Cons).init(n,self) };
  push(n : Node) : List {insert(n)};
	print_list() : Object { true }; -- do nothing

} ;
