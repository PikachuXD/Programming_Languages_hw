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
