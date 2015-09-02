{- This module defines function serialize and unserialize a cool program -}

module Unserialize where

import AstNodes
import Control.Monad.State

build_program :: [String] -> Program
build_program ( n:tl ) = evalState ( build_list (read n :: Int) build_class [] ) tl

build_class :: State [String] Class 
build_class = do
	line:name:kind:tl <- get
	case kind of
		"inherits" -> do
			put new_state
			return ( Class (ID line name) (Just (ID pline pname)) feats )
				where 
					(pline:pname:n:tl') = tl
					(feats, new_state) = runState ( build_list (read n :: Int) build_feature [] ) tl' 
		"no_inherits" -> do
			put new_state
			return ( Class (ID line name) Nothing feats )
				where 
					(n:tl') = tl
					(feats, new_state) = runState ( build_list (read n :: Int) build_feature [] ) tl' 

build_feature :: State [String] Feature
build_feature = do
	kind:line:name:tl <- get
	case kind of
		"attribute_no_init" -> do
			put new_state
			return ( Attribute (ID line name) (ID tline tname) Nothing )
				where (tline:tname:new_state) = tl
		"attribute_init" -> do
			put new_state
			return ( Attribute (ID line name) (ID tline tname) (Just exp) )
				where 
					(tline:tname:tl') = tl
					(exp, new_state) = runState build_expression tl'
		"method" -> do
			put new_state
			return ( Method (ID line name) (ID tline tname) formals (Just body) )
				where 
					(n:tl') = tl
					(formals, tline:tname:tl'') = runState ( build_list (read n :: Int) build_formal [] ) tl'
					(body, new_state) = runState build_expression tl''


build_formal:: State [String] Formal
build_formal = do 
	line:name:tline:tname:tl <- get
	put tl	
	return (Formal (ID line name ) (ID tline tname) )

build_expression :: State [String] Exp
build_expression = do 
	line:kind:tl <- get
	case kind of

		"assign" -> do
			put new_state
			return (Assign (ID line kind) (ID tline tname) exp) 
				where
					(tline:tname:tl') = tl
					(exp, new_state) = runState build_expression tl'
		"dynamic_dispatch" -> do
			put new_state
			return (Dynamic (ID line kind) exp (ID mline mname) args )
				where 
					(exp, mline:mname:n:tl') = runState build_expression tl	
					(args, new_state) = runState ( build_list (read n :: Int) build_expression [] ) tl' 
		"static_dispatch" -> do
			put new_state
			return (Static (ID line kind) exp (ID sline sname) (ID mline mname) args)
				where
					(exp, sline:sname:mline:mname:n:tl') = runState build_expression tl
					(args, new_state) = runState ( build_list (read n :: Int) build_expression [] ) tl' 
		"self_dispatch" -> do
			put new_state
			return (Self (ID line kind) (ID mline mname) args)
				where
					( mline:mname:n:tl' ) = tl
					(args, new_state) = runState ( build_list (read n :: Int) build_expression [] ) tl' 
		"if" -> do
			put new_state
			return (If (ID line kind) predicate then_exp else_exp )
				where
					(predicate, tl') = runState build_expression tl
					(then_exp, tl'') = runState build_expression tl'  
					(else_exp, new_state) = runState build_expression tl''  
		"while" -> do
			put new_state	
			return (While  (ID line kind) predicate body )
				where
					(predicate, tl') = runState build_expression tl
					(body, new_state) = runState build_expression tl'
		"block" -> do
			put new_state
			return (Block (ID line kind) body )
				where
					(n:tl') = tl
					(body, new_state) = runState ( build_list (read n :: Int) build_expression [] ) tl'
		"new" -> do
			put new_state
			return (New (ID line kind) (ID cline cname) ) 
				where (cline:cname:new_state) = tl
		"isvoid" -> do	
			put new_state
			return (Isvoid (ID line kind) exp)
				where (exp, new_state) = runState build_expression tl
		"plus" -> do
			put new_state
			return (Plus (ID line kind) a_exp b_exp)
				where
					(a_exp, tl') = runState build_expression tl
					(b_exp, new_state) = runState build_expression tl'
		"minus" -> do
			put new_state
			return (Minus (ID line kind) a_exp b_exp)
				where
					(a_exp, tl') = runState build_expression tl
					(b_exp, new_state) = runState build_expression tl'
		"times" -> do
			put new_state
			return (Times (ID line kind) a_exp b_exp)
				where
					(a_exp, tl') = runState build_expression tl
					(b_exp, new_state) = runState build_expression tl'
		"divide" -> do
			put new_state
			return (Divide (ID line kind) a_exp b_exp)
				where
					(a_exp, tl') = runState build_expression tl
					(b_exp, new_state) = runState build_expression tl'
		"lt" -> do
			put new_state
			return (Lt (ID line kind) a_exp b_exp)
				where
					(a_exp, tl') = runState build_expression tl
					(b_exp, new_state) = runState build_expression tl'
		"le" -> do
			put new_state
			return (Le (ID line kind) a_exp b_exp)
				where
					(a_exp, tl') = runState build_expression tl
					(b_exp, new_state) = runState build_expression tl'
		"eq" -> do
			put new_state
			return (Eq (ID line kind) a_exp b_exp)
				where
					(a_exp, tl') = runState build_expression tl
					(b_exp, new_state) = runState build_expression tl'
		"not" -> do
			put new_state
			return (Not (ID line kind) a_exp)
				where (a_exp, new_state) = runState build_expression tl
		"negate" -> do
			put new_state
			return (Negate (ID line kind) a_exp)
				where (a_exp, new_state) = runState build_expression tl
		"integer" -> do
			put new_state	
			return (Integer_exp (ID line kind) (read value :: Int) )
				where (value:new_state) = tl
		"string" -> do
			put new_state	
			return (String_exp (ID line kind) value )
				where (value:new_state) = tl
		"identifier" -> do
			put new_state
			return  (ID_exp (ID line kind) (ID idline idname))
				where (idline:idname:new_state) = tl
		"true" -> do
			put tl
			return (Bool_exp (ID line kind) True)
		"false" -> do
			put tl
			return (Bool_exp (ID line kind) False)
		"let" -> do
			put new_state
			return ( Let (ID line kind) bind_list exp )
				where
					(n:tl') = tl
					(bind_list, tl'') = runState ( build_list (read n :: Int) build_let_binding [] ) tl'
					(exp, new_state) = runState build_expression tl''
		"case" -> do
			put new_state
			return (Case (ID line kind) exp case_elems)
				where
					(exp, n:tl') = runState build_expression tl
					(case_elems, new_state) = runState ( build_list (read n :: Int) build_case_elem [] ) tl'
			
build_let_binding :: State [String] Let_binding
build_let_binding = do
	kind:vline:vname:tline:tname:tl <- get
	case kind of
		"let_binding_no_init" -> do
			put tl
			return (Let_binding (ID vline vname) (ID tline tname) Nothing ) 
		"let_binding_init" -> do
			put new_state
			return (Let_binding (ID vline vname) (ID tline tname) (Just init_exp) ) 
				where (init_exp, new_state) = runState build_expression tl

build_case_elem :: State [String] Case_element
build_case_elem = do
	vline:vname:tline:tname:tl <- get	
	let ( body, new_state ) = runState build_expression tl
	put new_state
	return (Case_element (ID vline vname) (ID tline tname) body )

build_list :: Int -> State [String] a -> [a] -> State [String] [a]
build_list n f acc = do 
	case n of
		0 -> return acc
		_ -> do
			x <- f
			result <- build_list (n-1) f (acc ++ [x])
			return result
