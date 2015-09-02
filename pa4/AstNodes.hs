{- this module defines all of the AST node data types -}
module AstNodes where

import Data.Map (Map)
import qualified Data.Map as Map

{- ###########################
 - data and type declarations
 - ######################### -}

data Error = Error String String deriving (Show)

type Program = [ Class ]

-- line, name
data ID = ID String String deriving (Eq, Ord, Show)

-- class_id, inherits_id, features
data Class = Class ID (Maybe ID) [Feature] deriving (Eq, Ord, Show)

data Feature = 
	-- attribute_id, attribute_type_it, attribute_init_expression
	  Attribute ID ID (Maybe Exp)
	-- method_id, type_id, formals, body
	| Method	ID ID [Formal] (Maybe Exp) deriving (Eq, Ord, Show)

-- formal_id, formal_type_id
data Formal = Formal ID ID deriving (Eq, Ord, Show)

data Exp = 
	-- expression_id, lhs_id, rhs_exp
	  Assign			ID ID Exp 
	-- expression_id, expression, static_type_id, method_id, args
	| Static        	ID Exp ID ID [Exp] 
	-- expression_id, expression, method_id, args
	| Dynamic        	ID Exp ID [Exp]
	-- expression_id, method_id, args
	| Self	         	ID ID [Exp]
	-- expression_id, prediate, then_expression, else_expression
	| If				ID Exp Exp Exp
	-- expression_id, predicate, while_body
	| While				ID Exp Exp
	-- expression_id, body
	| Block				ID [Exp]
	-- expression_id, bindings_list, expression
	| Let   			ID [Let_binding] Exp
	-- exprssion_id, body_expression, element_list
	| Case  			ID Exp [Case_element]
	-- expression_id, new_class_id
	| New				ID ID
	-- expression_id, expression
	| Isvoid			ID Exp
	-- expression_id x y
	| Plus				ID Exp Exp
	| Minus				ID Exp Exp
	| Times				ID Exp Exp
	| Divide			ID Exp Exp
	| Negate			ID Exp 
	| Lt				ID Exp Exp
	| Le				ID Exp Exp
	| Eq				ID Exp Exp
	| Not	 			ID Exp
	| ID_exp            ID ID 
	| Integer_exp		ID Int
	| String_exp		ID String
	| Bool_exp			ID Bool deriving (Eq, Ord, Show)

-- var, type_id, init_exp
data Let_binding = Let_binding ID ID (Maybe Exp) deriving (Eq, Ord, Show)

-- var, type_id, case_body
data Case_element = Case_element ID ID Exp deriving (Eq, Ord, Show)

{- ################
 - Sorting Methods
- ################ -}

sortID :: ID -> ID -> Ordering
sortID (ID line1 name1) (ID line2 name2 )
	| name1 < name2 = LT
	| name1 > name2 = GT
	| otherwise = compare (read line1 :: Int) (read line2 :: Int)

sortClass :: Class -> Class -> Ordering
sortClass ( Class id1 _ _ ) ( Class id2 _ _ ) = sortID id1 id2


{- ################################
 - Predifined Cool Objects
 - ############################### -}

internals :: [Class]
internals = [ objectClass, ioClass, stringClass, intClass, boolClass ]
internals_names = map (\(Class (ID _ name) _ _) -> name) [ objectClass, ioClass, stringClass, intClass, boolClass ]

objectFeats :: [Feature]
objectFeats = [  Method (ID "0" "abort") objectID [] Nothing 
              ,	 Method (ID "0" "copy") self_typeID [] Nothing
              , ( Method (ID "0" "type_name") stringID [] Nothing )
			  ]

objectClass :: Class
objectClass = ( Class objectID Nothing objectFeats ) 

ioFeats :: [Feature]
ioFeats = [ ( Method (ID "0" "in_int") intID [] Nothing ) 
		  , ( Method (ID "0" "in_string") stringID [] Nothing )
		  , ( Method (ID "0" "out_int") self_typeID [ (Formal (ID "0" "x") intID) ] Nothing )
		  ,	( Method (ID "0" "out_string") self_typeID [ (Formal (ID "0" "x") stringID) ] Nothing )
		  ]
            
ioClass :: Class
ioClass = ( Class ioID (Just objectID) ioFeats ) 
          
stringFeats :: [Feature]
stringFeats = [ ( Method (ID "0" "concat") stringID [ (Formal (ID "0" "s") stringID ) ] Nothing ) 
			  , ( Method (ID "0" "length") intID [] Nothing )
		      , ( Method (ID "0" "substr") stringID [ (Formal (ID "0" "i") intID), (Formal (ID "0" "l") intID) ] Nothing )
		      ]

stringClass :: Class
stringClass = ( Class stringID (Just objectID) stringFeats ) 

intClass :: Class
intClass = ( Class intID (Just objectID) [] ) 

boolClass :: Class
boolClass = ( Class boolID (Just objectID) [] ) 

objectID :: ID 
objectID = (ID "0" "Object")

ioID:: ID 
ioID = (ID "0" "IO")

stringID:: ID 
stringID = (ID "0" "String")

intID:: ID 
intID= (ID "0" "Int")

boolID:: ID 
boolID = (ID "0" "Bool")

self_typeID:: ID 
self_typeID = (ID "0" "SELF_TYPE")

-- internals = [ "abort", "type_name", "copy", "out_string", "out_int", "in_string", "in_int", "length", "concat", "substr" ]
