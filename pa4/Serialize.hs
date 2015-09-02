
module Serialize where

import AstNodes
import Data.List
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map


{- ################
 - Printing Helper Methods
 - ############### -}

genList :: (a -> [String]) -> [a] -> [String]
genList printFunc list = [ show $ length list ] ++ concatMap printFunc list

{- ################
 - Printing Class Map
 - ############### -}

printClassMap :: Map Exp String -> Map String [Feature] -> [String]
printClassMap tmap m = "class_map" : genList printClassAttribs ( sort $ Map.keys m )
	where printClassAttribs class_name = class_name : genList (printAttrib tmap) ( m Map.! class_name )

printAttrib :: Map Exp String -> Feature -> [String]
printAttrib tmap (Attribute (ID _ name) (ID _ tname) exp ) = case exp of
	Nothing -> "no_initializer" : name : [ tname ]
	Just x ->  "initializer" : name : tname: printExp tmap x

{- #################
 - Printing Implementation Map
 - ################ -}

printImpMap :: Map Exp String -> Map String [(Feature, String)] -> [String] 
printImpMap tmap m = "implementation_map" : genList printClassMethods ( sort $ Map.keys m )
	where printClassMethods class_name = class_name : genList (printMethod tmap) ( m Map.! class_name)

{-
nubify :: Map String [(Feature, String)] -> String -> [(Feature, String)] -> [(Feature, String)]
nubify m c f_tups = output'
	where
		 names = map ( \( (Method (ID _ n) _ _ _), _) -> n) f_tups
		 first_inst m_name = head $ filter ( \( (Method (ID _ n) _ _ _), _ ) -> n==m_name ) $ reverse f_tups
		 output = [ first_inst m_name | m_name <- nub names ]
		 output' = map format output 
		 format (meth@(Method (ID _ name) _ _ _), o) =  (meth, root meth)
		 root meth = filter  m 
-}

printMethod :: Map Exp String -> (Feature, String) -> [String]
printMethod tmap ( (Method m_id@(ID line m_name) _ formals m_body), origin ) = case m_body of
	Nothing ->  m_name : genList formal_name formals ++ [ origin ] ++ printInternal m_id 
	Just x  -> traceShow (m_name, line, origin) $ traceShow origin $  m_name : genList formal_name formals ++ [ origin ] ++ printExp tmap x
	where formal_name (Formal (ID _ name) _) = [name]

{- #################
 - Printing Parent Map
 - ################ -}

printParentMap :: Map ID (Maybe ID) -> [String]
printParentMap m = "parent_map" : (show $ length (Map.keys m) - 1) : (concatMap showRelation $ sortBy sortID $ Map.keys m)
	where showRelation id@(ID _ s) = case m Map.! id of
		Nothing -> []
		Just (ID _ x) -> [s, x]

{- ###################
 - Printing AST 
 - ################## -}

printInternal :: ID -> [String]
printInternal (ID _ name) = case name of
	"abort" -> [ "0", "Object", "internal", "Object.abort"] 
	"type_name" -> [ "0", "String", "internal", "Object.type_name"] 
	"copy" -> [ "0", "SELF_TYPE", "internal", "Object.copy"] 
	"out_string" -> [ "0", "SELF_TYPE", "internal", "IO.out_string"]
	"out_int" -> [ "0", "SELF_TYPE", "internal", "IO.out_int"]
	"in_string" -> [ "0", "String", "internal", "IO.in_string"]
	"in_int" -> [ "0", "Int", "internal", "IO.in_int"]
	"length" -> [ "0", "Int", "internal", "String.length"]
	"concat" -> [ "0", "String", "internal", "String.concat"]
	"substr" -> [ "0", "String", "internal", "String.substr"]

printID :: ID -> [String]
printID (ID line name)  = line:[name]

printExpID :: String -> ID -> [String]
printExpID dtype (ID line name) = line:dtype:[name]

printFormal :: Formal -> [String]
printFormal (Formal f_id t_id ) = printID f_id ++ printID t_id

printAst :: Map Exp String -> Program -> [String]
printAst m p = genList (printClass m) p

printClass :: Map Exp String -> Class -> [String]
printClass m (Class c_id inh_id feats) = case inh_id of 
	Nothing -> printID c_id ++ ["no_inherits"] ++ genList (printFeature m) feats
	Just x  -> printID c_id ++ ["inherits"] ++ printID x ++ genList (printFeature m) feats

printFeature :: Map Exp String -> Feature -> [String]
printFeature m f = case f of
	(Attribute a_id t_id exp ) -> case exp of
		Nothing -> ["attribute_no_init"] ++ printID a_id ++ printID t_id
		Just x  -> ["attribute_init"] ++ printID a_id ++ printID t_id ++ printExp m x
	(Method m_id t_id formals body) -> case body of
		Nothing -> ["method"] ++ printID m_id ++ genList printFormal formals ++ printID t_id ++ printInternal m_id
		Just x  -> ["method"] ++ printID m_id ++ genList printFormal formals ++ printID t_id ++ printExp m x

printExp :: Map Exp String -> Exp -> [String]
printExp m e = case e of
	(Assign id var rhs)            -> printExpID (m Map.! e) id ++ printID var ++ printExp m rhs
	(Static id exp t_id m_id args) -> printExpID (m Map.! e) id ++ printExp m exp ++ printID t_id ++ printID m_id ++ genList (printExp m) args
	(Dynamic id exp m_id args)     -> printExpID (m Map.! e) id ++ printExp m exp ++ printID m_id ++ genList (printExp m) args
	(Self id m_id args)            -> printExpID (m Map.! e) id ++ printID m_id ++ genList (printExp m) args
	(If id pred if_exp then_exp)   -> printExpID (m Map.! e) id ++ printExp m pred ++ printExp m if_exp ++ printExp m then_exp
	(While id pred body)           -> printExpID (m Map.! e) id ++ printExp m pred ++ printExp m body
	(Block id body)                -> printExpID (m Map.! e) id ++ genList (printExp m) body
	(Let id binds exp)             -> printExpID (m Map.! e) id ++ genList (printLetBinding m) binds ++ printExp m exp
	(Case id body elems)           -> printExpID (m Map.! e) id ++ printExp m body ++ genList (printCaseElem m) elems
	(New id new_class_id)          -> printExpID (m Map.! e) id ++ printID new_class_id
	(Isvoid id exp)                -> printExpID (m Map.! e) id ++ printExp m exp
	(Plus id x y)                  -> printExpID (m Map.! e) id ++ printExp m x ++ printExp m y
	(Minus id x y)                 -> printExpID (m Map.! e) id ++ printExp m x ++ printExp m y
	(Times id x y)                 -> printExpID (m Map.! e) id ++ printExp m x ++ printExp m y
	(Divide id x y)                -> printExpID (m Map.! e) id ++ printExp m x ++ printExp m y
	(Negate id x)                  -> printExpID (m Map.! e) id ++ printExp m x
	(Lt id x y)                    -> printExpID (m Map.! e) id ++ printExp m x ++ printExp m y
	(Le id x y)                    -> printExpID (m Map.! e) id ++ printExp m x ++ printExp m y
	(Eq id x y)                    -> printExpID (m Map.! e) id ++ printExp m x ++ printExp m y
	(Not id x)                     -> printExpID (m Map.! e) id ++ printExp m x
	(Integer_exp id lexeme)        -> printExpID (m Map.! e) id ++ [show lexeme]
	(String_exp id lexeme)         -> printExpID (m Map.! e) id ++ [lexeme]
	(ID_exp id lexeme)             -> printExpID (m Map.! e) id ++ printID lexeme
	(Bool_exp id _)                -> printExpID (m Map.! e) id 

printLetBinding :: Map Exp String -> Let_binding -> [String]
printLetBinding m (Let_binding var id init_exp) = case init_exp of 
	Nothing -> ["let_binding_no_init"] ++ printID var ++ printID id
	Just x  -> ["let_binding_init"] ++ printID var ++ printID id ++ printExp m x

printCaseElem :: Map Exp String -> Case_element -> [String]
printCaseElem m (Case_element var ctype body) = printID var ++ printID ctype ++ printExp m body				
