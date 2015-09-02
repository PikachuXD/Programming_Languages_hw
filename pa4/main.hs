import System.Environment
import System.IO
import System.Exit
import AstNodes
import Unserialize
import Serialize
import Debug.Trace
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map


import Debug.Trace

--Building maps
{- ######################
 - Building Maps
 - ####################### -}

-- maps each class to all of its parent classes (ascending order) 
-- this is used to build other maps actually used in typechecking
buildRawMap:: Program -> Map Class [Class]
buildRawMap p = Map.fromList $ map makePair p
	where 
		makePair c = (c, getParents c )
		getParents c@(Class (ID _ name) pname _ ) = case pname of
			Nothing -> case name of
				"Object" -> []
				_ -> [ objectClass ]
			Just (ID _ x) -> [c'] ++ getParents c'
				where 
					c' = getClassInst x
					getClassInst s = head $ filter ( \(Class (ID _ name) _ _) -> s == name) p

-- maps class names to all attributes (in ascending order -- object being greatest)
buildClassMap :: Map Class [Class] -> Map String [Feature]
buildClassMap m = Map.fromList . map makePair $ Map.keys m
	where makePair c@(Class (ID _ name) _ _)  =  (name, filter isAttrib $ getAscFeatures m c)

-- maps class names to all method tuples (method, method origin) in ascending order
buildImpMap :: Program -> Map Class [Class] -> Map String [(Feature, String)]
buildImpMap p m = Map.fromList $ map makePair $ Map.keys m 
	where 
		makePair c@(Class (ID _ name) _ _)  =  ( name, [(meth, getOrigin c meth) | meth <- allMethods c] )
		allMethods c@(Class _ _ feats)  = dumb_sort m c p (getMethods (filter (not . isAttrib) feats) (m Map.! c))
		getOrigin c meth =  class_name ( last ( filter (hasMethod meth) ((m Map.! c) ++ [c])))
		class_name (Class (ID _ name) _ _) = name
		hasMethod (Method (ID _ name) _ _ _) (Class _ _ feats) = name `elem` [ name' | (Method (ID _ name') _ _ _) <- filter (not . isAttrib) feats ]

-- maps class ids to parent ids 
buildParentMap :: Map Class [Class] -> Map ID (Maybe ID)
buildParentMap m = Map.fromList $ map makePair $ Map.keys m
	where makePair (Class cID@(ID _ name) pname _) = case name of  
		"Object" -> ( objectID , Nothing)
		_ -> case pname of 
			Nothing -> (cID, Just objectID)
			Just x -> (cID, Just x)

{- ######################
 - Building Maps Helpers
 - ####################### -}

-- determines if feature is attribute
isAttrib :: Feature -> Bool
isAttrib f = case f of 
	(Attribute _ _ _) -> True
	_ -> False

-- generates a list of all features for a class (in ascending order)
getAscFeatures :: Map Class [Class] -> Class -> [Feature]
getAscFeatures m c@(Class _ _ f) = f ++ concatMap (\(Class _ _ f') -> f') (m Map.! c)

getMethods :: [Feature] -> [Class] -> [Feature]
getMethods acc c = case c of
	((Class _ _ feats):xs) -> if xs == [] then new_methods++acc else getMethods (new_methods++acc) xs
		where new_methods = [ m | m@(Method (ID _ name) _ _ _) <- filter (not . isAttrib) feats, not (name `elem` (map ( \(Method (ID _ n) _ _ _)->n ) acc)) ]
	_ -> acc
	
-- this is disgusting -- a last minute hack to fix a pesky error
dumb_sort :: Map Class [Class] -> Class -> Program -> [Feature] -> [Feature]
dumb_sort m c@(Class (ID _ c_name) _ _) p feats = [ head $ filter (\(Method (ID _ n) _ _ _) -> n == name) feats | name <- sorted_names ]
	 where 
		
		sorted_names = internal_names ++ (allNames `intersect` ( (map (\(Method (ID _ n) _ _ _) -> n) feats) \\ (non_inherited++internal_names) )) ++ non_inherited 
		allNames = nub (map (\(Method (ID _ n) _ _ _) -> n) ordered_feats )
		ordered_feats = filter (not . isAttrib) $ concatMap (\(Class _ _ f) -> f) p

		internal_classes = (filter ( \(Class (ID _ n) _ _) -> n `elem` [ "Object", "String", "IO", "Int", "Bool"] ) (reverse $ m Map.! c))
		internal_names = map (\(Method (ID _ n) _ _ _) -> n) (concatMap (\(Class _ _ f) -> f) internal_classes)
		non_inherited = [ name | meth@(Method (ID _ name) _ _ _) <-feats,  (getOrigin c meth) == True] \\ internal_names
		getOrigin c meth =  length ( filter (hasMethod meth) $ (m Map.! c) ++ [c] ) == 1 && (class_name . last . filter (hasMethod meth) $ (m Map.! c) ++ [c]) == c_name 
		class_name (Class (ID _ name) _ _) = name
		hasMethod (Method (ID _ name) _ _ _) (Class _ _ feats) = name `elem` [ name' | (Method (ID _ name') _ _ _) <- filter (not . isAttrib) feats ]

{- ######################
 - Pre Map Construction Checks
 - Errors here would cause building of raw map to fail
 - ####################### -}

checkCycle :: Program -> Maybe Error
checkCycle p = if or $ map (hasCycle class_tups []) class_tups then Just (Error "0" "Cycle brah...") else Nothing
	where 
		class_tups = [ (x, y) | (Class (ID _ x) (Just (ID _ y)) _) <- p ]
		hasCycle tups visited (x, y) 
			| x `elem` visited =  True
			| length visited == length tups = False
			| otherwise = case parent_list of
				[] -> False	
				_ ->  hasCycle tups (x:visited) (head parent_list)
				where parent_list = [ t | t@(x', _) <- tups, x' == y ]

checkUnkownInherits :: Program -> Maybe Error
checkUnkownInherits p = case find inherits_unknown p of
	Nothing -> Nothing
	Just (Class _ (Just (ID line _)) _ ) -> Just (Error line "Inherits from unknown class.") 
	where 
		inherits_unknown (Class _ inherits_id _) = case inherits_id of
			Nothing -> False
			Just (ID _ name) -> (not $ name `elem` (map (\(Class (ID _ x) _ _) -> x) p)) 

checkRedefObject :: Program -> Maybe Error
checkRedefObject p = case getRedef False p of 
	Nothing -> Nothing
	Just (Class (ID line _ ) _ _) -> Just (Error line "class Object overriden.")
	where 
		getRedef visited classes = case classes of
			( c@(Class (ID _ name) _ _):xs ) -> if name=="Object" && visited then Just c else getRedef True xs
			_ -> Nothing

checkIllegalOverride :: Program -> Maybe Error
checkIllegalOverride p = case find fst $ map (illegal_overide []) (map methods p)  of
	Nothing -> Nothing
	Just (_, Just (Method (ID line _) _ _ _)) -> Just (Error line "Method redefined in same class.")
	where 
		illegal_overide visited methods = case methods of
			meth@(Method (ID _ name) _ _ _):xs -> if name `elem` visited then (True, Just meth) else illegal_overide (name:visited) xs  
			_ -> (False, Nothing)

		methods (Class _ _ feats) = filter (not . isAttrib) feats 
		
			
{- ######################
 - Error Checking Implementation Map
 - ####################### -}

checkImpMap :: Map String [(Feature, String)] -> Maybe Error
checkImpMap m = case (find isError [ check m | check <- checks ]) of
	Just (Just e) ->  Just e
	_ -> Nothing
	where checks = [ checkMain, checkReturns, checkFormalNames, 
	                 checkFormalType, checkDupFormals, checkOverrideType,
					 checkOverrideFormals ]

checkMain :: Map String [(Feature, String)] -> Maybe Error
checkMain m
	| checkClass && checkMethod == True = Nothing
	| otherwise = Just (Error "0" "Main class including main method not found")
		where 
			checkClass = "Main" `elem` (Map.keys m)
			checkMethod = case find (\e -> get_method_name e == "main") (map fst (m Map.! "Main")) of
				Nothing -> False
				Just (Method _ _ formals _) -> if length formals == 0 then True else False
			get_method_name (Method (ID _ name) _ _ _) = name
			
checkReturns :: Map String [(Feature, String)] -> Maybe Error
checkReturns m = case find return_invalid allFeatures of
	Nothing -> Nothing
	Just (Method _ (ID line  _) _ _) -> Just (Error line "Undefined method return type.")
	where  
		allFeatures = map fst $ concat (Map.elems m)
		return_invalid (Method _ (ID _ return_type) _ _) = not $ return_type `elem` "SELF_TYPE":(Map.keys m)

checkFormalNames :: Map String [(Feature, String)] -> Maybe Error
checkFormalNames m = case find invalid_formal allFormals of
	Nothing -> Nothing
	Just (Formal (ID line _) _) -> Just (Error line "Formal has name 'self'.")
	where 
		allFormals = concatMap get_formals $ map fst $ concat (Map.elems m)
		get_formals (Method _ _ formals _ ) = formals
		invalid_formal (Formal (ID _ name) _ ) = "self" == name

checkFormalType:: Map String [(Feature, String)] -> Maybe Error
checkFormalType m = case find invalid_formal allFormals of
	Nothing -> Nothing
	Just (Formal _ (ID line _) ) -> Just (Error line "Invalid formal type.")
	where 
		allFormals = concatMap get_formals $ map fst $ concat (Map.elems m)
		get_formals (Method _ _ formals _ ) = formals
		invalid_formal (Formal _ (ID _ tname) ) = not (tname `elem` "SELF_TYPE":(Map.keys m)) || tname == "SELF_TYPE"

checkDupFormals :: Map String [(Feature, String)] -> Maybe Error
checkDupFormals m = case find fst (map (getDup []) allFormals) of
	Nothing -> Nothing
	Just (_, Just (Formal (ID line _) _)) -> Just (Error line "Duplicate formal names.")
	where 
		allFormals = map get_formals $ map fst $ concat (Map.elems m)
		get_formals (Method _ _ formals _ ) = formals
		getDup visited formals = case formals of 
			(f@(Formal (ID _ name) _):xs) -> if name `elem` visited then (True, Just f) else getDup (name:visited) xs
			_ -> (False, Nothing)

checkOverrideType :: Map String [(Feature, String)] -> Maybe Error
checkOverrideType m = case find invalid_override  allMethods of
	Nothing -> Nothing
	Just ( Method _ (ID line _) _ _ , _) -> Just (Error line "Invalid return type in overriden method.")
	where 
		allMethods = concat (Map.elems m)
		invalid_override ( (Method (ID _ name) (ID _ rtype) _ _), origin) = super_rtype /= rtype
			where ( Method _ (ID _ super_rtype) _ _, _ ) = head $ filter (\(Method (ID _ super_name) _ _ _ , _) -> super_name == name) (m Map.! origin)

checkOverrideFormals :: Map String [(Feature, String)] -> Maybe Error
checkOverrideFormals m  = case find invalid_override allMethods of
	Nothing -> Nothing
	Just (Method (ID line _) _ _ _, _) -> Just (Error line "Invalid overridden formals.") 
	where
		allMethods = concat (Map.elems m)
		invalid_override ( (Method (ID _ name) _ formals _), origin) = not $ length_check && conform_check
			where 
				length_check = length super_formals == length formals 
				conform_check =  and $ map conforms (zip super_formals formals) 
				conforms ( Formal _ (ID _ t_name1),  Formal _ (ID _ t_name2) ) = t_name1 == t_name2
				(Method _ _ super_formals _) = fst $ head $ filter (\(Method (ID _ super_name) _ _ _ , _) -> super_name == name) (m Map.! origin)

{- ######################
 - Error Checking Class Map
 - ####################### -}

checkClassMap :: Map String [Feature] -> Maybe Error
checkClassMap m = case (find isError [ check m | check <- checks ] ) of
	Just (Just e) -> Just e
	_ -> Nothing
	where checks = [ checkRedefAttrib, checkAttribTypes, checkAttribNames ]

checkRedefAttrib :: Map String [Feature] -> Maybe Error
checkRedefAttrib m = case find fst $ map (getRedef []) (map reverse (Map.elems m)) of
	Nothing -> Nothing
	Just (_, Just (Attribute (ID line _ ) _ _) ) -> Just (Error line "Attribute overriden.")
	where getRedef visited attributes = case attributes of
		( a@(Attribute (ID _ name) _ _):xs ) ->  if name `elem` visited then (True, Just a) else getRedef (name:visited) xs
		_ -> (False, Nothing)

checkAttribTypes :: Map String [Feature] -> Maybe Error
checkAttribTypes m = case find bad_type $ concat (Map.elems m) of
	Nothing -> Nothing
	Just (Attribute _ (ID line _) _) -> Just (Error line "Attribute is of unknown type.") 
	where bad_type (Attribute  _ (ID _ t_name) _) = not $ t_name `elem` "SELF_TYPE":(Map.keys m)  

checkAttribNames :: Map String [Feature] -> Maybe Error
checkAttribNames m = case find invalid_name $ concat (Map.elems m) of
	Nothing -> Nothing
	Just (Attribute _ (ID line _) _) -> Just (Error line "Attribute has name 'self'.") 
	where invalid_name (Attribute (ID _ name) _ _) = name == "self"

{- ######################
 - Error Checking Parent Map
 - ####################### -}

checkParentMap :: Map ID (Maybe ID) -> Maybe Error
checkParentMap m = case (find isError [ check m | check <- checks ]) of
	Just (Just e) -> Just e
	_ -> Nothing
	where checks = [ checkRedefClass, checkSelfClass,
					 inheritsInternal ]
	
checkRedefClass :: Map ID (Maybe ID) -> Maybe Error
checkRedefClass m = case getDup [] (Map.keys m) of
	Nothing -> Nothing
	Just (ID line _) -> Just (Error line "Class redefined.")
	where getDup visited ids = case ids of
		(id@(ID _ name):xs) -> if name `elem` visited then (Just id) else getDup (name:visited) xs
		_ -> Nothing

checkSelfClass :: Map ID (Maybe ID) -> Maybe Error
checkSelfClass m = case get_self $ Map.keys m of
	Nothing -> Nothing
	Just (ID line _) -> Just (Error line "Class has name 'SELF_TYPE'")
	where get_self ids = find (\(ID _ name) -> name == "SELF_TYPE") ids 

inheritsInternal :: Map ID (Maybe ID) -> Maybe Error
inheritsInternal m = case find inherits_internal $ Map.elems m of
	Nothing -> Nothing
	Just (Just (ID line _)) -> Just (Error line "Inherits from internal class.")
	where 
		inherits_internal id = case id of
			Nothing -> False 
			Just (ID _ name) -> name `elem` ["Int", "String", "Bool"]
	
{- ######################
 - Error Checking Helpers
 - ####################### -}

isError :: Maybe Error -> Bool
isError e = case e of  
	Nothing -> False
	_ -> True

{- #######################
 - Error checking expressions helpers
  ####################### -}
--
-- removes uneeded info from parent map
stripParentMap :: Map ID (Maybe ID) -> Map String String
stripParentMap m = Map.fromList [ (c, getParent c_id) | c_id@(ID _ c) <- Map.keys m ]
	where getParent id = case m Map.! id of
		Just (ID _ name) -> name
		_ -> ""

-- removes uneeded info from parent map 
-- maps (method name, return_type, formal_types)
stripImpMap :: Map String [(Feature, String)] -> Map String [(String, String, [String])]
stripImpMap m = Map.fromList [ (c, getInfo c) | c <- Map.keys m ]
	where
		getInfo c = [ (name, r_type, getTypes formals) | (Method (ID _ name) (ID _ r_type) formals _, _) <- (m Map.! c) ]
		getTypes formals = map (\(Formal _ (ID _ t) )-> t) formals
		
--least upper bound method
--this is mostly straight from slides
lub :: Map String String -> String -> String -> String -> String
lub pMap c a b = case (a, b) of
	("SELF_TYPE", "SELF_TYPE") -> "SELF_TYPE"
	("SELF_TYPE", t) -> rec_lub c t
	(t, "SELF_TYPE") -> rec_lub c t
	(t, t') -> rec_lub t t'
	where 
		rec_lub a b = head [ a | a <- root_path a, a `elem` root_path b ] 
		root_path x = x : let x' = (pMap Map.! x) in if x' == "" then [] else root_path x'

--checks if "root" type conforms to "child" type
--this is also straight from the slides
conforms :: Map String String -> String -> String -> String -> Bool
conforms pMap c root child = case (root, child) of
	("SELF_TYPE", "SELF_TYPE") -> True
	("SELF_TYPE", t) -> False
	(t, "SELF_TYPE") ->  rec_conforms t c
	( t, t' ) -> rec_conforms t t'
	where rec_conforms root child = case (root, child) of
		("Object", _) -> True
		(_, "Object") -> False
		(root, child) -> if root==child then True else rec_conforms root (pMap Map.! child) 

conformList :: Map String String -> String -> [String] -> [String] -> Bool
conformList p c roots childs = and [ conforms p c root child | (root, child) <- zip roots childs ]

--builds the initial object map (contains declared attributes)
initialObject ::  Map String [Feature] -> String -> Map String String
initialObject m c_name = Map.fromList type_tups
	where type_tups = [ (a_name, dec_type) | (Attribute (ID _ a_name) (ID _ dec_type) _ ) <- (m Map.! c_name) ] 


{- ####################
 - Error checking expressions
 - ################### -}

-- generates a type map ( expressions -> types ) or throws error. This is used when printing the ast.
-- top level of recursion
getTypeMap :: Map String [Feature] -> Map String [(Feature, String)] -> Map ID (Maybe ID) -> Program -> Either Error (String, Map Exp String)
getTypeMap cMap iMap pMap p = foldl1 accResult $ map getTypeMap' p
	where
		getTypeMap' (Class (ID _ c) _ feats) = case feats of 
			-- some internals to not have features (fold crashes on empty list), so we return an empty map to make the accResult function happy
			[] -> Right ("", Map.empty)
			_  -> foldl1 accResult $ map (typeCheckFeature o m c p) feats
			where
				o = initialObject cMap c 
				m = stripImpMap iMap 
				p = stripParentMap pMap

-- combines the result of typchecking a list. That is, either returns an error or returns the union of all maps
accResult :: Either Error (String, Map Exp String) -> Either Error (String, Map Exp String) -> Either Error (String, Map Exp String)
accResult acc result = case (acc, result) of
	--already have an error? return error
	(Left err, _) -> Left err
	-- just found an error? return error
	(_, Left err) -> Left err
	-- else union the accumulator map with the new map (each expression is unique so no keys are overriden)
	-- the type returned will also be the very last type in the list -- this is useful later down
	(Right (_, acc), Right (last_type, new_map)) -> Right $ (last_type, acc `Map.union` new_map)

-- checks if method / attribute declared type matches its corresponding expression type
typeCheckFeature :: Map String String -> Map String [(String, String, [String])] -> String -> Map String String -> Feature -> Either Error (String, Map Exp String)
typeCheckFeature o m c p f = case f of

	--experession recursion starts here...
	(Method (ID line name) (ID _ r_type) formals (Just e)) -> case (typeCheckExp updated_o m c p e) of
		Right (exp_type, tmap) -> if not $ conforms p c r_type exp_type' then
				Left (Error line (exp_type++" does not conform to type "++r_type++" in definition of "++name)) 	
				else Right ("", tmap)  
				where exp_type' = exp_type --if exp_type == "SELF_TYPE" then c else exp_type
		Left err -> Left err 
		where updated_o = (Map.fromList [ (n, t) | (Formal (ID _ n) (ID _ t)) <- formals ]) `Map.union` o
			
	-- .... or here
	(Attribute (ID line name) (ID _ t_name) (Just e)) -> case (typeCheckExp o m c p e) of
		Right (exp_type, tmap) -> if not $ conforms p c t_name exp_type' then
				Left (Error line (exp_type++" does not conform to type "++t_name++" in definition of "++name)) 	
				else Right ("", tmap)  
				where exp_type' = exp_type --if exp_type == "SELF_TYPE" then c else exp_type
		Left err -> Left err 

	-- some internal classes might not have any features
	_ -> Right ("", Map.empty)

-- unions a list of maps together. Less verbose this way 
mapUn :: [Map Exp String] -> Map Exp String
mapUn lst = foldl1 (\acc e -> acc `Map.union` e) lst 

-- checks if call failed (isError is already taken)
isFail :: Either Error (String, Map Exp String) -> Bool
isFail return = case return of 
	Left _ -> True
	_ -> False

-- this just checks the type of an ID. Only ID_exp returns a map entry, 
-- but the return type has been kept the same here for uniformity. 
typeCheckID :: Map String String -> ID -> Either Error (String, Map Exp String)
typeCheckID o (ID line name)
	| name == "self" = Right ("SELF_TYPE", Map.empty)
	| name `Map.member` o =  Right (o Map.! name, Map.empty)
	| otherwise = Left $ Error line ("unbound identifier "++name)

typeCheckExp :: Map String String -> Map String [(String, String, [String])] -> String -> Map String String -> Exp -> Either Error (String, Map Exp String)
typeCheckExp o m c p e@(Assign (ID line _) lhs@(ID _ t_name) rhs) 
	| t_name == "self" = Left (Error line "cannot assign to self")
	| isFail ret_lhs = ret_lhs
	| isFail ret_rhs = ret_rhs
	| not $ conforms p c lhs_type rhs_type = Left $ Error line  (lhs_type++" does not conform to "++rhs_type++" in assign")
	| otherwise = Right (rhs_type, mapUn [rhs_tmap, Map.singleton e rhs_type])
		where 
			Right (rhs_type, rhs_tmap) = ret_rhs
			Right (lhs_type, _) = ret_lhs
			ret_lhs = typeCheckID o lhs
			ret_rhs = typeCheckExp o m c p rhs

typeCheckExp o m c p e@(Static (ID line _) call_exp (ID _ s_name) (ID _ m_name) args)
	| isFail ret_call_exp = ret_call_exp
	| isFail ret_acc_args = ret_acc_args
	| not ( conforms p c s_name call_exp_type) = Left $ Error line (s_name++" does not conform to "++call_exp_type++" in static dispatch")
	| not $ method_exists = Left $ Error line ("method "++m_name++" does not exist in class "++s_name)
	| length args /= length formals = Left $ Error line "wrong number of arguments in static dispatch" 
	| not $ formals_conform =  Left $ Error line "formal types do not conform in static dispatch"
	| otherwise = Right $ (master_return, mapUn [call_exp_tmap, acc_args_tmap, Map.singleton e master_return])
		where
			Right (call_exp_type, call_exp_tmap) = ret_call_exp
			Right (_, acc_args_tmap) = ret_acc_args
			ret_call_exp = typeCheckExp o m c p call_exp
			ret_acc_args = if length args == 0 then Right ("", Map.empty) else foldl1 accResult $ map (typeCheckExp o m c p) args

			method_query = filter (\(n, _, _) -> n==m_name)  (m Map.! s_name)
			method_exists = length method_query > 0
			(_, method_return, formals) =  head method_query
			master_return = if method_return == "SELF_TYPE" && s_name /= c then s_name else method_return 

			formals_conform = conformList p c formals [ acc_args_tmap Map.! e | e <- args ]

typeCheckExp o m c p e@(Dynamic (ID line _) call_exp (ID _ m_name) args)
	| isFail ret_call_exp = ret_call_exp
	| isFail ret_acc_args = ret_acc_args
	| not $ method_exists = Left $ Error line ("method "++m_name++" does not exist in class "++call_exp_type)
	| length args /= length formals = Left $ Error line "wrong number of arguments in static dispatch" 
	| not $ formals_conform =  Left $ Error line "formal types do not conform in dynamic dispatch"
	| otherwise = Right $ (master_return, mapUn [call_exp_tmap, acc_args_tmap, Map.singleton e master_return])
		where
			Right (x, call_exp_tmap) = ret_call_exp
			call_exp_type = if x == "SELF_TYPE" then c else x
			Right (_, acc_args_tmap) = ret_acc_args
			ret_call_exp = typeCheckExp o m c p call_exp
			ret_acc_args = if length args == 0 then Right ("", Map.empty) else foldl1 accResult $ map (typeCheckExp o m c p) args

			method_query = filter (\(n, _, _) -> n==m_name)  (m Map.! call_exp_type)
			method_exists = length method_query > 0
			(_, method_return, formals) = head method_query
			master_return = if method_return == "SELF_TYPE" && call_exp_type /= c then call_exp_type else method_return 
			lst =  [ acc_args_tmap Map.! e | e <- args ]
			formals_conform = conformList p c formals lst

typeCheckExp o m c p e@(Self (ID line _) (ID _ m_name) args)
	| isFail ret_acc_args = ret_acc_args
	| not $ method_exists = Left $ Error line ("method "++m_name++" does not exist in class "++c)
	| length args /= length formals = Left $ Error line "wrong number of arguments in static dispatch" 
	| not $ formals_conform =  Left $ Error line "formal types do not conform in self dispatch"
	| otherwise = Right $ (master_return, mapUn [ acc_args_tmap, Map.singleton e master_return])
		where
			Right (_, acc_args_tmap) = ret_acc_args
			ret_acc_args = if length args == 0 then Right ("", Map.empty) else foldl1 accResult $ map (typeCheckExp o m c p) args

			method_query = filter (\(n, _, _) -> n==m_name)  (m Map.! c)
			method_exists = length method_query > 0
			(_, method_return, formals) = head method_query
			master_return = method_return

			formals_conform = conformList p c formals [ acc_args_tmap Map.! e | e <- args ]

typeCheckExp o m c p e@(If (ID line _) pred th el)
	| isFail ret_pred = ret_pred
	| isFail ret_th = ret_th
	| isFail ret_el = ret_el
	| pred_type /= "Bool" =  Left $ Error line "Predicate of if expression not a Bool"
	| otherwise = Right $ (lub_type, mapUn [th_tmap, pred_tmap, el_tmap, Map.singleton e lub_type]) 
		where 
			lub_type = lub p c th_type el_type
			Right (pred_type, pred_tmap) = ret_pred
			Right (th_type, th_tmap) = ret_th
			Right (el_type, el_tmap) = ret_el
			ret_pred = typeCheckExp o m c p pred
			ret_th = typeCheckExp o m c p th
			ret_el = typeCheckExp o m c p el

typeCheckExp o m c p e@(While (ID line _) pred body)
	| isFail ret_pred = ret_pred
	| isFail ret_body = ret_body
	| pred_type /= "Bool" = Left $ Error line "While predicate not a bool."
	| otherwise = Right $ ("Object", mapUn [pred_tmap, body_tmap, Map.singleton e "Object"])
		where
			Right (pred_type, pred_tmap) = ret_pred
			Right (_, body_tmap) = ret_body
			ret_pred = typeCheckExp o m c p pred
			ret_body = typeCheckExp o m c p body

typeCheckExp o m c p e@(Block (ID line _) exps)
	| isFail acc_result = renumber acc_result
	| otherwise = Right (body_type, mapUn [exps_tmap, Map.singleton e body_type])
		where 
			Right (body_type, exps_tmap) = acc_result
			acc_result = if length exps == 0 then Right("", Map.empty) else foldl1 accResult $ map (typeCheckExp o m c p) exps

			renumber (Left (Error _ msg)) = Left $ Error line msg

typeCheckExp o m c p e@(Let (ID line _) bindings body) = case acc_ret_bindings of 
		Left err -> Left err
		_ -> if isFail ret_body then ret_body else Right (body_type, mapUn [bindings_tmap, body_tmap, Map.singleton e body_type])
		where 
			Right(body_type, body_tmap) = ret_body
			Right (omap_ext, (_, bindings_tmap)) = acc_ret_bindings		
			
			acc_ret_bindings = foldl (accLetBind m c p line) (Right (o, ("", Map.empty))) bindings
			updated_o = omap_ext `Map.union` o
			ret_body = typeCheckExp updated_o m c p body

typeCheckExp o m c p e@(Case (ID line _) parent cases) 
	| isFail ret_parent = ret_parent
	| isFail acc_case_elem = acc_case_elem
	| not $ conforms p c parent_type cases_type = Left $ Error line "Case lub type does not conform"
	| otherwise = case getDup [] cases of 
		Nothing -> Right (cases_type, mapUn [parent_tmap, cases_tmap, Map.singleton e cases_type])
		Just (Case_element (ID line _) _ _) -> Left $ Error line "case type more than once"
		where 
			Right (parent_type, parent_tmap) = ret_parent
			ret_parent = typeCheckExp o m c p parent

			Right (cases_type, cases_tmap) = acc_case_elem
			acc_case_elem = if length cases == 0 then Right ("", Map.empty) else foldl1 (accCaseElem p c) $ map (typeCheckCaseElem o m c p) cases

			getDup visited ids = case ids of
				(el@(Case_element _ (ID _ t_name) _):xs) -> if t_name `elem` visited then (Just el) else getDup (t_name:visited) xs
				_ -> Nothing

typeCheckExp o m c p e@(New _ (ID line t_name) )
	| t_name == "self" = Right ("SELF_TYPE", Map.singleton e "SELF_TYPE")
	| t_name `Map.member` p = Right (t_name, Map.singleton e t_name) 
	| otherwise = Left $ Error line "unkown type in new"

typeCheckExp o m c p e@(Isvoid (ID line _) exp )
	| isFail ret_exp = ret_exp
	| otherwise = Right ("Bool", mapUn [exp_tmap, Map.singleton e "Bool"]) 
		where
			Right (_, exp_tmap) = ret_exp
			ret_exp = typeCheckExp o m c p exp

typeCheckExp o m c p e@(Plus (ID line _) lhs rhs)
	| isFail ret_lhs = ret_lhs
	| isFail ret_rhs = ret_rhs
	| not ( lhs_type == "Int" && rhs_type == "Int" ) = Left $ Error line "non integer addition"
	| otherwise = Right ("Int", mapUn [lhs_tmap, rhs_tmap, Map.singleton e "Int"])
		where
			Right (lhs_type, lhs_tmap) = ret_lhs
			Right (rhs_type, rhs_tmap) = ret_rhs
			ret_lhs = typeCheckExp o m c p lhs
			ret_rhs = typeCheckExp o m c p rhs

typeCheckExp o m c p e@(Minus (ID line _) lhs rhs)
	| isFail ret_lhs = ret_lhs
	| isFail ret_rhs = ret_rhs
	| not ( lhs_type == "Int" && rhs_type == "Int" ) = Left $ Error line "non integer subtraction"
	| otherwise = Right ("Int", mapUn [lhs_tmap, rhs_tmap, Map.singleton e "Int"])
		where
			Right (lhs_type, lhs_tmap) = ret_lhs
			Right (rhs_type, rhs_tmap) = ret_rhs
			ret_lhs = typeCheckExp o m c p lhs
			ret_rhs = typeCheckExp o m c p rhs

typeCheckExp o m c p e@(Times (ID line _) lhs rhs)
	| isFail ret_lhs = ret_lhs
	| isFail ret_rhs = ret_rhs
	|  (lhs_type, rhs_type) /= ("Int", "Int") = Left $ Error line "non integer multiplication"
	| otherwise = Right ("Int", mapUn [lhs_tmap, rhs_tmap, Map.singleton e "Int"])
		where
			Right (lhs_type, lhs_tmap) = ret_lhs
			Right (rhs_type, rhs_tmap) = ret_rhs
			ret_lhs = typeCheckExp o m c p lhs
			ret_rhs = typeCheckExp o m c p rhs

typeCheckExp o m c p e@(Divide (ID line _) lhs rhs)
	| isFail ret_lhs = ret_lhs
	| isFail ret_rhs = ret_rhs
	|  (lhs_type, rhs_type) /= ("Int", "Int") = Left $ Error line "non integer multiplication"
	| otherwise = Right ("Int", mapUn [lhs_tmap, rhs_tmap, Map.singleton e "Int"])
		where
			Right (lhs_type, lhs_tmap) = ret_lhs
			Right (rhs_type, rhs_tmap) = ret_rhs
			ret_lhs = typeCheckExp o m c p lhs
			ret_rhs = typeCheckExp o m c p rhs

typeCheckExp o m c p e@(Negate (ID line _) exp )
	| isFail ret_exp = ret_exp
	| exp_type /= "Int" = Left $ Error line "negate not applied to integer"
	| otherwise = Right ("Int", mapUn [exp_tmap, Map.singleton e "Int"]) 
		where
			Right (exp_type, exp_tmap) = ret_exp
			ret_exp = typeCheckExp o m c p exp

typeCheckExp o m c p e@(Lt (ID line _) lhs rhs)
	| isFail ret_lhs = ret_lhs
	| isFail ret_rhs = ret_rhs
	| not $ (lhs_type, rhs_type) `elem` zip t t = Left $ Error line "invalid or incompatable types in <"
	| otherwise = Right ("Bool", mapUn [lhs_tmap, rhs_tmap, Map.singleton e "Bool"])
		where
			t = ["Int", "Bool", "String"]
			Right (lhs_type, lhs_tmap) = ret_lhs
			Right (rhs_type, rhs_tmap) = ret_rhs
			ret_lhs = typeCheckExp o m c p lhs
			ret_rhs = typeCheckExp o m c p rhs

typeCheckExp o m c p e@(Le (ID line _) lhs rhs)
	| isFail ret_lhs = ret_lhs
	| isFail ret_rhs = ret_rhs
	| not $ (lhs_type, rhs_type) `elem` zip t t = Left $ Error line "invalid or incompatable types in <="
	| otherwise = Right ("Bool", mapUn [lhs_tmap, rhs_tmap, Map.singleton e "Bool"])
		where
			t = ["Int", "Bool", "String"]
			Right (lhs_type, lhs_tmap) = ret_lhs
			Right (rhs_type, rhs_tmap) = ret_rhs
			ret_lhs = typeCheckExp o m c p lhs
			ret_rhs = typeCheckExp o m c p rhs

typeCheckExp o m c p e@(Eq (ID line _) lhs rhs)
	| isFail ret_lhs = ret_lhs
	| isFail ret_rhs = ret_rhs
	| not $ (lhs_type, rhs_type) `elem` zip t t = Left $ Error line "invalid or incompatable types in ="
	| otherwise = Right ("Bool", mapUn [lhs_tmap, rhs_tmap, Map.singleton e "Bool"])
		where
			t = ["Int", "Bool", "String"]
			Right (lhs_type, lhs_tmap) = ret_lhs
			Right (rhs_type, rhs_tmap) = ret_rhs
			ret_lhs = typeCheckExp o m c p lhs
			ret_rhs = typeCheckExp o m c p rhs

typeCheckExp o m c p e@(Not (ID line _) exp )
	| isFail ret_exp = ret_exp
	| exp_type /= "Bool" = Left $ Error line "not applied to non-bool"
	| otherwise = Right ("Bool", mapUn [exp_tmap, Map.singleton e "Bool"]) 
		where
			Right (exp_type, exp_tmap) = ret_exp
			ret_exp = typeCheckExp o m c p exp

typeCheckExp o m c p e@(ID_exp _ id)
	| isFail id_ret = id_ret
	| otherwise = Right (id_type, Map.singleton e id_type)
		where 
			id_ret = typeCheckID o id
			Right (id_type, _) = id_ret

-- constants
typeCheckExp o m c p e@(Integer_exp _ _) = Right ("Int", Map.singleton e "Int")
typeCheckExp o m c p e@(String_exp _ _) = Right ("String", Map.singleton e "String")
typeCheckExp o m c p e@(Bool_exp _ _) = Right ("Bool", Map.singleton e "Bool")

{- ###################################################
 - Due to our poor choice of types (no turning back at this point), 
 - we had to write seperate methods to handle case and let-bind elements
 - In hindsight, we should have generalized all aspects of the ast as some general type
 -}

-- returns a map of initialized variables and their types
-- this code is gross
typeCheckLetBind :: Map String String -> Map String [(String, String, [String])] -> String -> Map String String -> String -> Let_binding -> Either Error (Map String String, (String, Map Exp String))
typeCheckLetBind o m c p let_line (Let_binding (ID var_line name) (ID t_line t_name)  exp)
		| not $ t_name `elem` "SELF_TYPE":(Map.keys p) = Left $ Error t_line "undefined type in let binding."
		| name == "self" = Left $ Error var_line "assignment to self in let binding"
		| otherwise = case exp of 
			Just exp -> process_binding exp 
			_ -> Right (Map.singleton name t_name, ("", Map.empty))
		where process_binding e
				| isFail ret_e = return_error ret_e 
				| not $ conforms p c t_name e_type = Left $ Error let_line "initializer does not conform in let binding"
				| otherwise = Right (Map.singleton name t_name, (e_type, e_tmap)) 
					where
						Right (e_type, e_tmap) = ret_e
						ret_e = typeCheckExp o m c p e
						return_error (Left e) = Left e

-- to be used with fold to accumulate the result of typechecking all let bindings
accLetBind m c p line acc e = case (acc, e) of
	(Left err, _) ->  Left err 	
	(Right (omap, (_, tmap)), binding) -> case typeCheckLetBind omap m c p line binding of
		Left err -> Left err
		Right (omap', (_, tmap')) -> Right (omap' `Map.union` omap, ("", tmap `Map.union` tmap'))

typeCheckCaseElem :: Map String String -> Map String [(String, String, [String])] -> String -> Map String String -> Case_element -> Either Error (String, Map Exp String)
typeCheckCaseElem o m c p (Case_element (ID var_line var_name) (ID t_line t_name) exp)
		| isFail ret_exp = ret_exp
		| not $ t_name `elem` "SELF_TYPE":(Map.keys p) = Left $ Error t_line "undefined type in case statement"
		| t_name == "SELF_TYPE" = Left $ Error t_line "SELF_TYPE in case statement"
		| var_name == "self" = Left $ Error var_line "assignment to self in case statement"
		| otherwise = Right (exp_type, exp_tmap) 
			where 
				Right (exp_type, exp_tmap) = ret_exp
				ret_exp = typeCheckExp updated_o m c p exp
				updated_o = Map.insert var_name t_name o

accCaseElem :: Map String String -> String -> Either Error (String, Map Exp String) -> Either Error (String, Map Exp String) -> Either Error (String, Map Exp String)
accCaseElem p c acc e = case (acc, e) of 
	(Left err, _) -> Left err 	
	(_, Left err) -> Left err
	(Right (e_type, tmap), Right (e_type', tmap')) -> Right (lub p c e_type e_type', tmap `Map.union` tmap') 
			
{- ######################
 - Main
 - ####################### -}

genOutFilename :: String -> String
genOutFilename fn = take ((length fn)- 3 ) fn ++ "type"

printError :: Error -> IO b
printError (Error line msg) = do
	putStrLn $ "ERROR: " ++ line ++ ": Type-Check: " ++ msg
	exitFailure

main = do 
	argv <- getArgs
	let filename = head argv

	withFile filename ReadMode $ \handle -> do
		raw_ast <- hGetContents handle

		-- building program and testing for cycle
		let program = internals ++ ( build_program $ lines raw_ast )

		-- checking errors that would cause building rawMap to fail 
		case find isError [checkUnkownInherits program, checkCycle program, checkRedefObject program, checkIllegalOverride program] of
			Just (Just e) -> printError e
			_ -> return ()

		--building maps
		let rawMap = buildRawMap program
		let (classMap, impMap, parentMap) = (buildClassMap rawMap, buildImpMap program rawMap, buildParentMap rawMap)
		
		print $ map snd (impMap Map.! "Nil")
		
		--checking maps
		let result = [ checkClassMap classMap, checkImpMap impMap, checkParentMap parentMap ]
		
		case find isError result of  
			Just (Just e) ->  printError e
			_ -> return ()

		--putStrLn $ Map.showTree impMap

		-- this is just a test of executing the typeCheck
		case getTypeMap classMap impMap parentMap program of	
			Left err -> printError err
			Right (_, tmap) ->  do
				let output = (printClassMap tmap classMap) ++ (printImpMap tmap impMap) ++ (printParentMap parentMap) ++ (printAst tmap (program \\ internals))
				writeFile "test.txt" (unlines output)
				-- writeFile (genOutFilename filename) (unlines output)
		
