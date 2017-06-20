module program1

import StdEnv
import StdMaybe
import StdLib

class serialize a where
	write :: a [String] -> [String]
	read :: [String] -> Maybe (a,[String])

instance serialize Bool where
	write b c = [toString b:c]
	read ["True":r] = Just (True,r)
	read ["False":r] = Just (False,r)
	read _ = Nothing
	
instance serialize Int where
	write b c = [toString b:c]
	read [s:r] = Just (toInt s,r)
	read _ = Nothing

instance serialize [a] | serialize a where
	write [] c = [NilString : c]
	write [a:x] c = ["(",ConsString : write a (write x [")":c])]
	read [NilString:r] = Just ([],r)
	read ["(",ConsString:r] =
		case read r of
			Just (a,s) = case read s of
				Just (x,[")":t]) = Just ([a:x],t)
				_ = Nothing
			_ = Nothing
	read _ = Nothing

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
instance serialize (Bin a) | serialize a where
	write Leaf c = [LeafString:c]
	write (Bin l a r) c = ["(",BinString:write l (write a (write r [")":c]))]
	read [LeafString:r] = Just (Leaf,r)
	read ["(",BinString:r] =
		case read r of
			Just (l,s) = case read s of
				Just (a,t) = case read t of
					Just (r,[")":u]) = Just (Bin l a r,u)
					_ = Nothing
				_ = Nothing
			_ = Nothing
	read _ = Nothing

:: Rose a = Rose a [Rose a]
instance serialize (Rose a) | serialize a where
	write (Rose a []) c = write a [")":c]
	write (Rose a l) c = [RoseString:write a (write l c)]
	read _ = Nothing

RoseString :== "Rose"
BinString :== "Bin"
LeafString :== "Leaf"
NilString :== "Nil"
ConsString :== "Cons"
	
test :: a -> Bool | serialize,==a
test a = (isJust r && fst jr==a)
where
	s = write a ["ln"]
	r = read s
	jr = fromJust r

//Bool = *
//Bin = * -> *
//Rose = * -> *
//Bin Int = *
//Tree = * -> * -> *
//T1 = (*->*)->*->*
//T2 = (*->*)->(*->*)->*->*
//T3 = (*->*->*)->*->*->*
//T4 = (*->*)->(*->*)->*->*

class Container t where
    Cinsert   :: a (t a) -> t a      | <        a
    Ccontains :: a (t a) -> Bool     | <, Eq    a
    Cshow     ::   (t a) -> [String] | toString a
    Cnew      :: t a

instance Container [] where
	Cinsert a c = [a:c]
	Ccontains a c = isMember a c
	Cnew = []
	Cshow l = [toString e \\ e <- l]

instance Container Bin where
	Cinsert a Leaf = Bin Leaf a Leaf
	Cinsert a (Bin l b r)
		| a < b = Bin (Cinsert a l) b r
		| otherwise = Bin l b (Cinsert a r)
	Ccontains a Leaf = False
	Ccontains a (Bin l b r)
		| a == b = True
		| a < b = Ccontains a l
		| otherwise = Ccontains a r
	Cnew = Leaf
	Cshow t = ["bla"]


//Start = [test True, test False, test 2, test[1,2,3]]
//Start = write (Bin (Bin Leaf 1 Leaf) 2 (Bin Leaf 3 Leaf)) ["S"]
//Start = write (Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 []], Rose 6 []]) ["s"]
//Start = write (Rose 1 [Rose 2 [], Rose 3 []]) ["s"]
//Start = write [1,2,3] ["s"]

b = Bin (Bin Leaf 1 Leaf) 2 (Bin Leaf 3 Leaf)
Start = test b
