module serialize2start

/*
	Definition for assignment 2 in AFP 2016
	Pieter Koopman pieter@cs.ru.nl
	September 2016
*/

import StdEnv, StdMaybe

class serialize a where
	write :: a [String] -> [String]
	read  :: [String] -> Maybe (a,[String])

instance serialize Bool where
	write b c = [toString b:c]
	read ["True":r]  = Just (True,r)
	read ["False":r] = Just (False,r)
	read _ = Nothing

instance serialize Int where
	write i c = [toString i:c]
	read [s:r]
		# i = toInt s
		| s == toString i
			= Just (i,r)
			= Nothing
	read _ = Nothing

:: UNIT       = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR   a b = PAIR a b
:: CONS   a   = CONS String a

:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))

instance serialize [a] | serialize a where  // to be improved
	write l c = write (fromList l) c
	read l = case read l of
		Just(g,m) = Just (toList g,m)
		_ = Nothing

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

instance serialize (Bin a) | serialize a where // to be improved
	write a c = write (fromBin a) c
	read l = case read l of
		Just (a,m) = Just (toBin a,m)
		_ = Nothing

instance == (Bin a) | == a where // better use the generic approach
	(==) x y = fromBin x == fromBin y
	(==) _ _ = False

//*****************************************************************

instance serialize UNIT where
	write UNIT c = c
	read l = Just (UNIT, l)

instance serialize (EITHER a b) | serialize a & serialize b where
	write (LEFT a) c = write a c
	write (RIGHT b) c = write b c
	read l = case read l of
		Just (a,m) = Just (LEFT a,m)
		_ = case read l of
			Just (b,m) = Just (RIGHT b,m)
			_ = Nothing

instance serialize (PAIR a b) | serialize a & serialize b where
	write (PAIR a b) c = write a (write b c)
	read l = case read l of
		Just (a,m) = case read m of
			Just (b,n) = Just (PAIR a b,n)
			_ = Nothing
		_ = Nothing

instance serialize (CONS a) | serialize a where
	write (CONS s a) c = ["(",s:write a [")":c]]
	read ["(",s:l] = case read l of
		Just(a,[")":m]) = Just (CONS s a,m)
		_ = Nothing
	read _ = Nothing

instance == UNIT where
	(==) UNIT UNIT = True
instance == (EITHER a b) | == a & == b where
	(==)(LEFT x) (LEFT y) = x == y
	(==)(RIGHT x) (RIGHT y) = x == y
	(==)_ _ = False
instance == (PAIR a b) | == a & == b where
	(==)(PAIR x1 x2)(PAIR y1 y2) = x1==y1 && x2 ==y2
instance == (CONS a) | == a where
	(==)(CONS _ x1)(CONS _ y1) = x1 == y1

fromList :: [a] -> ListG a
fromList [] = LEFT (CONS "Nil" UNIT)
fromList [a:as] = RIGHT (CONS "Cons" (PAIR a as))

toList :: (ListG a) -> [a]
toList (LEFT (CONS _ UNIT)) = []
toList (RIGHT (CONS _ (PAIR a as))) = [a:as]

fromBin :: (Bin a) -> BinG a
fromBin Leaf = LEFT (CONS "Leaf" UNIT)
fromBin (Bin l a r) = RIGHT (CONS "Bin" (PAIR l (PAIR a r)))

toBin :: (BinG a) -> Bin a
toBin (LEFT (CONS _ UNIT)) = Leaf
toBin (RIGHT (CONS _ (PAIR l (PAIR a r)))) = Bin l a r

//Start = (fromList []) == (fromBin Leaf)
//Start = write [0..2] ["s"]
//Start = write (fromList [0..2]) ["A"]
//Start = write (fromBin (Bin (Bin Leaf False Leaf) True Leaf)) ["s"]
//Start = fromBin (Bin (Bin Leaf False Leaf) True Leaf)
//Start = toBin (RIGHT (CONS "Bin" (PAIR (Bin Leaf False Leaf) (PAIR True Leaf))))

Start = 
	[test True
	,test False
	,test 0
	,test 123
	,test -36
	,test [42]
	,test [0..4]
	,test [[True],[]]
	,test (Bin Leaf True Leaf)
	,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin Leaf [4,5] Leaf))]
	,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin (Bin Leaf [4,5] Leaf) [6,7] (Bin Leaf [8,9] Leaf)))]
	]

test :: a -> ([String],[String]) | serialize, == a
test a = 
	(if (isJust r)
		(if (fst jr == a)
			(if (isEmpty (tl (snd jr)))
				["Oke "]
				["Fail: not all input is consumed! ":snd jr])
			["Fail: Wrong result ":write (fst jr) []])
		["Fail: read result is Nothing "]
	, ["write produces ": s]
	)
	where
		s = write a ["\n"]
		r = read s
		jr = fromJust r





