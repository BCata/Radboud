module genericMap

import StdEnv, StdGeneric

generic gMap a b :: a -> b
gMap{|Int|}         x = x
gMap{|Real|}        x = x
gMap{|UNIT|}        x = x
gMap{|PAIR|}   f g (PAIR x y) = PAIR   (f x) (g y) 
gMap{|EITHER|} f g (LEFT x)   = LEFT   (f x)
gMap{|EITHER|} f g (RIGHT x)  = RIGHT  (g x)
gMap{|CONS|}   f   (CONS x)   = CONS   (f x)
gMap{|OBJECT|} f   (OBJECT x) = OBJECT (f x)

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
t = Bin (Bin Leaf 1 Leaf) 2 (Bin (Bin Leaf 3 Leaf) 4 Leaf)
l = [1..7]

fac :: Int -> Int 
fac 0 = 1
fac n = n * fac (n-1)

derive gMap [], Bin, (,)

//Start = gMap{|*->*|} fac l
//Start = gMap{|*->*|} fac t
//Start = gMap{|*->*->*|} (gMap{|*->*|} fac) (gMap{|*->*|} fac) (l,t)  


//************************************************************************************

:: Maybe a = Nothing | Just a
	
generic gToNothing a :: a -> a

gToNothing{|OBJECT|} fx (OBJECT x) = OBJECT (fx x)
gToNothing{|CONS|} fx (CONS x) = CONS (fx x)
gToNothing{|EITHER|} fx _ (LEFT x) = LEFT (fx x)
gToNothing{|EITHER|} _ fy (RIGHT y) = RIGHT (fy y)
gToNothing{|PAIR|} fx fy (PAIR x y) = PAIR (fx x) (fy y)
gToNothing{|UNIT|} u = u

gToNothing{|Maybe|} _ _ = Nothing
gToNothing{|Int|} i = i

derive gToNothing (,), []

foo :: (Maybe Int) -> Maybe Int
foo (Just a) = Just 2
foo Nothing = Just 3 

//Start = gToNothing{|*|} ([Just 1], [Just 5])
//Start = res

res :: (NothingCount (Maybe a))
res = gMaxNothingCount{|*->*|} (NothingCount 0)

:: NothingCount a = NothingCount Int
generic gMaxNothingCount a :: NothingCount a
gMaxNothingCount{|OBJECT|} (NothingCount c) = NothingCount c
gMaxNothingCount{|CONS|} (NothingCount c) = NothingCount c
gMaxNothingCount{|EITHER|} (NothingCount c)  (NothingCount d) = NothingCount(max c d)
gMaxNothingCount{|PAIR|} (NothingCount c)  (NothingCount d) = NothingCount (c+d)
gMaxNothingCount{|UNIT|} = NothingCount 0
gMaxNothingCount{|Int|} = NothingCount 0
gMaxNothingCount{|Bool|} = NothingCount 0
gMaxNothingCount{|Maybe|} (NothingCount c) = NothingCount (max 1 c)

//************************************************************************************

generic gJustCount a :: a -> Int
gJustCount{|UNIT|} u = 0
gJustCount{|EITHER|} f1 f2 (LEFT a) = f1 a
gJustCount{|EITHER|} f1 f2 (RIGHT b) = f2 b
gJustCount{|PAIR|} f1 f2 (PAIR a b) = (f1 a) + (f2 b)
gJustCount{|OBJECT|} f1 (OBJECT a) = f1 a
gJustCount{|CONS|} f1 (CONS a) = f1 a
gJustCount{|Maybe|} f1 Nothing = 0
gJustCount{|Maybe|} f1 (Just a) = 1 + f1 a

gJustCount{|Int|} _ = 0
gJustCount{|String|} _ = 0
gJustCount{|Bool|} _ = 0


//**************************************************************************************

derive gJustCount (,,), []

count = gJustCount{|*|} (Just 5, [Nothing, Just True, Just False], "hello")

//**************************************************************************************

p :: a -> Bool | gJustCount a
p x = gJustCount{|*|} (Just x) > 0

//**************************************************************************************

Start = count
