implementation module gastjeStart

/*
	Pieter Koopman, Radboud University, 2016
	pieter@cs.ru.nl
	Advanced programming
	A simplified MBT tool based on logical properties
	
	Use the iTask environment!
*/

import StdEnv, StdGeneric, GenEq

:: T a b = T ((a -> b), [a])
:: V a = V (a, a)

//Start = holds (pUpper For ['a'..'z']) prop0
//Start = holds (\c -> isLower c ==> pUpper c) prop0
Start = holds pfac prop0

pfac i = abs i < 10 ==> prod[1..i] =.= fac i

fac :: Int -> Int
fac i = prod[1..i]

property :: Char -> (Bool, Bool)
property c = ((isLower c), (pUpper c))

pUpper :: Char -> Bool
pUpper c = c <> toUpper c

fun :: Int -> Bool
fun i = False

test :: p -> [String] | prop p
test p = check 1000 (holds p prop0)

check :: Int [Prop] -> [String]
check n [] = ["Proof\n"]
check 0 l  = ["Passed\n"]
check n [p:x] | p.bool
	= check (n-1) x
	= ["Fail for: ":reverse ["\n":p.info]]

class prop a where holds :: a Prop -> [Prop]

instance prop Bool where holds b p = [{p & bool = b}]

// crashes the iTask compiler if there is no dcl module :(
instance prop (a->b) | prop b & testArg a 
where
	holds f p = diagonal [holds (f a) {p & info = [" ",string{|*|} a:p.info]} \\ a <- gen{|*|}]

instance prop (T a b) | prop b & testArg a
where
	holds (T (f, l)) p = diagonal [holds (f a) {p & info = [" ",string{|*|} a:p.info]} \\ a <- l]

instance prop (Bool, b) | prop b
where
	holds (a, b) p = if a (holds b p) (holds b {p & info = ["Rejected": p.info]})
	
instance prop (V a) | testArg a
where
	holds (V (a,b)) p = holds True {p & info = [string{|*|} a +++ " == " +++ string{|*|} b: p.info]}

(For) infixr 1 :: (a -> b) [a] -> T a b
(For) f l = T (f, l)

(==>) infixr 1:: Bool a -> (Bool, a)
(==>) b1 b2 = (b1, b2)

(=.=) infixr 1 :: a a -> V a
(=.=) l r = V (l, r)

class testArg a | gen{|*|}, string{|*|}, gEq{|*|} a 

:: Prop =
	{ bool :: Bool
	, info :: [String]
	}
prop0 = {bool = True, info = []}

generic gen a :: [ a ]
gen{|Int|}  = [1..12]//[0,1,-1,maxint,minint,maxint-1,minint+1:[j\\i<-[2..], j<-[i,~i]]]
gen{|Bool|} = [True,False]
gen{|Char|} = [' '..'~'] ++ ['\t\n\b']
gen{|UNIT|} = [UNIT]
gen{|PAIR|}   f g	= map (\(a,b)=PAIR a b) (diag2 f g)
gen{|EITHER|} f g = merge (map RIGHT g) (map LEFT f)
where
  merge [a:x] ys = [a: merge ys x]
  merge []    ys = ys
gen{|CONS|}   f  = map CONS f
gen{|OBJECT|} f  = map OBJECT f
gen{|RECORD|} f  = map RECORD f
gen{|FIELD|}  f  = map FIELD f

generic string a :: a -> String
string{|Int|} i = toString i
string{|Bool|} b = toString b
string{|Char|} c = toString ['\'',c,'\'']
string{|UNIT|} _ = ""
string{|PAIR|} f g (PAIR x y) = f x + " " + g y
string{|EITHER|} f g (LEFT x) = f x
string{|EITHER|} f g (RIGHT y) = g y
string{|CONS of gcd|} f (CONS x) | gcd.gcd_arity > 0
	= "(" + gcd.gcd_name + " " + f x + ")"
	= gcd.gcd_name
string{|OBJECT|} f (OBJECT x) = f x
string{|RECORD of grd|} f (RECORD x) = "{" + grd.grd_name + "|" + f x + "}"
string{|FIELD of gfd|} f (FIELD x) = gfd.gfd_name + " = " + f x + " "

maxint :: Int
maxint =: IF_INT_64_OR_32 (2^63-1) (2^31-1) //2147483647

minint :: Int
minint =: IF_INT_64_OR_32 (2^63) (2^31) //-2147483648

instance + String where + s t = s +++ t

diagonal :: [[a]] -> [a]
diagonal list = f 1 2 list []
where
	f n m [] [] = []
	f 0 m xs ys = f m (m+1) (rev ys xs) []
	f n m [] ys = f m (m+1) (rev ys []) []
	f n m [[x:r]:xs] ys = [x: f (n-1) m xs [r:ys]]
	f n m [[]:xs] ys = f (n-1) m xs ys
	
	rev []    accu = accu
	rev [x:r] accu = rev r [x:accu]

