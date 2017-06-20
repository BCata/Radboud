module assignment9Tests

import StdCleanTypes, StdMisc, StdList, StdInt, Data.Tuple, StdClass, iTasks._Framework.Generic, Text.JSON, Data.Functor, Control.Applicative, Control.Monad, Data.Void
import StdCleanTypes, StdMisc, StdList, StdInt, Data.Tuple, StdClass, iTasks._Framework.Generic, Text.JSON, Data.Functor, Control.Applicative, Control.Monad, Data.Void
import qualified iTasks
import qualified Text
from Text import class Text, instance Text String
from StdFunc import o
from StdTuple import fst
import qualified Data.Map as Map
from Data.List import union, removeMember, instance Functor []
import qualified Data.List as List

:: Val = I Int | S [Int]
:: Ident :== String
:: Element :== Value Int
:: Set :== Value [Int]
:: State :== Map Ident Val
:: Result a = Succ (a, State) | Error String
:: Sem a = Sem (State -> Result a)
:: Value a = {sem :: Sem a, print :: [String]}
:: Variable :== String

integer :: Int -> Element
integer i = {sem = return i, print = ["(Int " +++ toString i] ++ [")"]}

variable :: Ident -> Value a | read a
variable i = {sem = read i, print = ["(Var " +++ i] ++ [")"]}

size :: Set -> Element
size set = 	{sem = set.sem >>= \xs -> return (length xs)
			, print = ["(Size ":set.print] ++ [")"]}

//read/store

class read a where 
	read :: Ident -> Sem a
	
class store a where
	store :: Ident a -> Sem a

instance read Int where
	read v =  Sem \s.case 'Map'.get v s of
				Just (I x) 	= Succ (x, s)
				Just (S x) 	= Error ("Variable " +++ v +++ " is of wrong type.")
				_		   	= Error ("Variable " +++ v +++ " not found.")

instance read [Int] where
	read v = Sem \s.case 'Map'.get v s of
				Just (S x) = Succ (x, s)
				Just (I x) 	= Error ("Variable " +++ v +++ " is of wrong type.")
				_		   	= Error ("Variable " +++ v +++ " not found.")
	

instance store Int where
	store x v = Sem \s.Succ (v, 'Map'.put x (I v) s)
	
instance store [Int] where
	store x v = Sem \s.Succ (v, 'Map'.put x (S v) s)
	
//Operations

instance + Element where
	(+) x y = 	{sem = (+) <$> x.sem <*> y.sem
				, print = ["("] ++ x.print ++ ["+"] ++ y.print ++ [")"]}
	
instance - Element where
	(-) x y = 	{sem = (-) <$> x.sem <*> y.sem
				, print = ["("] ++ x.print ++ ["-"] ++ y.print ++ [")"]}
				
instance * Element where
	(*) x y = 	{sem = (*) <$> x.sem <*> y.sem
				, print = ["("] ++ x.print ++ ["*"] ++ y.print ++ [")"]}
				
insert :: Element Set -> Set
insert elem set = 	{sem = elem.sem >>= \e -> set.sem >>= \s -> return [e:s]
					, print = ["(Insert "] ++ elem.print ++ [" "] ++ set.print ++ [")"]}
					
delete :: Element Set -> Set
delete elem set =	{sem = elem.sem >>= \e -> set.sem >>= \s -> return (removeMember e s)
					, print = ["(Delete "] ++ elem.print ++ [")"]}
					
(+.) infixl :: Set Set -> Set
(+.) set1 set2 =	{sem = set1.sem >>= \s1 -> set2.sem >>= \s2 -> return ('List'.union s1 s2)
					, print = ["(Union "] ++ set1.print ++ set2.print ++ [")"]}
					
(-.) infixl :: Set Set -> Set
(-.) set1 set2 = 	{sem = set1.sem >>= \s1 -> set2.sem >>= \s2 -> return ('List'.difference s1 s2)
					, print = ["(Union "] ++ set1.print ++ set2.print ++ [")"]}
						
intersection :: Set Set -> Set
intersection set1 set2 = {sem = set1.sem >>= \s1 -> set2.sem >>= \s2 -> return ('List'.difference s1 s2)
							, print = ["(Union "] ++ set1.print ++ set2.print ++ [")"]}
					
//Functor, Applicative, Monad

instance Functor Sem where
	fmap :: (a -> b) (Sem a) -> Sem b
	fmap f (Sem g) = Sem func
	where
		func = \s.case g s of
					Succ (x, ns) = Succ (f x, ns)
					Error msg = Error msg
					
instance Applicative Sem where
	pure :: a -> Sem a
	pure x = Sem (\s.Succ(x, s))
	(<*>) infixl 4 :: (Sem (a->b)) (Sem a) -> Sem b
	(<*>) (Sem f) (Sem g) = Sem func
	where func = \s.case f s of
					Succ (nf, ns) = case g s of
						Succ (x, nns) = Succ (nf x, nns)
						Error msg = Error msg
					Error msg = Error msg

instance Monad Sem where
	bind :: (Sem a) (a -> Sem b) -> Sem b
	bind (Sem f) g = Sem func
	where func = \s.case f s of
					Succ (x, ns) = let (Sem h) = g x
									in h ns
									
//Statements
		
(=.) infixl 2 :: Ident (Value a) -> Value a | store a
(=.) i x = 	{sem = x.sem >>= \v.store i v
			, print = [i +++ " =. "] ++ x.print}

(:.)  infixl 1 :: (Value a) (Value b) -> Value b    // sequential composition
(:.) s1 s2 = {sem = s1.sem >>= \_ -> s2.sem
		, print = s1.print ++ [";\n"] ++ s2.print}

(==.) infix  4 :: (Value a) (Value a) -> Value Bool | == a // equality
(==.) s1 s2 = 	{sem = (==) <$> s1.sem <*> s2.sem
				, print = ["("] ++ s1.print ++ [" == "] ++ s2.print ++ [")"]}

(<.)  infix  4 :: (Value a) (Value a) -> Value Bool | < a // less than
(<.) s1 s2 = 	{sem = (<) <$> s1.sem <*> s2.sem
				, print = ["("] ++ s1.print ++ [" < "] ++ s2.print ++ [")"]}

IF :: (Value Bool) THEN (Value a) ELSE (Value a) -> Value a    // conditional expression
IF cond THEN s1 ELSE s2 = 	{sem = cond.sem >>= \b -> if b s1.sem s2.sem
							,print = ["If "] ++ cond.print ++ [" then "] ++ s1.print ++ [" else "] ++ s2.print}

WHILE :: (Value Bool) DO (Value a) -> Value ()   // repetition
WHILE cond DO s = 	{sem = cond.sem >>= 
						\b -> if b (s :. WHILE cond DO s).sem (return ())
					,print = ["While "] ++ cond.print ++ [" do \n	"] ++ s.print}

:: THEN = THEN
:: ELSE = ELSE
:: DO   = DO

new :: Set
new = {sem = return [], print = ["New"]}

// examples

expr1 :: Element
expr1 = integer 2

expr2 :: Element
expr2 = expr1 + expr1

expr3 :: Element
expr3 = expr1 + expr1 * integer 3

expr4 :: Set
expr4 = insert expr3 new

expr5 :: Set
expr5 =
    x =. expr4 :.
    variable x

expr6 :: Element
expr6 =
    x =. insert (integer 11) new :.
    x =. size (variable x) :.
    variable x

expr7 :: Set
expr7 =
    x =. insert (integer 11) new :.
    y =. variable x

expr8 :: Set
expr8 =
    x =. insert (integer 11) new :.
    x =. insert (size (variable x)) (variable x) :.
    variable x

expr9 :: Set
expr9 =
    x =. insert (integer 0) new :.
    IF (size (variable x) ==. integer 0) THEN
        (x =. insert (integer 0) (variable x))
    ELSE
        (x =. delete (integer 0) (variable x)) :.
    variable x

expr10 :: Set
expr10 =
	z =. integer 7 :.
	x =. new :.
	x =. insert (variable z) (variable x) :.
	y =. (variable x) +. (variable x) :.
	WHILE (size (variable x) <. integer 5) DO
		(x =. insert (size (variable x)) (variable x)) :.
	z =. (variable x) -. (intersection (variable x) (insert (variable z) new))

exprTypeError :: Set
exprTypeError =
    x =. integer 0 :.
    variable x

x = "x"
y = "y"
z = "z"

eval :: (Value a) -> Result a
eval v = let (Sem f) = v.sem in f ('Map'.fromList [])

print :: (Value a) -> String
print v = 'Text'.concat v.print

Start = print expr10
//Start = eval expr10