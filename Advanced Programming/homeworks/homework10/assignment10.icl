module assignment10

//derive class iTask Val, Expression, Op, BM, Element, Set

/*
	This code is very similiar to the one I made in assignment 8. In previous code Set and Element
	were both Expression while now they are seperable because we have to ensure static type
	safety. Furthermore, store and read functions are now created as part of the classes and then
	special instances for Elements and Sets are made. I have also created OperE and OperS inside of
	Expression so I can use +. and -. for Elements and Sets.
	State is changed as well, this time I used tuple so I can save both Elements and Sets in maps. I
	think this approach is cumbersome but I didn't think of more elegant solution.
	
	I wasn't able to make iTask simulator work. I got an overloading error considering bm. I am not
	sure if it is possible to run iTask in this kind of GADT representation but I think not because
	of the bimap in Expression data structure.
*/

from iTasks import always, hasValue, :: TaskValue(..), :: Task, :: Stability, :: TaskCont(..), :: Action, updateInformation, 
		viewInformation, class descr, instance descr String, :: UpdateOption, :: ViewOption(..), -||-, -||, ||-, startEngine, 
		class Publishable, >>*, class TFunctor, instance TFunctor Task, class TApplicative, instance TApplicative Task, 
		instance Publishable Task, Void
import qualified iTasks
import Data.Tuple, StdClass, StdList, StdMaybe, StdString, iTasks._Framework.Generic, Data.Functor, Control.Applicative, Control.Monad
from StdFunc import o
import qualified Data.List as List
import qualified Data.Map as Map

:: BM a b = {t :: a -> b, f :: b -> a}

bm :: BM a a
bm = {f = id, t = id}

id :: a -> a
id x = x

fst :: (a, b) -> a
fst (x, y) = x

snd :: (a, b) -> b
snd (x, y) = y

:: Expression a
	= New			(BM a Set)
	| Insert		(BM a Set) (Expression Element) (Expression Set)
	| Delete		(BM a Set) (Expression Element) (Expression Set)
	| Variable		Ident & read a
	| Intersection	(BM a Set) (Expression Set) (Expression Set)
	| Integer		(BM a Element) Int
	| Size			(BM a Element) (Expression Set)
	| OperE			(BM a Element) (Expression Element) Op (Expression Element)
	| OperS			(BM a Set) (Expression Set) Op (Expression Set)
	| (=.) infixl 2 Ident (Expression a) & store a
	
new = New bm
insert = Insert bm
delete = Delete bm
var = Variable
intersection = Intersection bm
int = Integer bm
size = Size bm
operS = OperS bm
operE = OperE bm

:: Op 		=	+. | -. | *.
:: Set 		= S [Int]
:: Element	= I Int
:: Ident	:== String

:: Val		= Element | Set
:: Result a = Succ (a, State) | Error String
:: State 	:== (Map Ident Element, Map Ident Set)
:: Sem a	=  Sem (State -> Result a)

instance Functor Sem where
	fmap :: (a -> b) (Sem a) -> Sem b
	fmap f (Sem g) = Sem func
	where
		func = \state.case g state of
					Succ (x, ns) = Succ (f x, ns)
					Error msg = Error msg
					
instance Applicative Sem where
	pure :: a -> Sem a
	pure x = Sem (\s.Succ(x, s))
	(<*>) infixl 4 :: (Sem (a -> b)) (Sem a) -> Sem b
	(<*>) (Sem f) (Sem g) = Sem func
	where
		func = \s.case f s of
				Succ (x, ns) = case g ns of
								Succ (y, nns) = Succ (x y, nns)
								Error msg = Error msg
				Error msg = Error msg
				
instance Monad Sem where
	bind :: (Sem a) (a -> Sem b) -> Sem b
	bind (Sem f) g = Sem func
	where
		func = \s.case f s of
				Succ (x, ns) = let (Sem h) = g x
								in h ns

class store a where store :: Ident a -> Sem a

instance store Element where
	store i v = Sem \s.Succ(v, ('Map'.put i v (fst s), snd s))
	
instance store Set where
	store i v = Sem \s.Succ(v, (fst s, 'Map'.put i v (snd s)))
	
class read a where read :: Ident -> Sem a

instance read Element where
	read i = Sem \s.case 'Map'.get i (fst s) of
					Just x 	= Succ (x, s)
					_ 		= Error ("Undeclared variable: " +++ i)
					
instance read Set where
	read i = Sem \s.case 'Map'.get i (snd s) of
					Just x 	= Succ (x, s)
					_ 		= Error ("Undeclared variable: " +++ i)

fail :: String -> Sem a
fail msg = Sem \s.Error msg

eval :: (Expression a) -> Sem a
eval (New bm) = Sem \s.Succ (bm.f (S []), s)
eval (Insert bm e s) = eval e >>= \x.eval s >>= \y.let (I a) = x
													   (S b) = y in return (bm.f (S [a:b]))
eval (Delete bm e s) = eval e >>= \x.case x of
									(I a) = eval s >>= \y.case y of
														(S b) = return (bm.f (S (removeMember a b)))
														_ = fail "Set required"
									_ = fail "Element required"
eval (Variable i) = read i
eval (Intersection bm s1 s2) = eval s1 >>= \x.case x of
											(S a) = eval s2 >>= \y.case y of
																	(S b) = return (bm.f (S ('List'.intersect a b)))
																	_ = fail "Set required"
											_ = fail "Set required"
eval (Integer bm i) = return (bm.f (I i))
eval (Size bm s) = eval s >>= \x.case x of
								(S a) = return (bm.f (I (length a)))
								_ = fail "Set required"
eval (OperE bm e1 +. e2) = eval e1 >>= \x.case x of
									(I a) = eval e2 >>= \y.case y of
															(I b) = return (bm.f (I (a + b)))
															_ = fail "Element required"
									_ = fail "bla bla"
eval (OperE bm e1 -. e2) = eval e1 >>= \x.case x of
									(I a) = eval e2 >>= \y.case y of
															(I b) = return (bm.f (I (a - b)))
															_ = fail "Element required"
									_ = fail "bla bla"
eval (OperE bm e1 *. e2) = eval e1 >>= \x.case x of
									(I a) = eval e2 >>= \y.case y of
															(I b) = return (bm.f (I (a * b)))
															_ = fail "Element required"
									_ = fail "Element required"
eval (OperS bm e1 +. e2) = eval e1 >>= \x.case x of
									(S a) = eval e2 >>= \y.case y of
															(S b) = return (bm.f (S ('List'.union a b)))
															_ = fail "Element required"
									_ = fail "bla bla"
eval (OperS bm e1 -. e2) = eval e1 >>= \x.case x of
									(S a) = eval e2 >>= \y.case y of
															(S b) = return (bm.f (S ('List'.difference a b)))
															_ = fail "Element required"
									_ = fail "bla bla"
eval (i =. e) = eval e >>= \x.store i x

evalExpr :: (Expression a) State -> Result a
evalExpr e state = res
    where
        res
         # (Sem f) = eval e
		 = f state

print :: (Expression a) -> String
print e = concateStr (show e [])

concateStr :: [String] -> String
concateStr [x:xs] = x +++ concateStr xs
concateStr [] = ""

show :: (Expression a) [String] -> [String]
show (New _) c = ["(New)": c]
show (Insert _ e s) c = ["(Insert ": show e (show s [")":c])]
show (Delete _ e s) c = ["(Delete ": show e (show s [")":c])]
show (Variable i) c = ["(Var " +++ i +++ ")": c]
show (Intersection _ s1 s2) c = ["(Intersection ": show s1 (show s2 [")":c])]
show (Integer _ i) c = ["(Int " +++ toString i +++ ")": c]	
show (Size _ s) c = ["(Size ": show s [")":c]]
show (OperE _ e1 +. e2) c = show e1 [" + ": show e2 c]
show (OperE _ e1 -. e2) c = show e1 [" - ": show e2 c]
show (OperE _ e1 *. e2) c = show e1 [" * ": show e2 c]
show (OperS _ e1 +. e2) c = show e1 [" + ": show e2 c]
show (OperS _ e1 -. e2) c = show e1 [" - ": show e2 c]
show (OperS _ e1 *. e2) c = show e1 [" * ": show e2 c]
show (i =. e) c = [i +++ " = ": show e c]

// === simulation
(>>>=)     :== 'iTasks'.tbind
(>>>|) a b :== 'iTasks'.tbind a (\_ -> b)
treturn    :== 'iTasks'.return
ActionOk   :== 'iTasks'.ActionOk
ActionQuit :== 'iTasks'.ActionQuit
ActionNew  :== 'iTasks'.ActionNew

/*
simulate :: Expression State String -> Task Expression
simulate e state res = 	(viewInformation "State:" [] state 
						||- (viewInformation "Result:" [] res 
							||- (viewInformation "Expression:" [] (print e) 
								||- updateInformation "Current statement:" [] e)))
               >>* [OnAction ActionOk (hasValue (\e -> case evalExpr e state of Succ (x, newstate) = simulate e newstate (toString x); Error msg = simulate e state ("Error: " +++ msg)))]

*/
instance toString Element where
    toString (I x) = "Int " +++ toString x

instance toString Set where
    toString (S xs) = "[" +++ convert xs +++ "]"

convert :: [Int] -> String
convert [] = ""
convert [x:xs] = toString x +++ " " +++ convert xs

//Start :: *World -> *World
//Start world = startEngine (simulate New ('Map'.fromList []) "") world

e1 :: Expression Element
e1 = size (insert (int 5) (insert (operE (int 3) +. (int 5)) new))

e2 :: Expression Set
e2 = operS (insert (int 1) (insert (int 2) new)) +. (insert (int 3) (insert (int 4) new))

/*
e2 :: Expression Element
e2 = size (int 2)
Compile time error example
*/

Start = evalExpr e2 ('Map'.newMap, 'Map'.newMap)