module skeleton8

derive class iTask Val, Expression, Op

/*
	Advanved Progrmming 2016, Assignment 8
	Pieter Koopman, pieter@cs.ru.nl
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

:: Expression
	= New
	| Insert		Element Set
	| Delete		Element Set
	| Variable		Ident
	| Intersection	Set 	Set
	| Integer		Int
	| Size			Set
	| Oper			Expression Op Expression
	| (=.) infixl 2 Ident Expression

:: Op 		=	+. | -. | *.
:: Set 		:== Expression
:: Element	:== Expression
:: Ident	:== String

// === State

:: Val		= I Int | S [Int]
:: Result a 	= Succ (a, State) | Error String
:: State 	:== Map Ident Val
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

														
return :: Val -> Sem Val
return v = Sem \s.Succ (v, s)

store :: Ident Val -> Sem Val
store i v = Sem \s.Succ (v, 'Map'.put i v s)

read :: Ident -> Sem Val
read i = Sem \s.case 'Map'.get i s of
				Just x = Succ (x, s)
				_	   = Error ("Undeclared variable: " +++ i)

fail :: String -> Sem a
fail msg = Sem \s.Error msg

eval :: Expression -> Sem Val
eval New = Sem \s.Succ (S [], s)
eval (Insert e s) = eval e >>= \x.case x of
									(I a) = eval s >>= \y.case y of
														(I b) = fail "Set required"
														(S b) = return (S [a:b])
									(S a) = fail "Element required"
eval (Delete e s) = eval e >>= \x.case x of
									(I a) = eval s >>= \y.case y of
														(I b) = fail "Set required"
														(S b) = return (S (removeMember a b))
									(S a) = fail "Element required"
eval (Variable i) = read i
eval (Intersection s1 s2) = eval s1 >>= \x.case x of
											(I a) = fail "Set required"
											(S a) = eval s2 >>= \y.case y of
																	(I b) = fail "Set required"
																	(S b) = return (S ('List'.intersect a b))
eval (Integer i) = return (I i)
eval (Size s) = eval s >>= \x.case x of
								(I a) = fail "Set required"
								(S a) = return (I (length a))
eval (Oper e1 +. e2) = eval e1 >>= \x.case x of
									(I a) = eval e2 >>= \y.case y of
															(I b) = return (I (a + b))
															(S b) = fail "Element required"
									(S a) = eval e2 >>= \y.case y of
															(I b) = fail "Set required"
															(S b) = return (S ('List'.union a b))
eval (Oper e1 -. e2) = eval e1 >>= \x.case x of
									(I a) = eval e2 >>= \y.case y of
															(I b) = return (I (a - b))
															(S b) = fail "Element required"
									(S a) = eval e2 >>= \y.case y of
															(I b) = fail "Set required"
															(S b) = return (S ('List'.difference a b))
eval (Oper e1 *. e2) = eval e1 >>= \x.case x of
									(I a) = eval e2 >>= \y.case y of
															(I b) = return (I (a * b))
															(S b) = fail "Element required"
									(S a) = fail "Element required"
eval (i =. e) = eval e >>= \x.store i x

evalExpr :: Expression State -> Result Val
evalExpr e state = res
    where
        res
         # (Sem f) = eval e
		 = f state

print :: Expression -> String
print e = concateStr (show e [])

concateStr :: [String] -> String
concateStr [x:xs] = x +++ concateStr xs
concateStr [] = ""

show :: Expression [String] -> [String]
show New c = ["(New)": c]
show (Insert e s) c = ["(Insert ": show e (show s [")":c])]
show (Delete e s) c = ["(Delete ": show e (show s [")":c])]
show (Variable i) c = ["(Var " +++ i +++ ")": c]
show (Intersection s1 s2) c = ["(Intersection ": show s1 (show s2 [")":c])]
show (Integer i) c = ["(Int " +++ toString i +++ ")": c]	
show (Size s) c = ["(Size ": show s [")":c]]
show (Oper e1 +. e2) c = show e1 [" + ": show e2 c]
show (Oper e1 -. e2) c = show e1 [" - ": show e2 c]
show (Oper e1 *. e2) c = show e1 [" * ": show e2 c]
show (i =. e) c = [i +++ " = ": show e c]

// === simulation
(>>>=)     :== 'iTasks'.tbind
(>>>|) a b :== 'iTasks'.tbind a (\_ -> b)
treturn    :== 'iTasks'.return
ActionOk   :== 'iTasks'.ActionOk
ActionQuit :== 'iTasks'.ActionQuit
ActionNew  :== 'iTasks'.ActionNew

simulate :: Expression State String -> Task Expression
simulate e state res = 	(viewInformation "State:" [] state 
						||- (viewInformation "Result:" [] res 
							||- (viewInformation "Expression:" [] (print e) 
								||- updateInformation "Current statement:" [] e)))
               >>* [OnAction ActionOk (hasValue (\e -> case evalExpr e state of Succ (x, newstate) = simulate e newstate (toString x); Error msg = simulate e state ("Error: " +++ msg)))]

instance toString Val where
    toString (I x) = "Int " +++ toString x
    toString (S xs) = "[" +++ convert xs +++ "]"

convert :: [Int] -> String
convert [] = ""
convert [x:xs] = toString x +++ " " +++ convert xs

Start :: *World -> *World
Start world = startEngine (simulate New ('Map'.fromList []) "") world
