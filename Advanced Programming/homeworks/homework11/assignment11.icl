module assignment11

from iTasks import always, hasValue, :: TaskValue(..), :: Task, :: Stability, :: TaskCont(..), :: Action, updateInformation, 
		viewInformation, class descr, instance descr String, :: UpdateOption, :: ViewOption(..), -||-, -||, ||-, startEngine, 
		class Publishable, >>*, class TFunctor, instance TFunctor Task, class TApplicative, instance TApplicative Task, 
		instance Publishable Task, Void
import qualified iTasks
import Data.Tuple, StdClass, StdList, StdMaybe, StdString, iTasks._Framework.Generic, Data.Functor, Control.Applicative, Control.Monad
from StdFunc import o
import qualified Data.List as List
import qualified Data.Map as Map

// DSL

class aexpr v where
	lit :: a -> v a Expr | type a
	(+.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, + t
	(-.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, - t
	(*.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, * t
	
class bexpr v where
	(&.) infixr 3 	:: (v Bool p) (v Bool q) -> v Bool Expr
	(|.) infixr 2 	:: (v Bool p) (v Bool q) -> v Bool Expr
	(~.)			:: (v Bool p) -> v Bool Expr
	(<.) infix 4	:: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	
class sexpr v where
	new 	:: v t Expr | type t
	union 	:: (v t p) (v t q) -> v t Expr | type t
	diff	:: (v t p) (v t q) -> v t Expr | type t
	ins		:: (v a p) (v t q) -> v t Expr | type t & type a
	del		:: (v a p) (v t q) -> v t Expr | type t & type a
	inters	:: (v t p) (v t q) -> v t Expr | type t
	
class stmt v where
	If :: (v Bool p) (v a q) Else (v b r) -> v () Stmt | isExpr p
	While :: (v Bool p) (v a q) -> v () Stmt
	(:.) infixr 1 :: (v a p) (v b q) -> v b Stmt
	(=.) infixr 2 :: (v t Upd) (v t p) -> v t Expr | type t & isExpr p
	var	:: ((v t Upd) -> In t (v a p)) -> (v a p) | type t
	
:: In a b = In infix 0 a b
:: Upd = Upd
:: Expr = Expr
:: Stmt = Stmt
:: Else = Else

class isStmt a :: a -> a
instance isStmt Stmt where isStmt a = a
instance isStmt Expr where isStmt a = a
instance isStmt Upd where isStmt a = a

class isExpr a :: a -> a
instance isExpr Expr where isExpr a = a
instance isExpr Upd where isExpr a = a

class type a | toString, TC a where
	type :: a -> String
instance type Int where type a = "int"
instance type Bool where type a = "bool"
instance type Char where type a = "char"
instance type [a] where type a = "set"

// show view

:: Show a p = Show (SHOW -> SHOW)
:: SHOW =
	{ fresh :: Int
	, indent :: Int
	, print :: [String]
	}
s0 :: SHOW
s0 =
	{ fresh = 0
	, indent = 0
	, print = ["\n"]
	}
	
show :: (Show a p) -> [String]
show (Show f) = reverse (f s0).print

c :: a -> Show b c | toString a
c a = Show \c.{c & print = [toString a:c.print]}

(+.+) infixl 5 :: (Show a p) (Show b q) -> Show c r
(+.+) (Show f) (Show g) = Show (g o f)

fresh :: (Int -> (Show a p)) -> (Show a p)
fresh f = Show \c.unShow (f c.fresh) {c & fresh = inc c.fresh}

unShow :: (Show a p) SHOW -> SHOW
unShow (Show f) s = f s

freshVar :: ((Show b q) -> (Show a p)) -> (Show a p)
freshVar f = fresh (f o \n.c ("v"+++toString n))

indent :: Show a b
indent = Show \c.{c & indent = inc c.indent}

unindent :: Show a b
unindent = Show \c.{c & indent = dec c.indent}

nl :: Show a b
nl = Show \c.{c & print = [toString ['\n':repeatn (2 * c.indent) ' ']:c.print]}

// show expressions
instance aexpr Show where
	lit a = c a
	(+.) x y = brac (x +.+ c "+" +.+ y)
	(-.) x y = brac (x +.+ c "-" +.+ y)
	(*.) x y = brac (x +.+ c "*" +.+ y)
	
instance bexpr Show where
	(&.) x y = brac (x +.+ c "&" +.+ y)
	(|.) x y = brac (x +.+ c "|" +.+ y)
	(~.) x = brac (c "~" +.+ x)
	(<.) x y = brac (x +.+ c "<" +.+ y)
	
(>.) x y :== y <. x
	
instance sexpr Show where
	new			= c "new"
	union x y 	= brac (c "union" +.+ x +.+ y)
	diff x y 	= brac (c "difference" +.+ x +.+ y)
	ins x y		= brac (c "insert" +.+ x +.+ y)
	del x y 	= brac (c "delete" +.+ x +.+ y)
	inters x y	= brac (c "instersection" +.+ x +.+ y)
	
brac :: (Show a p) -> Show b q
brac e = c "(" +.+ e +.+ c ")"

instance stmt Show where
	(:.) s t = s +.+ c ";" +.+ nl +.+ t +.+ nl
	While b s = 
		c "while " +.+ b +.+ c " {" +.+ indent +.+ nl
		+.+ s +.+ unindent +.+ nl +.+ c "}" +.+ nl
	If b t Else e =
		c "if " +.+ b +.+ c " {" +.+ indent +.+ nl +.+
		t +.+ unindent +.+ nl +.+ c "} else {" +.+
		indent +.+ nl +.+ e +.+ unindent +.+ nl +.+
		c "}" +.+ nl
	(=.) v e = v +.+ c " = " +.+ e
	var f =
		freshVar \v.let (x In rest) = f v in
			c (type x) +.+ c " " +.+ v +.+ c " = " +.+
			c x +.+ c ";" +.+ nl +.+ rest
			
// evaluation

:: Eval a p = Eval ((RW a) State -> (MB a, State))
:: RW a = R | W a
:: MB a = Jst a | Err String
:: State =
	{ map :: Map Int Dynamic
	, vars :: Int
	}
e0 :: State
e0 =
	{ map = 'Map'.newMap
	, vars = 0
	}
	
rtrn :: a -> Eval a p
rtrn a = Eval \r s.(Jst a, s)

(>>-) infixl 1 :: (Eval a p) (a -> Eval b q) -> Eval b q
(>>-) (Eval e) f =
	Eval \r s.case e R s of
		(Jst a,t) = unEval (f a) r t
		(Err e,t) = (Err e,t)
		
(<*.>) infixl 4 :: (Eval (a -> b) p) (Eval a q) -> Eval b r
(<*.>) f a = f >>- \g.a >>- \b.rtrn (g b)

unEval :: (Eval a p) (RW a) State -> (MB a, State)
unEval (Eval f) r s = f r s

rwvar :: Int (RW a) State -> (MB a, State) | TC a
rwvar n R s =
	case 'Map'.get n s.map of
		(Just (v :: a^)) = (Jst v,s)
		(Just d) 		= (Err (toString n+++" wrong type"), s)
		_				= (Err (toString n+++" undefined"), s)
rwvar n (W a) s = (Jst a, {s & map = 'Map'.put n (dynamic a) s.map})

instance aexpr Eval where
	lit a		= rtrn a
	(+.) x y 	= rtrn (+) <*.> x <*.> y
	(-.) x y 	= rtrn (-) <*.> x <*.> y
	(*.) x y 	= rtrn (*) <*.> x <*.> y
	
instance bexpr Eval where
	(&.) x y = rtrn (&&) <*.> x <*.> y
	(|.) x y = rtrn (||) <*.> x <*.> y
	(<.) x y = rtrn (<) <*.> x <*.> y
	(~.) x	 = rtrn (~~) <*.> x
	
/*
Unfinished segment ...
instance sexpr Eval where
	new			= 
	union x y 	=
	diff x y 	= 
	ins x y		= 
	del x y 	= 
	inters x y	= 
*/
	
(&&) infix :: Bool Bool -> Bool
(&&) True True = True
(&&) _ _ = False

(||) infix :: Bool Bool -> Bool
(||) False False = False
(||) _ _ = True

(~~) infix :: Bool -> Bool
(~~) True = False
(~~) False = True
	
instance stmt Eval where
	(:.) s t = s >>- \_.toStmt t
	If b t else e
		= b >>- \c.if c (t >>- \_.rtrn ())
						(e >>- \_.rtrn())
	While b s
		= b >>- \c.if c (s :. While b s) (rtrn ())
	(=.) v e = e >>- \a.Eval \r s.unEval v (W a) s
	var f
		= Eval \r s.
			let (x In (Eval rest)) = f (Eval (rwvar s.vars))
			in rest R {s & vars = inc s.vars
						, map = 'Map'.put s.vars (dynamic x) s.map
						}
						
eval :: (Eval a p) -> [String] | type a
eval (Eval f) = case fst (f R e0) of
	Jst a = [toString a, "\n"]
	Err e = ["Error: ", e, "\n"]
	
fst :: (a, b) -> a
fst (x, y) = x

toStmt :: (Eval t p) -> Eval t Stmt
toStmt (Eval f) = Eval f

fac = 
	var \n -> 4 In
	var \r -> 1 In
	While (n >. lit 1) (
		r =. r *. n :.
		n =. n -. lit 1
	) :.
	r
	
test1 = (~.) (lit False)

test2 = If ((lit 1) <. (lit 2)) (lit 1) Else (lit 2)

test3 = var \n -> 'c' In n

test4 = var \n -> [1,2] In n

test5 = new

test6 = union (lit [1,2]) (lit [3,4])

test7 = (lit 5) +. (lit 37)

//Start = eval test7
Start = show fac