module Chapter8 where
import Data.List hiding (find)
import Prelude 

data Nat = Zero | Succ Nat deriving (Show, Eq)


add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ x) y = Succ (add x y)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ n) m = add m (mult n m)


int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n


data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show)

occurs :: (Ord a) => a -> Tree a -> Bool
occurs v (Leaf l) = v == l
occurs v (Node l a r) = case compare v a of 
                             LT -> occurs v l 
                             EQ -> True
                             GT -> occurs v r 

data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving (Eq, Show)


leaves :: Tree2 a -> Int
leaves (Leaf2 _) = 1
leaves (Node2 l r ) = leaves l + leaves r

balanced :: Tree2 a -> Bool
balanced (Leaf2 a) = True
balanced (Node2 l r) = (abs ( leaves l  - leaves r ) <= 1)
                       && balanced l 
                       && balanced r  



balance :: [a] -> Tree2 a
balance [] = error "Cannot create empty tree"
balance [x] = Leaf2 x
balance xs = Node2 (balance (take mid xs))  (balance ( drop mid xs) )
           where s = length xs
                 mid = s `div` 2
                  





type Assoc k v = [(k,v)]

type Subst = Assoc Char Bool

data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Or Prop Prop
            | Imply Prop Prop
            | Equivalence Prop Prop



find :: Eq k => k -> Assoc k v -> v
find k xs = head [ v | (k', v) <- xs , k' == k ]

p1 :: Prop 
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Not (Var 'A'))  -- A || (not) A -> is always true

p6 :: Prop
p6 = Equivalence (Var 'A') (Var 'A')  -- A <=> A   -> is always true

p7 :: Prop
p7 = Equivalence (Or (Var 'A') (Var 'B')) (Or (Var 'B') (Var 'A'))  -- A OR B <=> A OR B -> is always true



eval_0 :: Subst -> Prop  -> Bool
eval_0 _ (Const b)      = b
eval_0 xs (Var c)       =  find c xs
eval_0 s  (Not p)       = not (eval_0 s p)
eval_0 s  (And p1 p2)   = (eval_0 s p1) && (eval_0 s p2)
eval_0 s  (Or p1 p2)    = (eval_0 s p1) || (eval_0 s p2)
eval_0 s  (Imply p1 p2) = (not a) || b
                        where a = (eval_0 s p1) 
                              b = (eval_0 s p2)  
eval_0 s  (Equivalence p1 p2) = (eval_0 s (Imply p1 p2)) && (eval_0 s (Imply p2 p1)) 


bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (True : ) xs ++ map (False : ) xs
          where xs = bools (n - 1)


vars :: Prop -> [Char]
vars (Const _)           = []
vars (Var c)             = [c]
vars (Not p)             = vars p
vars (And p1 p2)         = vars p1 ++ vars p2
vars (Or p1 p2)          = vars p1 ++ vars p2
vars (Imply p1 p2)       = vars p1 ++ vars p2
vars (Equivalence p1 p2) = vars p1 ++ vars p2

substs :: Prop -> [Subst]
substs p = [ zip vs bs | bs <- bools t ] 
          where vs = nub (vars p)
                t  = length vs


isTaut :: Prop -> Bool
isTaut p = and [ eval_0 s p | s <- substs p]



-----------

data Expr = Val Int | Add Expr Expr | Mult Expr Expr 

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val i)    = f i 
folde f g (Add a b)  = g (folde f g a) (folde f g b)


eval_1 :: Expr -> Int
eval_1  = folde (id) (+)


size :: Expr -> Int
size  = folde (\x -> 1) (+)

type Control = [Op]

data Op = EVAL_ADD Expr | EVAL_MULT Expr | ADD Int | MULT Int



eval :: Expr -> Control -> Int
eval (Val n)    c = exec c n
eval (Add x y)  c = eval x (EVAL_ADD y : c)
eval (Mult x y) c = eval x (EVAL_MULT y : c)


exec :: Control -> Int -> Int
exec [] n = n 
exec (EVAL_ADD a:as) n = eval a ( (ADD n) : as)  
exec (EVAL_MULT a:as) n = eval a ( (MULT n) : as)  
exec (ADD a:as) n = exec as (a + n) 
exec (MULT a:as) n = exec as (a * n)

value2 :: Expr -> Int
value2 e = eval e []   


-- Sublime hates block comments!
-- instance Eq a => Eq (Maybe a) where
--	Nothing  == Nothing  = True
--	(Just x) == (Just y) = x == y
--	_        == _        = False 


-- instance Eq a => Eq [a] where
--	[]     == []  = True
--	(x:xs) == (y:ys) = (x == y ) && ( xs == ys  ) 
--	_      == _    = False 
--

value :: Expr -> Int
value (Val i)    = i 
value (Add a b)  = (+) (value a) (value b) 
value (Mult a b) = (*) (value a) (value b)