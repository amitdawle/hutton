module Chapter8 where

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

