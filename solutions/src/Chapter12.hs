module Chapter12 where

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where 
    fmap f (Leaf a)   = Leaf (f a)
    fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)    


instance Eq a => Eq (Tree a) where
    (Leaf x)   == (Leaf y)   = x == y
    (Node a m b) == (Node c n d) =  m == n &&  (a == c ) && (b == d)
    _          == _          = False

-- 2
-- instance Functor ((->) a) where
        -- fmap :: (x -> y) -> f x -> f x 
        -- fmap :: (b->c) -> (->a) b -> (->a) c
        -- fmap :: (b->c) -> (a->b) -> (a->c) 
--        fmap  = (.)     



getChars :: Int -> IO [Char]
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1 )


-- 3
-- instance Applicative ((->) a) where
--     pure = const
--     -- <*> :: f(a -> b) -> f a -> f b
--     -- replacing  f with (->) x
--     -- <*> ::  (x -> a -> b) -> (x -> a) -> (x -> b)
--     g  <*> h = \x -> g x (h x)
--     --  h x => a
--     --  g x a => b


-- 6
-- instance Monad ((->) a) where 
--      --  return :: a -> f a
--      --  return :: a -> (x -> a)      
--      return  = pure 
--      -- (>>=) :: m a  -> (a -> m b) -> m b
--      -- (>>=) :: (x -> a) -> (a -> x -> b) -> ( x -> b)
--      (>>=) h g = \x -> g (h x) x  




data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
    fmap f (Var a) = Var (f a)
    fmap f (Val i) = Val i 
    fmap f (Add a b) = Add (fmap f a) (fmap f b)



instance Applicative Expr where
    pure = Var
    -- <*> :: f(a -> b) -> f a -> f b
    (Var f) <*> (Var x) = Var (f x)
    (Var f) <*> (Add a b) = Add (fmap f a) (fmap f b)





