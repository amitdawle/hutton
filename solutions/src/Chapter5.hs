module Chapter5 where

-- 3

square :: Int -> [(Int,Int)]
square n = [(x,y) | x<-[0..n], y<-[0..n],  y /= x]

-- 4

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : (replicate' (n-1) a)