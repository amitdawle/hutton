module Chapter5 where

-- 3

square :: Int -> [(Int,Int)]
square n = [(x,y) | x<-[0..n], y<-[0..n],  y /= x]

-- 4

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : (replicate' (n-1) a)

-- 5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z)| x <-[1..n], y <- [1..n], z <- [x..n], y /= x && (x^2 + y^2 == z^2) ]

--6 
factors :: Int -> [Int]
factors n = [x| x <- [1..n] , n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n] , sum (init (factors x)) == x ]