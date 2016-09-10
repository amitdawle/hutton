module Chapter6 where
import Prelude hiding ((!!), concat, replicate, elem, take, sum, last)

-- 1
factorial :: Integer -> Integer
factorial n 
         | n < 0 = error "factorial undefined for negative number"
         | n == 0 = 1
         | otherwise = n * factorial (n - 1)


-- tail recursive
factorial2 :: Integer -> Integer
factorial2 n 
        | n < 0 = error "factorial undefined for negative number"
        | otherwise   = go n 1
                        where go 0 a = a 
                              go x a = go (x-1) (x * a)


-- 2
sumdown :: Int -> Int
sumdown x
        | x == 0      = 0
        | x < 0       = error "sumdown not defined for negative numbers." 
        | otherwise   = x + sumdown (x - 1)

-- 3
exp1 :: Int -> Int -> Int
exp1 _ 0 = 1
exp1 a 1 = a
exp1 a n = a * (exp1 a (n-1))


exp2 :: Int -> Int -> Int
exp2 _ 0 = 1
exp2 a 1 = a
exp2 a n 
       | even n  =  exp2 ( a * a ) (n `div` 2)
       | otherwise = a * exp2 ( a * a ) (n `div` 2)   

--- 4
euclid :: Int -> Int -> Int 
euclid a b
       | a == 0 = b
       | b == 0 = a  
       | a == b = a 
       | a > b = euclid (a - b) b
       | otherwise = euclid a  (b - a)
       

--- 6
and :: [Bool] -> Bool
and [] = True
and (x:xs) | x = Chapter6.and xs
           | otherwise = False

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a 
          | n < 0 = []
          | otherwise = a : replicate (n-1) a

(!!) :: [a] -> Int -> a
(!!) [] _ = error "Index too large"
(!!) (x:_) 0 = x
(!!) (_:xs) n = (!!) xs (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem e (x:xs) | x == e = True
              | otherwise = elem e xs

-- 7
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge left@(x:xs) right@(y:ys) 
       | x > y     = y : merge left ys
       | otherwise = x : merge xs right       


--- 8
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve [x] = ([x],[])
halve xs = (take n xs, drop n xs)
         where n = length xs  `div` 2        


msort :: (Ord a) => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs = merge (msort left)  (msort right)
           where (left, right) = halve xs

--- 9
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs


take :: Int -> [a] -> [a]
take 0 xs = []
take _ [] = []
take n (x:xs)
    | n < 0 = [] 
    | otherwise = x : take (n-1) xs

last :: [a] -> a
last [] = error "last of empty list"
last [x] = x
last (_:xs) = last xs 


