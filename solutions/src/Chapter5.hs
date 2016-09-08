module Chapter5 where
import Data.Char 
import Data.List as List
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

-- 8
find :: Eq a => a -> [(a, b)] -> [b]
find k ts = [v| (k', v) <- ts, k' == k]

-- In terms of find
positions :: Eq a => a -> [a] -> [Int]
positions k xs =  Chapter5.find k (zip xs [0..]) 


-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum (zipWith (*) xs ys)

-- Ceasar cipher

letterToInt :: Char -> Int
letterToInt c 
           | isLower c = ord c - ord 'a'
           | isUpper c = ord c - ord 'A' 

intToLetter :: Int -> Char
intToLetter i = chr $ ord 'a' + i

intToUpperLetter :: Int -> Char
intToUpperLetter i = chr $ ord 'A' + i


shift :: Int -> Char -> Char
shift n c 
      | isLower c = intToLetter ( ( letterToInt c  + n ) `mod` 26 )
      | isUpper c = intToUpperLetter ( ( letterToInt c  + n ) `mod` 26 ) 
      | otherwise = c


encode :: Int -> String -> String
encode n = map (shift n) 


decode :: Int -> String -> String
decode n = encode (-n)

lowers :: String -> Int
lowers s = length (filter (isLower) s ) 

alphabets :: String -> Int
alphabets s = length (filter (isAlpha) s ) 



count :: Char -> String -> Int
count c = length . filter (==c)


percent :: Int -> Int -> Float
percent a b = (fromIntegral a / fromIntegral b ) * 100


frequency :: String -> [Float]
frequency xs =  [percent  (count x ls) n | x <- ['a'..'z']]
                where n = alphabets xs
                      ls = map toLower xs


chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ ((o-e)^2)/e | (o,e) <- zip os es ]


rotate :: Int -> [a] -> [a]
rotate n xs =  drop n xs ++ take n xs


table = [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]

crack :: String -> String
crack iss = decode factor iss
            where inputFreq = frequency iss
                  table' = [ chisqr (rotate n inputFreq) table | n <- [0..25]]
                  factor = head ( positions (minimum table') table' )
                  





