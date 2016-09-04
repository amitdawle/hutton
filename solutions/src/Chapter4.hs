module Chapter4 where

-- 1 
halve :: [a] -> ([a],[a]) 
halve xs = (take mid xs, drop mid xs)
	     where mid = length xs `div` 2

-- 2
-- With head and tail
third_1 :: [a] -> a
third_1 = head . tail . tail 

-- With !!
third_2 :: [a] -> a
third_2 xs = xs !! 2

-- With pattern matching
third_3 :: [a] -> a
third_3 [] = error "Empty list"
third_3 [_] = error "List with one elem only"
third_3 [_,_] = error "List with only 2 elems"
third_3 (_:_:z:_) = z

-- 3
-- With conditional
safetail_1 :: [a] -> [a]
safetail_1 xs = if null xs then [] else tail xs

-- With gaurds
safetail_2 :: [a] -> [a]
safetail_2 xs
     | null xs = []
     | otherwise = tail xs

-- With pattern matching
safetail_3 :: [a] -> [a]
safetail_3 [] = []
safetail_3 (_:xs) = xs

-- 8
luhnDouble :: Int -> Int
luhnDouble x = if x > 9 then x - 9 else x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = crc `mod` 10 == 0
	         where crc = luhnDouble (a * 2)  + b + luhnDouble (c * 2) + d