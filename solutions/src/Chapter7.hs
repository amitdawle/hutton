module Chapter7 where

import Data.Char 
import Data.List
import Data.Function

---
type Bit = Int 

bin2int :: [Bit] -> Int 
bin2int = foldr (\x t -> x + 2 * t) 0  


int2bin :: Int -> [Bit]
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8  (bits ++ repeat 0)


encode :: String -> [Bit]
encode =  concat .  map ( make8 . int2bin  . ord )


decode :: [Bit] -> String
decode [] = [] 
decode xs =   chr ( bin2int (take 8 xs) ) : decode (drop 8 xs)

----

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]


result :: (Ord a) => [a] -> [(Int, a)]
result = sortBy (compare `on` fst) . map (\vs -> (length vs, head vs)) . groupBy (==) . sort 


winner :: (Ord a) => [a] -> a
winner = snd . last . result


ballot :: [[String]]
ballot = [ ["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rank :: Ord a => [[a]] -> [a]
rank =  map snd . result .map head


winner' :: (Ord a) => [[a]] -> a
winner' bs = case rank (xs') of 
                  [c]  -> c
                  (c:cs) -> winner' (map (filter (/= c) ) xs')  
           where xs' = filter (not.null) bs





