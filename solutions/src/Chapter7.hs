module Chapter7 where

import Data.Char 
import Data.List hiding (takeWhile, dropWhile)
import Prelude hiding (takeWhile, dropWhile)
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


--2 
all :: (a -> Bool) -> [a] -> Bool
all p  = foldr (\x t -> (p x) && t) True 

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x t -> p x || t ) False
-- also  or $ map (p) 

takeWhile :: (a -> Bool)->[a]->[a]
takeWhile _ [] = []
takeWhile p (x:xs)
          | p x = x : takeWhile p xs 
          | otherwise = []


dropWhile :: (a -> Bool)->[a]->[a]
dropWhile _ [] = []
dropWhile p (x:xs)
          | p x =  dropWhile p xs 
          | otherwise = x:xs


mapWithFoldR :: (a -> b) -> [a] -> [b]
mapWithFoldR f = foldr (\x t -> (f x) : t) []

filterWithFoldR :: (a -> Bool) -> [a] -> [a]
filterWithFoldR f = foldr (\x t -> if (f x) then x : t else t) []


dec2int :: [Int] -> Int
dec2int = foldl (\t x -> t * 10 + x) 0


curry :: ((a,b) -> c) -> a -> b ->c
curry f a b = f (a,b)

uncurry :: (a -> b -> c) -> (a,b) ->c 
uncurry f (a,b) = f a b

-- 6
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)


chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (null) (take 8) (drop 8)


mapWithUnfold f = unfold (null) (f.head) (tail)

iterateWithUnfold f = unfold (\x -> False) id (f) 



parity :: [Bit] -> Bit
parity xs = if even (sum xs) then 0 else 1


addParityBit :: [Bit] -> [Bit]
addParityBit bs = parity bs : bs


encode2 :: String -> [Bit]
encode2 =  concat .  map ( addParityBit. make8 . int2bin  . ord )


hasValidParity :: [Bit] -> Bool
hasValidParity bs = parity (drop 1 bs) == head bs 


decode2 :: [Bit] -> String
decode2 [] = [] 
decode2 xs = case (hasValidParity encodedByte) of
               True ->  chr (bin2int (tail encodedByte))  : decode2 (drop 9 xs)
               False -> error "Parity error"
              where encodedByte = take 9 xs  



altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []  = []
altMap f _ [x] = [f x]
altMap f g (x:y:xs) = f x : g y : altMap f g xs



luhn :: [Int] -> Bool
luhn xs = ( == 0) . (`mod` 10) . sum $ ( altMap (id) (f) (reverse xs) ) 

f :: Int -> Int 
f n = if double > 9 then double - 9 else double
   where double = n * 2 
















