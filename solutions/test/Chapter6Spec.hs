module Chapter6Spec where
import Prelude hiding ((!!), concat, replicate, elem, take, sum, last)
import Chapter6

import Test.Hspec
import Test.QuickCheck

import Control.Exception (evaluate)


spec = describe "Chapter6" $ do
         
         describe "Chapter6.factorial" $ do
           it "works for n = 0" $ do
            factorial 0 `shouldBe` 1
           it "works n > 0" $ do
            factorial 10  `shouldBe` 3628800
           it "throws error for n < 0" $ do
            evaluate(factorial (-1))  `shouldThrow` anyException

         describe "Chapter6.factorial2" $ do
           it "works for n = 0" $ do
            factorial2 0 `shouldBe` 1
           it "works for large n" $ do
            factorial2 50  `shouldBe` 30414093201713378043612608166064768844377641568960512000000000000

         describe "Chapter6.sumdown" $ do
           it "works for n = 0" $ do
            sumdown 0 `shouldBe` 0
           it "works for n > 0" $ do
            sumdown 3 `shouldBe` 6
           it "throws error for n < 0" $ do
            evaluate(sumdown (-1)) `shouldThrow` anyException 

         describe "Chapter6.exp2" $ do
           it "works for n = 0" $ do
            exp2 2 0 `shouldBe` 1
           it "works for n > 0" $ do
            exp2 2 5 `shouldBe` 32
       
         describe "Chapter6.euclide" $ do
           it "works when one number is 0" $ do
            euclid 2 0 `shouldBe` 2
           it "works when second number is 0" $ do
            euclid 0 13 `shouldBe` 13
           it "works for some number " $ do
            euclid 45 18`shouldBe` 9

         describe "Chapter6.and" $ do
           it "works for empty list" $ do
            Chapter6.and [] `shouldBe` True
           it "works when list contains all True(s)" $ do
            Chapter6.and [True,True] `shouldBe` True
           it "works for all False" $ do
            Chapter6.and [False,False] `shouldBe` False
           it "returns False when list contains one False" $ do
            Chapter6.and [True,True,False] `shouldBe` False

         describe "Chapter6.concat" $ do
           it "works for empty list" $ do
            Chapter6.concat [[],[]] `shouldBe` ([]::Num a=>[a])
           it "works concatting to an empty list" $ do
            Chapter6.concat [[1], [2]] `shouldBe` [1,2]
           it "works for Strings" $ do
            Chapter6.concat ["Hello", "World"] `shouldBe` "HelloWorld"

         describe "Chapter6.!!" $ do
           it "throws error for empty list" $ do
            evaluate((!!) [] 0)  `shouldThrow` anyException
           it "throws error when index is larger than the length of list" $ do
            evaluate((!!) [1] 3) `shouldThrow` anyException
           it "works for n > 0" $ do
            (!!) [1,2,3] 2 `shouldBe` 3
          
         describe "Chapter6.elem" $ do
           it "works for an empty list" $ do
            elem 1 []  `shouldBe` False
           it "returns true when element is present" $ do
            elem 1 [2,3,1]  `shouldBe` True
           it "returns false when element is no present" $ do
            elem 'a' "bcd" `shouldBe` False

         describe "Chapter6.replicate" $ do
           it "works for n = 0" $ do
            Chapter6.replicate 0 1  `shouldBe` ([]::Num a=>[a])
           it "works for n > 0" $ do
            Chapter6.replicate 10  1 `shouldBe` [1,1,1,1,1,1,1,1,1,1]
           it "works for n < 0" $ do
            Chapter6.replicate (-10)  1 `shouldBe` []

         describe "Chapter6.merge" $ do
           it "works for empty list" $ do
            Chapter6.merge [] []  `shouldBe` ([]:: Num a=>[a])
           it "works when first list is empty" $ do
            Chapter6.merge [] [1,3,4] `shouldBe` [1,3,4]
           it "works when second list is empty" $ do
            Chapter6.merge [1,3,4] [] `shouldBe` [1,3,4]   
           it "merges sorted lists correctly" $ do
            Chapter6.merge [2,5,6] [1,3,4] `shouldBe` [1,2,3,4,5,6] 

         describe "Chapter6.msort" $ do
           it "works for empty list" $ do
            Chapter6.msort []   `shouldBe` ([]:: Num a=>[a])
           it "works when list has one element" $ do
            Chapter6.msort [1] `shouldBe` [1]
           it "works for large list" $ do
            Chapter6.msort [1,3,5,2,4,6,7,8,10,9] `shouldBe` [1,2,3,4,5,6,7,8,9,10]   
          
         describe "Chapter6.sum" $ do
           it "works for empty list" $ do
            Chapter6.sum []   `shouldBe` (0)
           it "works when list has one element" $ do
            Chapter6.sum [2]   `shouldBe` (2)
           it "works for large list" $ do
            Chapter6.sum [1..10] `shouldBe` 55

         describe "Chapter6.take" $ do
           it "works for empty list" $ do
            Chapter6.take 1 [] `shouldBe` ([]::[Int])
           it "works when list has one element" $ do
            Chapter6.take 1 [1] `shouldBe` [1]
           it "works for large list" $ do
            Chapter6.take 2 [1..10] `shouldBe` [1,2]
           it "works when n is larger than list" $ do
            Chapter6.take 3 [1,2] `shouldBe` [1,2]
           it "works when n is less than 0" $ do
            Chapter6.take (-3) [1,2] `shouldBe` ([]::[Int])

         describe "Chapter6.last" $ do
           it "throws error for for empty list" $ do
            Chapter6.last [] `shouldThrow` anyException
           it "works when list has one element" $ do
            Chapter6.last [1] `shouldBe` 1
           it "works for large list" $ do
            Chapter6.last [1..10] `shouldBe` 10







-- --- 6
-- and :: [Bool] -> Bool
-- and [] = True
-- and (x:xs) | x = Chapter6.and xs
--            | otherwise = False

-- concat :: [[a]] -> [a]
-- concat [] = []
-- concat (xs:xss) = xs ++ concat xss

-- replicate :: Int -> a -> [a]
-- replicate 0 _ = []
-- replicate n a = a : replicate (n-1) a

-- (!!) :: [a] -> Int -> a
-- (!!) [] _ = error "Index too large"
-- (!!) (x:_) 0 = x
-- (!!) (_:xs) n = (!!) xs (n-1)

-- elem :: Eq a => a -> [a] -> Bool
-- elem _ [] = False
-- elem e (x:xs) | x == e = True
--               | otherwise = elem e xs

-- -- 7
-- merge :: (Ord a) => [a] -> [a] -> [a]
-- merge [] ys = ys
-- merge xs [] = xs
-- merge left@(x:xs) right@(y:ys) 
--        | x > y     = y : merge left ys
--        | otherwise = x : merge xs right       


-- --- 8
-- halve :: [a] -> ([a],[a])
-- halve [] = ([],[])
-- halve [x] = ([x],[])
-- halve xs = (take n xs, drop n xs)
--          where n = length xs  `div` 2        


-- msort :: (Ord a) => [a] -> [a]
-- msort []  = []
-- msort [x] = [x]
-- msort xs = merge (msort left)  (msort right)
--            where (left, right) = halve xs

-- --- 9
-- sum :: [Int] -> Int
-- sum [] = 0
-- sum (x:xs) = x + sum xs


-- take :: Int -> [a] -> [a]
-- take 0 xs = []
-- take _ [] = []
-- take n (x:xs) = x : take (n-1) xs

-- last :: [a] -> a
-- last [] = error "last of empty list"
-- last [x] = x
-- last (_:xs) = last xs 


