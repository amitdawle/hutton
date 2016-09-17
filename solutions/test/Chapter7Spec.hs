module Chapter7Spec where
import Chapter7

import Test.Hspec
import Test.QuickCheck
import Prelude hiding (curry, uncurry)

import Control.Exception (evaluate)


spec = describe "Chapter7" $ do
         
         describe "Chapter7.1" $ do
           it "show that list comprehension [f x | x <- xs , p x] is same as map f ( filter p  xs) for empty list" $ do
            ( [ x + 1 | x <- [] , x > 5 ] ==  map (+1) (filter (>5) [])  ) `shouldBe` True
           it "show that list comprehension [f x | x <- xs , p x] is same as map f ( filter p  xs) for non empty list" $ do
            ( [ x + 1 | x <- [1..10] , x > 5 ] ==  map (+1) (filter (>5) [1..10])  ) `shouldBe` True
         
         describe "Chapter7.all" $ do
           it "should work for empty list" $ do
            Chapter7.all (>0) [] `shouldBe` True
           it "should work for non empty list" $ do
            Chapter7.all (>0) [1..10] `shouldBe` True
           it "should return false when some elements do not satisfy the condition" $ do
            Chapter7.all (>0) [1,2,3,-1] `shouldBe` False
         
         describe "Chapter7.any" $ do
           it "should work for empty list" $ do
            Chapter7.any (>0) [] `shouldBe` False
           it "should work for non empty list" $ do
            Chapter7.any (>0) [-1,1,-2,-3] `shouldBe` True
           it "should return false when no elements satisfy the condition" $ do
            Chapter7.any (>0) [-1,-2,-3] `shouldBe` False  

         describe "Chapter7.takeWhile" $ do
           it "should work on empty list" $ do
            Chapter7.takeWhile (>0) [] `shouldBe` ([]::[Int])
           it "should work for non empty list" $ do
            Chapter7.takeWhile (>0) [1,2,-2,-3] `shouldBe` [1,2]
           it "should return empty when starting element does not satisfy the condition" $ do
            Chapter7.takeWhile (>0) [-1,-2,-3] `shouldBe` ([]::[Int])  
           it "should work on infinite list" $ do
            Chapter7.takeWhile (<2) [0,1..] `shouldBe` [0,1]  
            
         describe "Chapter7.dropWhile" $ do
           it "should work on empty list" $ do
            Chapter7.dropWhile (>0) [] `shouldBe` ([]::[Int])
           it "should work for non empty list" $ do
            Chapter7.dropWhile (>0) [1,2,-2,-3] `shouldBe` [-2,-3]
           it "should return empty when starting element does not satisfy the condition" $ do
            Chapter7.dropWhile (>0) [-1,-2,-3] `shouldBe` ([-1,-2,-3]::[Int]) 

         --mapWithFoldR
         describe "Chapter7.mapWithFoldR" $ do
           it "should work on empty list" $ do
            Chapter7.mapWithFoldR (+1) [] `shouldBe` ([]::[Int])
           it "should work for non empty list" $ do
            Chapter7.mapWithFoldR (+1) [1,2,-2,-3] `shouldBe` [2,3,-1,-2]

         describe "Chapter7.filterWithFoldR" $ do
           it "should work on empty list" $ do
            Chapter7.filterWithFoldR (>0) [] `shouldBe` ([]::[Int])
           it "should work for non empty list" $ do
            Chapter7.filterWithFoldR (>0) [1,2,-2,-3,6] `shouldBe` [1,2,6]

 
         describe "Chapter7.dec2int" $ do
           it "should work on empty list" $ do
            Chapter7.dec2int  [] `shouldBe` 0
           it "should work for non empty list" $ do
            Chapter7.dec2int [1,2,3,4,5] `shouldBe` 12345

         describe "Chapter7.curry" $ do
           it "should convert a function with takes a pair into one that takes 2 args" $ do
            (Chapter7.curry  (\(x,y) -> x + y) ) 2 3 `shouldBe` 5
          
         describe "Chapter7.uncurry" $ do
           it "should convert a function with takes 2 args into one that takes a pair" $ do
             Chapter7.uncurry  (+) (2, 3) `shouldBe` 5
        

         describe "Chapter7.chop8" $ do
           it "should work on empty list" $ do
             Chapter7.chop8 []  `shouldBe` []
           it "should work on non empty list" $ do
             Chapter7.chop8 [1..16]  `shouldBe` [[1..8],[9..16]]
           it "should work on non empty list" $ do
             Chapter7.chop8 [1..16]  `shouldBe` [[1..8],[9..16]]
           it "should work on non empty list whose size is not multiple of 8" $ do
             Chapter7.chop8 [1..14]  `shouldBe` [[1..8],[9..14]]

         describe "Chapter7.mapWithUnfold" $ do
           it "should work on empty list" $ do
            Chapter7.mapWithUnfold (+1) [] `shouldBe` ([]::[Int])
           it "should work for non empty list" $ do
            Chapter7.mapWithUnfold (+1) [1,2,-2,-3] `shouldBe` [2,3,-1,-2]

         describe "Chapter7.iterateWithUnfold" $ do
           it "should work generate numbers in sequence when function is +1 and start is 1" $ do
            take 10 (Chapter7.iterateWithUnfold (+1) 1) `shouldBe` ([1..10]::[Int])
           

         describe "Chapter7.parity" $ do
           it "should work for empty list" $ do
            parity [] `shouldBe` 0
           it "should work for non empty list with even numbers of 1s" $ do
            parity [1,1,1,1,0,0] `shouldBe` 0
           it "should work for non empty1list with even numbers of 1s" $ do
            parity [1,1,0,1] `shouldBe` 1

         describe "Chapter7.decode2" $ do
           it "should work for empty list" $ do
            decode2 [] `shouldBe` ""
           it "should work for correctly encoded string" $ do
             decode2 (encode2 "A") `shouldBe` "A"
           it "should work for correctly encoded string" $ do
             evaluate (decode2 [1,1,1,1,1,1,1,1,1,1,1]) `shouldThrow` anyException
             
---           it "should work for incorrectly encoded string" $ do
---             evaluate(decode2 [1,1,1,1,1,1,1,1,1,1,1]) `shouldThrow` anyException


         describe "Chapter7.luhn" $ do
           it "should work for empty list" $ do
            luhn [] `shouldBe` True
           it "should work for correct Visa CC number" $ do
             luhn [4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] `shouldBe` True -- Visa
           it "should return False for incorrect Visa CC number" $ do
             luhn [4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0] `shouldBe` False -- Visa
           it "should work for correct Mastercard CC number" $ do
             luhn [5,5,0,0,0,0,0,0,0,0,0,0,0,0,0,4] `shouldBe` True -- Visa
           it "should return False incorrect Mastercard CC number" $ do
             luhn [5,5,0,0,0,0,0,0,0,0,0,0,0,0,0,3] `shouldBe` False -- Visa


                               












