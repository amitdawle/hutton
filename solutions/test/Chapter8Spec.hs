module Chapter8Spec where
import Chapter8

import Test.Hspec
import Test.QuickCheck

import Control.Exception (evaluate)


spec = describe "Chapter8" $ do
         
         describe "Chapter8.add" $ do
           it "works for Zero add Zero" $ do
            add Zero Zero `shouldBe` Zero
           it "2 and 2 is 4" $ do
            add (Succ (Succ Zero)) (Succ (Succ Zero)) `shouldBe` (Succ (Succ (Succ (Succ Zero)))) 
           it "6 and 4 is 10" $ do
            add (int2nat 6) (int2nat 4) `shouldBe` (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))))

         describe "Chapter8.mult" $ do
           it "works when first number is Zero" $ do
            mult Zero (Succ Zero) `shouldBe` Zero
           it "works when second number is Zero" $ do
            mult (Succ Zero) Zero  `shouldBe` Zero
           it "5 * 2 should 10" $ do
            mult (int2nat 5) (int2nat 2) `shouldBe` (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))))


         describe "Chapter8.occurs" $ do
           it "works for Leaf" $ do
            occurs 1 (Leaf 1) `shouldBe` True
           it "works for Leaf when element is not present" $ do
            occurs 2 (Leaf 1) `shouldBe` False
           it "works for large trees when element is present" $ do
            occurs 4 (Node (Node (Leaf 2) 3 (Leaf 4)) 5 (Node (Leaf 8) 9 (Leaf 10) )) `shouldBe` True
           it "works for large trees when element is not present" $ do
            occurs 11 (Node (Node (Leaf 2) 3 (Leaf 4)) 5 (Node (Leaf 8) 9 (Leaf 10) )) `shouldBe` False
