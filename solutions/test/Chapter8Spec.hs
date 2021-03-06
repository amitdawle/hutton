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


         describe "Chapter8.balanced" $ do
           it "works for Leaf" $ do
            balanced (Leaf2 1) `shouldBe` True
           it "works for a balanced tree" $ do
            balanced (Node2 (Leaf2 1) (Node2 (Leaf2 3) (Leaf2 4))) `shouldBe` True
           it "works unbalanced trees" $ do
            balanced (Node2 (Node2 (Node2 (Leaf2 1) (Leaf2 2)) (Leaf2 4)) (Leaf2 10) ) `shouldBe` False
          
  
         describe "Chapter8.balance" $ do
           it "works for one element" $ do
            balance [1] `shouldBe` Leaf2 1
           it "works for a list" $ do
            balance [1,3,4] `shouldBe` (Node2 (Leaf2 1) (Node2 (Leaf2 3) (Leaf2 4)))

         describe "Chapter8.folde" $ do
           it "works for one element" $ do
            folde (id) (+) (Val 1) `shouldBe` 1
           it "works for a complex expression" $ do
            folde  (id) (+) (Add (Val 1) (Add (Val 2) (Val 3))) `shouldBe` 6
          
         describe "Chapter8.size" $ do
           it "works for one element" $ do
            size (Val 1) `shouldBe` 1
           it "works for a complex expression" $ do
            size (Add (Val 1) (Add (Val 2) (Val 3))) `shouldBe` 3


         describe "Chapter8.disjunction" $ do
           it "works for a proposition A OR (not A) (which is always true)" $ do
            isTaut (Or (Var 'A') (Not (Var 'A'))) `shouldBe` True
           
         describe "Chapter8.equivalence" $ do
           it "works for : Equivalent (A And B => A)  (A Or Not A)  :  A <=> B " $ do
            isTaut (Equivalence (Imply (And (Var 'A') (Var 'B')) (Var 'A') )  (Or (Var 'A') (Not (Var 'A')) )  ) `shouldBe` True 


         describe "Chapter8.value2" $ do
           it "works for one element" $ do
            value2  (Val 1) `shouldBe` 1
           it "works for a complex expression" $ do
            value2 (Add (Val 1) (Add (Val 2) (Val 3))) `shouldBe` 6
           it "works for a complex expression" $ do
            value2 (Mult (Val 2) (Add (Val 4) (Val 3))) `shouldBe` 14
        

         describe "Chapter8.value" $ do
           it "works for one element" $ do
            value  (Val 1) `shouldBe` 1
           it "works for a complex expression" $ do
            value (Add (Val 1) (Add (Val 2) (Val 3))) `shouldBe` 6
           it "works for a complex expression" $ do
            value (Mult (Val 2) (Add (Val 4) (Val 3))) `shouldBe` 14

