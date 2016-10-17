module Chapter9Spec where

import Chapter9

import Test.Hspec
import Test.QuickCheck
import Chapter1


spec = describe "Chapter9" $ do
         describe "Chapter9.choices2" $ do
           it "works for empty List" $ do
            choices2 [] `shouldBe` [[]]
           it "works for non empty List" $ do
            choices2 [1,2] `shouldBe` [[1,2],[2,1],[1],[2],[]]
           it "works same as choice" $ do
            choices2 [1,2,3] `shouldBe` (choices [1,2,3])
       
         describe "Chapter9.isChoice" $ do
           it "works for empty List" $ do
            isChoice []  [1,2,4]`shouldBe` True
           it "works for non empty List" $ do
            isChoice [3,1] [1,2,3]  `shouldBe` True
           it "works for non empty List when list is not a choice" $ do
            isChoice [4,1] [1,2,3]  `shouldBe` False   

         describe "Chapter9.solution with exponentiation" $ do
            it "produces a solution " $ do
             show ( head ( solutions [2,3,6] 192 ) ) `shouldBe` "(3 * (2 ^ 6))"

         describe "Chapter9.closestSolutions" $ do
            it "produces a closest solution when no solution exists" $ do
             show (closestSolutions' [1,3,7,10,25,50] 831) `shouldBe` "(((7 * (1 + 25)) + ((3 + 10) * 50)),1)" -- 830 possible, not 831

         describe "Chapter9.orderedBySimplicity" $ do
            it "orders simpler solutions first" $ do
             map (show) (orderedBySimplicity [1,2,3] 6) `shouldBe` ["(1 + (2 + 3))","((1 + 2) + 3)","(2 + (1 + 3))","(3 + (1 + 2))","(2 * 3)"]
            it "orders simpler solutions first" $ do
             map (show) (orderedBySimplicity [2,4,6] 6) `shouldBe` ["6","(2 + 4)"]