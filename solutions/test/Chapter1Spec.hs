module Chapter1Spec where

import Test.Hspec
import Test.QuickCheck
import Chapter1


spec = describe "Chapter1" $ do
         describe "Chapter1.myproduct" $ do
           it "works for empty List" $ do
            myproduct [] `shouldBe` 1
           it "works for non-empty List" $ do
            myproduct [1,2,3,4] `shouldBe` 24
           it "works for List with repeated characters" $ do
            myproduct [1,2,2,3] `shouldBe` 12
         describe "Chapter1.revQsort" $ do
           it "works for empty List" $ do
            revQsort [] `shouldBe` ([]::(Num a => [a]))
           it "works for non-empty List" $ do
            revQsort [1,2,3,4] `shouldBe` [4,3,2,1]
           it "works for duplicates in List" $ do
            revQsort [4,1,2,2,3,1] `shouldBe` [4,3,2,2,1,1]