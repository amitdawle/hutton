module Chapter4Spec where

import Test.Hspec
import Test.QuickCheck
import Chapter4
import Control.Exception (evaluate)


spec = describe "Chapter4" $ do
         describe "Chapter4.halve" $ do
           it "works for even length list" $ do
            halve [1,2,3,4,5,6] `shouldBe` ([1,2,3],[4,5,6])
           it "works for odd length list" $ do
            halve [1,2,3,4,5,6,7] `shouldBe` ([1,2,3],[4,5,6,7])
           it "works for empty list" $ do
            halve []  `shouldBe` (([],[])::([Int],[Int]))
         
         describe "Chapter4.third_1" $ do
           it "works for list with 3 elements" $ do
            third_1 [1,2,3] `shouldBe` 3
           it "works for more than 3 elements" $ do
            third_1 [2,3,4,5,6,7] `shouldBe` 4
           it "throws error for list less then size 3" $ do
            evaluate(third_1 [2,3]) `shouldThrow` anyException
         
         describe "Chapter4.third_2" $ do
           it "works for list with 3 elements" $ do
            third_2 [1,2,3] `shouldBe` 3
           it "works for more than 3 elements" $ do
            third_2 [2,3,4,5,6,7] `shouldBe` 4
           it "throws error for list less then size 3" $ do
            evaluate(third_2 [2,3]) `shouldThrow` anyException
         
         describe "Chapter4.third_3" $ do
           it "works for list with 3 elements" $ do
            third_3 [1,2,3] `shouldBe` 3
           it "works for more than 3 elements" $ do
            third_3 [2,3,4,5,6,7] `shouldBe` 4
           it "throws error for list less then size 3" $ do
            evaluate(third_3 [2,3]) `shouldThrow` anyException
          
         describe "Chapter4.safetail_1" $ do
           it "works same as tail for list with 1 element" $ do
            safetail_1 [1] `shouldBe` []
           it "works same as tail for more than 1 element" $ do
            safetail_1 [1,2] `shouldBe` [2]
           it "returns [] for empty list" $ do
            safetail_1 [] `shouldBe` ([]::[Int])

         describe "Chapter4.safetail_2" $ do
           it "works same as tail for list with 1 element" $ do
            safetail_2 [1] `shouldBe` []
           it "works same as tail for more than 1 element" $ do
            safetail_2 [1,2] `shouldBe` [2]
           it "returns [] for empty list" $ do
            safetail_2 [] `shouldBe` ([]::[Int])

         describe "Chapter4.safetail_3" $ do
           it "works same as tail for list with 1 element" $ do
            safetail_3 [1] `shouldBe` []
           it "works same as tail for more than 1 element" $ do
            safetail_3 [1,2] `shouldBe` [2]
           it "returns [] for empty list" $ do
            safetail_3 [] `shouldBe` ([]::[Int])

         describe "Chapter4.luhnDouble" $ do
           it "Returns same number if less than 9" $ do
            luhnDouble 5 `shouldBe` 5
           it "Returns 9 if number is 9" $ do
            luhnDouble 9 `shouldBe` 9
           it "Returns number - 9  if number is greater than 9" $ do
            luhnDouble 17 `shouldBe` 8

         describe "Chapter4.luhn" $ do
           it "returns true for a valid card number" $ do
            luhn 1 7 8 4 `shouldBe` True
           it "returns false for a invalid card number" $ do
            luhn 4 7 8 3 `shouldBe` False
           

