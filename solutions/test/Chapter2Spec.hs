module Chapter2Spec where

import Test.Hspec
import Test.QuickCheck
import Chapter2
import Control.Exception (evaluate)


spec = describe "Chapter2" $ do
         describe "Chapter2.n" $ do
           it "works" $ do
            n `shouldBe` 2
         describe "Chapter2.last'" $ do
           it "works for list with single element" $ do
            last'[1] `shouldBe` 1
           it "works for list with many elements" $ do
            last'[1..100] `shouldBe` 100
           it "throws exception on empty list" $ do
             last' [] `shouldThrow` anyException 
         describe "Chapter2.last''" $ do
           it "works for list with single element" $ do
            last''[1] `shouldBe` 1
           it "works for list with many elements" $ do
            last''[1..100] `shouldBe` 100
           it "throws exception on empty list" $ do
             last'' [] `shouldThrow` anyException 
         describe "Chapter2.init'" $ do
           it "works for list with one element" $ do
             init' [1] `shouldBe` ([]::[Int])
           it "works for list with many elements" $ do
             init' [1..100] `shouldBe` [1..99]
           it "works for list with many elements" $ do
             evaluate(init' []) `shouldThrow` anyException  
         describe "Chapter2.init''" $ do
           it "works for list with one element" $ do
             init'' [1] `shouldBe` ([]::[Int])
           it "works for list with many elements" $ do
             init'' [1..100] `shouldBe` [1..99]
           it "works for list with many elements" $ do
             init'' [] `shouldBe` ([]::[Int])
             
            
          