module Chapter5Spec where

import Test.Hspec
import Test.QuickCheck
import Chapter5
import Control.Exception (evaluate)


spec = describe "Chapter5" $ do
         
         describe "Chapter5.square" $ do
           it "works for n = 0" $ do
            square 0 `shouldBe` []
           it "works n > 0" $ do
            square 2  `shouldBe` [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
         

         describe "Chapter5.replicate'" $ do
           it "works for n = 0" $ do
            replicate' 0  'a' `shouldBe` []
           it "works n > 0 and type string" $ do
            replicate' 3 "a" `shouldBe` ["a", "a", "a"]
           it "works n > 0 and type char" $ do
            replicate' 3 'a' `shouldBe` "aaa"

