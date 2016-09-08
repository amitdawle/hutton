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
           it "works for n > 0 and type string" $ do
            replicate' 3 "a" `shouldBe` ["a", "a", "a"]
           it "works for n > 0 and type char" $ do
            replicate' 3 'a' `shouldBe` "aaa"

         describe "Chapter5.pyths" $ do
           it "works for n = 0" $ do
            pyths 0  `shouldBe` []
           it "works for n > 0" $ do
            pyths 10 `shouldBe` [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
           it "works for a large n > 0" $ do
            pyths 20 `shouldBe` [(3,4,5),(4,3,5),(5,12,13),(6,8,10),(8,6,10),(8,15,17),(9,12,15),(12,5,13),(12,9,15),(12,16,20),(15,8,17),(16,12,20)]


         describe "Chapter5.factors" $ do
           it "works for 0" $ do
            factors 0  `shouldBe` []
           it "works for small numbers" $ do
            factors 6  `shouldBe` [1,2,3,6]
           it "works for large numbers" $ do
            factors 120 `shouldBe` [1,2,3,4,5,6,8,10,12,15,20,24,30,40,60,120]

         describe "Chapter5.perfects" $ do
           it "works for 0" $ do
            perfects 0  `shouldBe` []
           it "works for small numbers" $ do
            perfects 10  `shouldBe` [6]
           it "works for large numbers" $ do
            perfects 500 `shouldBe` [6,28,496]            

         describe "Chapter5.positions" $ do
           it "works for empty list" $ do
            positions 1 []  `shouldBe` []
           it "works for small list" $ do
            positions 'a' "abracadabra"  `shouldBe` [0,3,5,7,10]
           it "works for when element is not present in list" $ do
            positions 'v' "abracadabra"  `shouldBe` []


         describe "Chapter5.scalarproduct" $ do
           it "works when first list is empty" $ do
            scalarproduct [1,2,3] []  `shouldBe` 0
           it "works for when second list is empty" $ do
            scalarproduct [] [1,2,3] `shouldBe` 0
           it "works for small list" $ do
            scalarproduct [1,2,3] [4,5,6] `shouldBe` 32
 
         describe "Chapter5.encode" $ do
           it "works when list is empty" $ do
            encode 3 ""  `shouldBe` ""
           it "works when list has one element" $ do
            encode 1 "a"  `shouldBe` "b"
           it "works for small shift" $ do
            encode 1 "Hello" `shouldBe` "Ifmmp"
           it "works for larger shift" $ do
            encode 10 "ABcd" `shouldBe` "KLmn"
           it "works for large string" $ do
            encode 3 "We live in a strange and dangerous world."  `shouldBe` "Zh olyh lq d vwudqjh dqg gdqjhurxv zruog."


         describe "Chapter5.decode" $ do
           it "works when list is empty" $ do
            decode 3 ""  `shouldBe` ""
           it "works when list has one element" $ do
            decode 1 "b"  `shouldBe` "a"
           it "works for small shift" $ do
            decode 1 "Ifmmp" `shouldBe` "Hello"
           it "works for larger shift" $ do
            decode 10 "KLmn" `shouldBe` "ABcd"
           it "works for larger string" $ do
            decode 3 "Zh olyh lq d vwudqjh dqg gdqjhurxv zruog." `shouldBe` "We live in a strange and dangerous world."


         describe "Chapter5.crack" $ do
           it "works when list is empty" $ do
            crack ""  `shouldBe` ""
           it "works when list upper and lower case and other characters" $ do
            crack "Zh olyh lq d vwudqjh dqg gdqjhurxv zruog." `shouldBe` "We live in a strange and dangerous world."
           it "works when list upper and lower case and other characters like numbers" $ do
            crack "1. Zh olyh lq d vwudqjh dqg gdqjhurxv zruog." `shouldBe` "1. We live in a strange and dangerous world."
