module Main where

import Day2
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "splitNumbers" $ do
    it "should split the numbers from the input string" $
      splitNumbers "3x2x5" `shouldBe` [3, 2, 5]
  describe "requiredArea" $ do
    it "should calculate the required area" $
      requiredArea [2,3,4] `shouldBe` 58
    it "should calculate the required area" $
      requiredArea [1,1,10] `shouldBe` 43
  describe "requiredRibbon" $ do
    it "should calculate the required length" $
      requiredRibbon [2,3,4] `shouldBe` 34
    it "should calculate the required length" $
      requiredRibbon [1,1,10] `shouldBe` 14
