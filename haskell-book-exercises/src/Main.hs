module Main where

import Test.Hspec
import Test.QuickCheck

trivialInt :: Gen Int
trivialInt = return 1

-- Arbitrary is a typeclass
-- Gen is a newtype
-- Arbitrary a => Gen a
main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is 2" $ do
      (1 + 1) == 2 `shouldBe` True
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
