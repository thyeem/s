module LispSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Text.S.Example.Lisp

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "FIRST Hspec" $ do
    it "first test" $ do
      5 > 3 `shouldBe` True

    it "second test" $ do
      5 < 7 `shouldBe` True

    it "third test" $ do
      5 > 7 `shouldBe` False
