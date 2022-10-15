module LispSpec where

import           Test.Hspec
import           Text.S.Example.Lisp

t :: String -> String
t stream = do
  case re stream of
    Left  err    -> err
    Right (_, e) -> show' e

spec :: Spec
spec = do
  describe "SLISP repl-show" $ do
    it "nil" $ do
      t "nil" `shouldBe` "nil"
    it "'nil" $ do
      t "'nil" `shouldBe` "nil"
    it "()" $ do
      t "()" `shouldBe` "nil"
    it "'()" $ do
      t "'()" `shouldBe` "nil"
    it "(cons '(2 . 3) ())" $ do
      t "(cons '(2 . 3) ())" `shouldBe` "((2 . 3))"
    it "(eval (nth 2 '(1 2 (+ (* 34 43) 22) 4 5)))" $ do
      t "(eval (nth 2 '(1 2 (+ (* 34 43) 22) 4 5)))" `shouldBe` "1484"
    it "(defvar covid-19 2020)" $ do
      t "(defvar covid-19 2020)" `shouldBe` "covid-19"
    it "(setq covid-19 2020)" $ do
      t "(setq covid-19 2020)" `shouldBe` "2020"
