module LispSpec where

import           Test.Hspec
import           Text.S.Example.Lisp

spec :: Spec
spec = do
  describe "SLISP REPL-show" $ do
    mapM_ (uncurry (test t)) in'out

t :: String -> String
t stream = do
  case re stream of
    Left  err    -> err
    Right (_, e) -> show' e

test :: (String -> String) -> String -> String -> SpecWith (Arg Expectation)
test f i o = it i $ do
  f i `shouldBe` o

in'out :: [(String, String)]
in'out =
  [ ("nil"                          , "nil")
  , ("'nil"                         , "nil")
  , ("()"                           , "nil")
  , ("'()"                          , "nil")
  , ("(eval (nth 2 '(1 2 (+ (* 34 43) 22) 4 5)))", "1484")
  , ("(cons '(2 . 3) ())"           , "((2 . 3))")
  , ("(defvar covid-19 2020)"       , "covid-19")
  , ("(setq covid-19 2020)"         , "2020")
  , ("`(,@`(,@`(1 2 3) 112))"       , "(1 2 3 112)")
  , ("`(,@'(+ 1 2))"                , "(+ 1 2)")
  , ("``(,@,`(1 2 3))"              , "`(,@(1 2 3))")
  , ("(eval `(list ,@'(9 5) 1 2 3))", "(9 5 1 2 3)")
  , ("`(1 2 @,'(a b c))"            , "(1 2 @ (a b c))")
  , ("(let ((x '(1 2 3))) `(,@x))"  , "(1 2 3)")
  , ("(let ((x '(1 2 3))) ``(,@,x))", "`(,@(1 2 3))")
  , ("``(a ,,(+ 1 2) ,(+ 3 4))"     , "`(a ,3 ,(+ 3 4))")
  , ("``,,'(1 2)"                   , "`,(1 2)")
  , ("`(,@`(1 2 3))"                , "(1 2 3)")
  , ("``(bq x ,x ,@x ,bq)"          , "`(bq x ,x ,@x ,bq)")
  , ("`(,@'123)"                    , "123")
  , ("(let ((y 123)) `(,@y))"       , "123")
  , ("``(,@,@'(1 2 3))"             , "`(,@1 ,@2 ,@3)")
  , ("(let ((x '(1 2 3))) ``(,,@x))", "`(,1 ,2 ,3)")
  ]
