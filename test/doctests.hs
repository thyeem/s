module Main where

import           Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/Text/S.hs"
  , "src/Text/S/Base.hs"
  , "src/Text/S/Internal.hs"
  , "src/Text/S/Language.hs"
  , "src/Text/S/Lexeme.hs"
  , "src/Text/S/Expr.hs"
  , "src/Text/S/Combinator.hs"
  , "src/Text/S/Example/Calc.hs"
  , "src/Text/S/Example/JSON.hs"
  , "src/Text/S/Example/CSV.hs"
  , "src/Text/S/Example/Markdown.hs"
  ]
