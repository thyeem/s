module Main where

import           System.Environment
import           Text.S
import           Text.S.Example.JSON

jsonParser = parseJSON :: ParserS Text JSON

main :: IO ()
main = do
  args <- getArgs
  parseFromFile jsonParser (head args) >>= print
