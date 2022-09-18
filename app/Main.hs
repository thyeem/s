module Main where

import           System.Environment
import           Text.S
import           Text.S.Example.JSON


main :: IO ()
main = do
  args <- getArgs
  parseFile (jsonParser :: ParserS Text JSON) (head args) >>= print
