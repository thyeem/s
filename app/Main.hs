module Main where

import           System.Environment
import           Text.S
import           Text.S.Example.JSON

main :: IO ()
main = do
  args <- getArgs
  parseFromFile jsonParser (args !! 0) >>= pp
