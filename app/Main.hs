module Main where

import           Text.S
import           Text.S.Example.JSON

main :: IO ()
main =
  parseFromFile jsonParser "/Users/thyeem/Downloads/dao_dump.json" >>= print
