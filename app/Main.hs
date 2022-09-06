module Main where

import           Text.S
import           Text.S.Example.JSON

main :: IO ()
main =
  parseFromFile
      jsonParser
      "/Users/thyeem/Downloads/genesis_9b01743ef1a5e0832b20d68bded01c0d2bfcd4b9.json"
    >>= print
