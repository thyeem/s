module Main where

import Text.S
import Text.S.Example.JSON

main :: IO ()
main = do
  f <- readFile "data/large-025m.json"
  case parse' parseJSON f of
    Ok _ s -> pp s
    Error s -> pp s
