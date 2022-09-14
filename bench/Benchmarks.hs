module Main where

import           Criterion.Main
import           Text.S
import           Text.S.Example.JSON


-- fib n = fibs !! n where fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

-- main :: IO ()
-- main = defaultMain
  -- [bgroup "fib" [bench "10" $ whnf fib 10, bench "35" $ whnf fib 35]]

              -- (t' jsonParser "{\"class\": [\"JavaScript\",\"HTML\",\"CSS\"]}")

stest = t' jsonParser

main :: IO ()
main = defaultMain
  [ bgroup
      "s"
      [ bench "readStream"
          $ whnf stest "{\"class\": [\"JavaScript\",\"HTML\",\"CSS\"]}"
      ]
  ]
