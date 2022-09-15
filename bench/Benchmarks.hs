module Main where

import qualified Attoparsec                    as A
import           Criterion.Main
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import qualified Megaparsec                    as M
import qualified Text.S                        as S
import qualified Text.S.Example.CSV



main :: IO ()
main = defaultMain []
