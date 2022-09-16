module Main where

import           Criterion.Main
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Text.S                        as S
import           Text.S.Example.CSV


main :: IO ()
main = defaultMain []
