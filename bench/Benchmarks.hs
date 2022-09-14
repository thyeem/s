module Main where

import           Control.Applicative            ( many )
import           Criterion.Main
import qualified Data.Attoparsec.ByteString    as AB
import qualified Data.Attoparsec.ByteString.Char8
                                               as AC
import qualified Data.Attoparsec.ByteString.Lazy
                                               as ABL
import qualified Data.Attoparsec.Text          as AT
import qualified Data.Attoparsec.Text.Lazy     as ATL
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as BL
import           Data.List.Split                ( chunksOf )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Text.Parsec                   as P
import qualified Text.S                        as S




main :: IO ()
main = do
  let !s  = take 819200 . cycle $ ['a' .. 'z'] ++ ['A' .. 'Z'] :: String
      !b  = BC.pack s
      !bl = BL.fromChunks . map BC.pack . chunksOf 4 $ s
      !t  = T.pack s
      !tl = TL.fromChunks . map T.pack . chunksOf 4 $ s
  defaultMain
    [ bgroup
        "many"
        [ bgroup
          "attoparsec"
          [ bench "B" $ whnf (AB.parse (many (AC.satisfy AC.isAlpha_ascii))) b
          , bench "BL"
            $ whnf (ABL.parse (many (AC.satisfy AC.isAlpha_ascii))) bl
          , bench "T" $ whnf (AT.parse (many (AT.satisfy AC.isAlpha_ascii))) t
          , bench "TL"
            $ whnf (ATL.parse (many (AT.satisfy AC.isAlpha_ascii))) tl
          ]
        , bgroup
          "s"
          [ bench "S" $ whnf (S.t (S.many (S.charParserOf AC.isAlpha_ascii))) s
          , bench "B" $ whnf (S.t (S.many (S.charParserOf AC.isAlpha_ascii))) b
          , bench "BL"
            $ whnf (S.t (S.many (S.charParserOf AC.isAlpha_ascii))) bl
          , bench "T" $ whnf (S.t (S.many (S.charParserOf AC.isAlpha_ascii))) t
          , bench "TL"
            $ whnf (S.t (S.many (S.charParserOf AC.isAlpha_ascii))) tl
          ]
        , bgroup
          "parsec"
          [ bench "S" $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") s
          , bench "B" $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") b
          , bench "BL"
            $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") bl
          , bench "T" $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") t
          , bench "TL"
            $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") tl
          ]
        ]
    ]
