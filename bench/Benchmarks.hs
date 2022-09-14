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
import           Data.Char                      ( isAlpha )
import           Data.List.Split                ( chunksOf )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Text.Megaparsec               as M
import qualified Text.Parsec                   as P
import qualified Text.S                        as S



main :: IO ()
main = do
  let s   = take 8192 . cycle $ ['a' .. 'z'] ++ ['A' .. 'Z']
      !b  = BC.pack s
      !bl = BL.fromChunks . map BC.pack . chunksOf 4 $ s
      !t  = T.pack s
      !tl = TL.fromChunks . map T.pack . chunksOf 4 $ s
  defaultMain
    [ bgroup
        "many"
        [ bgroup
          "attoparsec"
          [ bench "B" $ whnf (AB.parse (many (AC.satisfy isAlpha))) b
          , bench "BL" $ whnf (ABL.parse (many (AC.satisfy isAlpha))) bl
          , bench "T" $ whnf (AT.parse (many (AT.satisfy isAlpha))) t
          , bench "TL" $ whnf (ATL.parse (many (AT.satisfy isAlpha))) tl
          ]
        , bgroup
          "s"
          [ bench "S" $ whnf (S.t (many (S.charParserOf isAlpha))) s
          , bench "B" $ whnf (S.t (many (S.charParserOf isAlpha))) b
          , bench "BL" $ whnf (S.t (many (S.charParserOf isAlpha))) bl
          , bench "T" $ whnf (S.t (many (S.charParserOf isAlpha))) t
          , bench "TL" $ whnf (S.t (many (S.charParserOf isAlpha))) tl
          ]
        , bgroup
          "megaparsec"
          [ bench "S" $ whnf
              (M.runParser
                (many (M.satisfy isAlpha :: M.ParsecT M.Void String M.Identity))
              )
              s
          -- , bench "B" $ whnf (M.parse (many (M.satisfy isAlpha)) "") b
          -- , bench "BL" $ whnf (M.parse (many (M.satisfy isAlpha)) "") bl
          -- , bench "T" $ whnf (M.parse (many (M.satisfy isAlpha)) "") t
          -- , bench "TL" $ whnf (M.parse (many (M.satisfy isAlpha)) "") tl
          ]
        , bgroup
          "parsec"
          [ bench "S" $ whnf (P.parse (many (P.satisfy isAlpha)) "") s
          , bench "B" $ whnf (P.parse (many (P.satisfy isAlpha)) "") b
          , bench "BL" $ whnf (P.parse (many (P.satisfy isAlpha)) "") bl
          , bench "T" $ whnf (P.parse (many (P.satisfy isAlpha)) "") t
          , bench "TL" $ whnf (P.parse (many (P.satisfy isAlpha)) "") tl
          ]
        ]
    ]
