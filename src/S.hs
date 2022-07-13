{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module S where

import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Char8    as CL
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Text.RawString.QQ



-- | stream kind: source input such as string, bytestring, lazybytestring ..
class Stream s where
  uncons :: s -> Maybe (Char, s)

instance Stream C.ByteString where
  uncons = C.uncons

instance Stream CL.ByteString where
  uncons = CL.uncons

instance Stream T.Text where
  uncons = T.uncons

instance Stream TL.Text where
  uncons = TL.uncons

instance Stream [Char] where
  uncons = L.uncons


data SourceLoc = SourceLoc
  { sourceName   :: FilePath
  , sourceLine   :: !Int
  , sourceColumn :: !Int
  }
  deriving (Show, Eq)

initSourceLoc :: FilePath -> SourceLoc
initSourceLoc n = SourceLoc n 1 1

type ErrorMessage = String

data State s = State
  { stateStream      :: s
  , stateSourceLoc   :: SourceLoc
  , stateParseErrors :: [ParseError]
  }
  deriving (Show, Eq)


data ParseError = ParseError !SourceLoc [ErrorMessage]
  deriving (Show, Eq)


data Result a s = Ok a !(State s) ParseError
                | Error ParseError
                deriving (Show, Eq)

data Munched a = Munched a
               | Nil !a
               deriving (Show, Eq)

-- generalized parser
newtype Parse s a = Parse
  { unPack :: forall b . State s -> b }

-- Down to a concrete type
type Parser a = Parse String a


-- combinators

stream = [r|
  public class Simple {

    /* block comment
    */

    public static void main(String[] args) { /* Wow  */
        System.out.println("get/*ting better");
    }
    // line comment
}
|]


loc = initSourceLoc "simple.java"
st :: State String
st = State stream loc []
