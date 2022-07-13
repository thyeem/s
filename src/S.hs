{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module S where

import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Char8    as CL
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Text.RawString.QQ


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

-------------
-- Parser-S
-------------
-- A generailized parser combinator easy-to-read and easy-to-read
-- The most simplified ever but robust.

-- | Type of Parser-S
-- self-describing process of parsing work
newtype Parser'S t s = Parser'S
  {
    unpack :: forall a .
    State s ->                             -- state including stream input
    (t -> State s -> ParseError -> a) ->   -- ok. somthing comsumed
    (ParseError -> a) ->                   -- error. nothing consumed
    a
  }

type ErrorMessage = String

data Source = Source
  { sourceName   :: FilePath
  , sourceLine   :: !Int
  , sourceColumn :: !Int
  }
  deriving (Show, Eq)

initSource :: FilePath -> Source
initSource file = Source file 1 1


data State s = State
  { stateStream      :: s
  , stateSource      :: Source
  , stateParseErrors :: [ParseError]
  }
  deriving (Show, Eq)

initState :: Stream s => FilePath -> s -> State s
initState file stream = State stream (initSource file) []


data ParseError = ParseError !Source [ErrorMessage]
  deriving (Show, Eq)

data Result t = Ok t
                | Error ParseError
                deriving (Show, Eq)

data Return t s = Return (Result t) (State s)
  deriving (Show, Eq)

-- get a concrete-type parser
type Parser a = Parser'S String a

-- runParser :: Parser'S t s -> State s -> Result t s
-- runParser parser state =  parser state ok err
 -- where
  -- ok result state' error = Ok result state' error
  -- err error = Error error


-- parse :: Stream s => Parser'S t s -> State s -> Either ParseError t
-- parse parser state = do
  -- r <- parse'S parser state

parse'S :: Stream s => Parser'S t s -> State s -> Return t s
parse'S parser state = unpack parser state ok err
 where
  ok t state' = Return (Ok t) state'
  err error = Error error

-- parseFromFile :: Stream a => Parser a -> FilePath -> IO (Either ParseError a)
-- parseFromFile parser file = do
  -- stream <- readFile file
  -- return $ parse parser file stream

parserOf :: Stream s => (Char -> Bool) -> Parser'S t s
parserOf predicate = undefined


stream' = [r|
  public class Simple {

    /* block comment
    */

    public static void main(String[] args) { /* Wow  */
        System.out.println("get/*ting better");
    }
    // line comment
}
|]


loc = initSource "simple.java"
st :: State String
st = State stream' loc []
