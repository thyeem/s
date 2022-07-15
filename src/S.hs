{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module S where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Char8    as CL
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Text.RawString.QQ


class Stream s where
  unCons :: s -> Maybe (Char, s)

instance Stream C.ByteString where
  unCons = C.uncons

instance Stream CL.ByteString where
  unCons = CL.uncons

instance Stream T.Text where
  unCons = T.uncons

instance Stream TL.Text where
  unCons = TL.uncons

instance Stream [Char] where
  unCons = L.uncons

data Source = Source
  { sourceName   :: FilePath
  , sourceLine   :: !Int
  , sourceColumn :: !Int
  }
  deriving (Show, Eq)

initSource :: FilePath -> Source
initSource file = Source file 1 1

updateSourceByChar :: Char -> Source -> Source
updateSourceByChar c src@Source {..} = case c of
  '\n' -> src { sourceLine = sourceLine + 1, sourceColumn = 1 }
  '\t' -> src { sourceColumn = addTab sourceColumn 4 }
  _    -> src { sourceColumn = sourceColumn + 1 }
  where addTab col size = col + size - ((col - 1) `mod` size)


data State s = State
  { stateStream      :: s
  , stateSource      :: Source
  , stateParseErrors :: [ParseError]
  }
  deriving (Show, Eq)

initState :: Stream s => FilePath -> s -> State s
initState file stream = State stream (initSource file) []

type ErrorMessage = String

data ParseError = ParseError
  { errorSource   :: !Source
  , errorMessages :: [ErrorMessage]
  }
  deriving (Show, Eq)

fakeError :: ParseError
fakeError = ParseError fakeSource fakeErrors where
  fakeSource = initSource "fakePath"
  fakeErrors =
    [ "Error: The Sun has become a blackhole."
    , "Error: The Riemann conjecture has just proved."
    ]

data Result a = Ok a
              | Error ParseError
              deriving (Show, Eq)

data Return a s = Return (Result a) (State s)
  deriving (Show, Eq)

-------------
-- Parser-S
-------------
-- A generailized parser combinator easy-to-use/read
-- The most simplified ever but robust.

-- | Type of Parser-S
-- self-describing process of parsing work
newtype Parser'S s a = Parser'S {
  unpack :: forall b.
    State s ->                        -- state including stream input
    (a -> State s -> b) ->            -- ok. somthing comsumed
    (ParseError -> State s -> b) ->   -- error. nothing consumed
    b
  }

-- | Parser'S is Functor
instance Functor (Parser'S s) where
  fmap = smap

smap :: (a -> b) -> Parser'S s a -> Parser'S s b
smap f parser =
  Parser'S $ \state fOk fError -> unpack parser state (fOk . f) fError

-- | Parser'S is Applicative
instance Applicative (Parser'S s) where
  pure x = Parser'S $ \state ok _ -> ok x state
  (<*>) = liftA2 id
  liftA2 f x = (<*>) (fmap f x)
  (*>) a b = (id <$ a) <*> b
  (<*) = liftA2 const

-- | Parser'S is Monad
instance Monad (Parser'S s) where
  return = pure
  (>>=)  = sbind
  (>>)   = (*>)
  -- m >>= return . f
  -- f <$> m

sbind :: Parser'S s a -> (a -> Parser'S s b) -> Parser'S s b
-- sbind parser f = parser >>= f
sbind parser f = Parser'S unpack'
  where unpack' = \state fOk fError -> undefined





-- get a concrete-type parser
type Parser a = Parser'S String a

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile parser file = do
  stream <- readFile file
  let state = initState file stream
  return . parse parser $ state

parse :: Stream s => Parser'S s a -> State s -> Either ParseError a
parse parser state = case result of
  Ok ok | null (stateParseErrors state') -> Right ok
        | otherwise                      -> Left fakeError
  Error _ -> Left fakeError
  where (Return result state') = runParser parser state


runParser :: Stream s => Parser'S s a -> State s -> Return a s
runParser parser state = unpack parser state answerOk answerErr
 where
  answerOk ok state' = Return (Ok ok) state'
  answerErr error state' = Return (Error error) state'

parserOf :: Stream s => (Char -> Bool) -> Parser'S s a
parserOf predicate = Parser'S unpack'
 where
  unpack' = \state@(State stream src errors) answerOk answerErr ->
    case unCons stream of
      Nothing -> answerErr fakeError state
      Just (c, cs) | predicate c -> undefined
                   | otherwise   -> answerErr fakeError state'
        where state' = undefined

-- anyChar :: Stream s => Parser'S s a
-- anyChar = parserOf (const True)

-- oneOf :: Stream s => [Char] -> Parser'S s a
-- oneOf cs = parserOf (`elem` cs)

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
