-----------------------------------------------------------------------------
-- |
-- Module      : Text.S.Internal
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module implements internal process the parser really does.
--
-- One of core works is to design the parser as a Monad instance
-- while defining several primitive data types surrounding it.
--
-----------------------------------------------------------------------------

module Text.S.Internal
  ( ByteString'
  , LazyByteString'
  , Text'
  , LazyText'
  , Stream(..)
  , reduceStream
  , Source(..)
  , initSource
  , ErrorMessage(..)
  , message
  , mergeMessages
  , ParseError(..)
  , appendError
  , newError
  , unexpectedError
  , expectedError
  , msgError
  , Result(..)
  , Return(..)
  , State(..)
  , initState
  , Parser'S(..)
  , (<|>)
  , empty
  , (<?>)
  , label
  , parse
  , parse'
  , parseFromFile
  , t
  , t'
  , unwrap
  , unwrap'
  , assert
  , charParserOf
  ) where

import           Control.Applicative            ( Alternative(..)
                                                , liftA2
                                                )
import           Control.DeepSeq                ( NFData
                                                , force
                                                )
import           Control.Monad                  ( MonadPlus(..) )
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Char8    as C'
import           Data.List                      ( intercalate
                                                , uncons
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy                as T'
import qualified Data.Text.Lazy.IO             as TIO'
import           GHC.Generics                   ( Generic )
import           System.IO                      ( readFile )


type ByteString' = C.ByteString

type LazyByteString' = C'.ByteString

type Text' = T.Text

type LazyText' = T'.Text


-------------------------
-- Stream
-------------------------
class Stream s where
  unCons :: s -> Maybe (Char, s)
  readStream :: FilePath -> IO s

instance Stream ByteString' where
  unCons     = C.uncons
  readStream = C.readFile

instance Stream LazyByteString' where
  unCons     = C'.uncons
  readStream = C'.readFile

instance Stream Text' where
  unCons     = T.uncons
  readStream = TIO.readFile

instance Stream LazyText' where
  unCons     = T'.uncons
  readStream = TIO'.readFile

instance Stream String where
  unCons     = uncons
  readStream = readFile


-------------------------
-- Source
-------------------------
data Source = Source
  { sourceName   :: FilePath
  , sourceLine   :: !Int
  , sourceColumn :: !Int
  }
  deriving (Eq, Ord, Generic, NFData)


instance Semigroup Source where
  (<>) = appendSource


instance Monoid Source where
  mempty = Source mempty 1 1


appendSource :: Source -> Source -> Source
appendSource src1@(Source f1 _ _) src2@(Source f2 _ _)
  | f1 /= f2 = error $ unwords ["files don't match:", f1, "and", f2]
  | otherwise = case src1 `compare` src2 of
    LT -> src2
    _  -> src1

initSource :: FilePath -> Source
initSource file = Source file 1 1


-------------------------
-- ErrorMessage
-------------------------
data ErrorMessage = Unexpected !String
                  | Expected !String
                  | Message !String
                  deriving (Eq, Show, Ord, Generic, NFData)


message :: ErrorMessage -> String
message msg = case msg of
  Unexpected msg -> msg
  Expected   msg -> msg
  Message    msg -> msg

-- | merge ErrorMessages by folding consecutive expected errors
mergeMessages :: [ErrorMessage] -> [ErrorMessage]
mergeMessages = foldr merge []
 where
  merge msg msgs = case msgs of
    msg' : msgs' -> case (msg, msg') of
      (e1@Expected{}, e2@Expected{}) -> e2 : msgs'
      (e1           , e2           ) -> e1 : msgs
    _ -> [msg]


-------------------------
-- ParseError
-------------------------
data ParseError = ParseError
  { errorSource   :: !Source
  , errorMessages :: [ErrorMessage]
  }
  deriving (Eq, Generic, NFData)


instance Semigroup ParseError where
  (<>) = appendError


appendError :: ParseError -> ParseError -> ParseError
appendError e1@(ParseError src1 msgs1) e2@(ParseError src2 msgs2) =
  case src1 `compare` src2 of
    LT -> e2
    GT -> e1
    EQ -> ParseError src1 $ msgs1 <> msgs2


instance Monoid ParseError where
  mempty = ParseError mempty empty


newError :: Source -> (String -> ErrorMessage) -> String -> ParseError
newError src e msg = ParseError src [e msg]

unexpectedError :: Source -> String -> ParseError
unexpectedError src = newError src Unexpected

expectedError :: Source -> String -> ParseError
expectedError src = newError src Expected

msgError :: Source -> String -> ParseError
msgError src = newError src Message


-------------------------
-- State
-------------------------
data State s = State
  { stateStream :: s
  , stateSource :: !Source
  }
  deriving (Eq, Generic, NFData)


initState :: FilePath -> s -> State s
initState file stream = State stream $ initSource file


-------------------------
-- Result / Return
-------------------------
data Result a = Ok a
              | Error ParseError
              deriving (Show, Eq, Generic, NFData)


data Return a s = Return (Result a) (State s)
  deriving (Eq, Generic, NFData)


-------------------------
-- Parser S
-------------------------
-- Parser-S data type that self-describing the process of parsing work
newtype Parser'S s a = Parser'S {
    runParser :: forall b.
      State s ->                      -- state including stream input
      (a -> State s -> b) ->          -- call @Ok@ when somthing comsumed
      (ParseError -> State s -> b) -> -- call @Error@ when nothing consumed
      b
    }


-- | infix operator of flipped `label`
(<?>) :: Parser'S s a -> String -> Parser'S s a
(<?>) = flip label


-- |
label :: String -> Parser'S s a -> Parser'S s a
label msg parser = Parser'S $ \state@(State _ src) fOk fError ->
  let fError' err = fError (err <> expected)
      expected = expectedError src $ unwords ["->", "expected:", msg]
  in  runParser parser state fOk fError'


instance Functor (Parser'S s) where
  fmap = smap


smap :: (a -> b) -> Parser'S s a -> Parser'S s b
smap f parser =
  Parser'S $ \state fOk fError -> runParser parser state (fOk . f) fError


instance Applicative (Parser'S s) where
  pure x = Parser'S $ \state ok _ -> ok x state
  (<*>) = sap


sap :: Parser'S s (a -> b) -> Parser'S s a -> Parser'S s b
sap f parser = Parser'S $ \state fOk fError ->
  let fOk' x state' = runParser parser state' (fOk . x) fError
  in  runParser f state fOk' fError


instance Monad (Parser'S s) where
  return = pure
  (>>=)  = sbind
  (>>)   = (*>)


sbind :: Parser'S s a -> (a -> Parser'S s b) -> Parser'S s b
sbind parser f = Parser'S $ \state fOk fError ->
  let fOk' x state' = runParser (f x) state' fOk fError
  in  runParser parser state fOk' fError


instance Alternative (Parser'S s) where
  empty = mzero
  (<|>) = mplus


instance MonadPlus (Parser'S s) where
  mzero = szero
  mplus = splus


szero :: Parser'S s a
szero = fail mempty

splus :: Parser'S s a -> Parser'S s a -> Parser'S s a
splus p q = Parser'S $ \state fOk fError ->
  let fError' err' state' =
        let fError'' err'' state' = fError (err' <> err'') state'
        in  runParser q state fOk fError''
  in  runParser p state fOk fError'


instance MonadFail (Parser'S s) where
  fail msg = Parser'S $ \state@(State _ src) _ fError ->
    fError (ParseError src [Message msg]) state


-- | Takes a given state and parses it. The outermost function of the @Parser'S@
parse :: Parser'S s a -> State s -> Return a s
parse parser state = runParser parser state fOk fError
 where
  fOk    = Return . Ok
  fError = Return . Error

-- | The same to `parse`, but unwrap the @Return@ of the parse result
parse' :: Parser'S s a -> State s -> Either ParseError a
parse' parser = unwrap . parse parser

-- | The same to `parse`, but takes the stream from a given file
parseFromFile :: Stream s => Parser'S s a -> FilePath -> IO (Return a s)
parseFromFile parser file = do
  stream <- readStream file
  let state = initState file stream
  return . parse parser $ state

-- | Tests parsers and its combinators with given strings
t :: Parser'S String a -> String -> Return a String
t parser s = parse parser (State s mempty)

-- | The same to `t`, but unwrap the @Return@ of the parse result
t' :: Parser'S String a -> String -> Either ParseError a
t' parser = unwrap . t parser

-- | Unwraps @Return a s@, then return the result @a@ only
unwrap :: Return a s -> Either ParseError a
unwrap (Return r _) = case r of
  Ok    ok  -> Right ok
  Error err -> Left err

-- | Unwraps @Return a s@, then return stream from the state @s@
unwrap' :: Stream s => Return a s -> s
unwrap' (Return _ (State s _)) = s

-- | Tries to parse without comsuming any input
assert :: Parser'S s a -> Parser'S s a
assert parser = Parser'S $ \state fOk fError ->
  let fOk' x _ = fOk x state in runParser parser state fOk' fError

-- | Gets a char parser that satisfies the given predicate.
--
-- This function describes every parsing work at a fundamental level.
--
-- Here is where each parsing job starts.
--
charParserOf :: (Stream s, NFData s) => (Char -> Bool) -> Parser'S s Char
charParserOf predicate = Parser'S $ \state@(State stream src) fOk fError ->
  case unCons stream of
    Nothing ->
      fError (unexpectedError src "end-of-stream: nothing to parse") state
    Just (c, cs) | predicate c -> fOk c state'
                 | otherwise   -> fError error' state
     where
      src'   = force $ jump src c
      state' = force $ State cs src'
      jump (Source n ln col) c = case c of
        '\n' -> Source n (ln + 1) 1
        '\t' -> Source n ln (move col 8)
        _    -> Source n ln (col + 1)
        where move col size = col + size - ((col - 1) `mod` size)

      error' = unexpectedError src
        $ unwords ["failed to match unexpected character:", show c]


-------------------------
-- Show instances
-------------------------
instance Show Source where
  show src@Source {..} = unwords
    [ sourceName
    , "(line"
    , show sourceLine <> ","
    , "column"
    , show sourceColumn <> "):"
    ]


instance (Stream s, Show a, Show s) => Show (Return a s) where
  show (Return result state) = join'nl [show result, show state]


instance Show ParseError where
  show err =
    join'indent
      $ show (errorSource err)
      : (message <$> (mergeMessages . errorMessages $ err))


instance (Stream s, Show s) => Show (State s) where
  show state@State {..} = join'nl
    [ join'indent ["source from:", sourceName stateSource]
    , join'indent ["stream:", showStream]
    ]
   where
    stream = show stateStream
    showStream | length stream < 200 = stream
               | otherwise           = reduceStream stateStream 60


join'indent :: [String] -> String
join'indent = intercalate "\n\t"

join'nl :: [String] -> String
join'nl = intercalate "\n"

reduceStream :: (Stream s, Show s) => s -> Int -> String
reduceStream stream n = join'indent
  [take n s, "", "... (omitted) ...", "", drop (length s - n) s]
  where s = show stream
