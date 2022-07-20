module Text.S.Internal
  ( ByteString'
  , LazyByteString'
  , Text'
  , LazyText'
  , Stream(..)
  , reduceStream
  , Source(..)
  , initSource
  , updatePos
  , updatePos'
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
  , (<?>)
  , label
  , parseFromFile
  , parse
  , runParser
  , charParserOf
  ) where

import           Control.Applicative            ( Alternative(..) )
import           Control.Monad                  ( MonadPlus(..) )
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Char8    as C'
import           Data.List                      ( intercalate )
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy                as T'
import qualified Data.Text.Lazy.IO             as TIO'
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
  unCons     = L.uncons
  readStream = readFile


-------------------------
-- Source
-------------------------
data Source = Source
  { sourceName   :: FilePath
  , sourceLine   :: !Int
  , sourceColumn :: !Int
  }
  deriving (Eq, Ord)


instance Semigroup Source where
  (<>) = appendSource


instance Monoid Source where
  mempty = Source mempty 1 1


appendSource :: Source -> Source -> Source
appendSource src1@(Source f1 _ _) src2@(Source f2 _ _)
  | f1 /= f2 = error . unwords $ ["filepaths don't match:", f1, "and", f2]
  | otherwise = case src1 `compare` src2 of
    LT -> src2
    _  -> src1

initSource :: FilePath -> Source
initSource file = Source file 1 1

updatePos :: Source -> Char -> Source
updatePos src@Source {..} c = case c of
  '\n' -> src { sourceLine = sourceLine + 1, sourceColumn = 1 }
  '\t' -> src { sourceColumn = move sourceColumn 8 }
  _    -> src { sourceColumn = sourceColumn + 1 }
  where move col size = col + size - ((col - 1) `mod` size)

updatePos' :: Source -> String -> Source
updatePos' = foldl updatePos


-------------------------
-- ErrorMessage
-------------------------
data ErrorMessage = Unexpected !String
                  | Expected !String
                  | Message !String
                  deriving (Eq, Show, Ord)


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
  deriving Eq


instance Semigroup ParseError where
  (<>) = appendError


instance Monoid ParseError where
  mempty = ParseError mempty empty


appendError :: ParseError -> ParseError -> ParseError
appendError e1@(ParseError src1 msgs1) e2@(ParseError src2 msgs2) =
  case src1 `compare` src2 of
    LT -> e2
    GT -> e1
    EQ -> ParseError src1 $ msgs1 <> msgs2

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
  deriving Eq

initState :: FilePath -> s -> State s
initState file stream = State stream $ initSource file


-------------------------
-- Result / Return
-------------------------
data Result a = Ok a
              | Error ParseError
              deriving (Show, Eq)


data Return a s = Return (Result a) (State s)
  deriving Eq


-------------------------
-- Parser S
-------------------------
-- | Parser-S data type of self-describing the process of parsing work
newtype Parser'S s a = Parser'S {
  unpack :: forall b.
    State s ->                        -- state including stream input
    (a -> State s -> b) ->            -- answer Ok: when somthing comsumed
    (ParseError -> State s -> b) ->   -- answer Error: when nothing consumed
    b
  }

-- | infix operator of label
(<?>) :: Parser'S s a -> String -> Parser'S s a
(<?>) = label

label :: Parser'S s a -> String -> Parser'S s a
label parser msg = Parser'S $ \state@(State _ src) fOk fError ->
  let fError' err = fError (err <> expected)
      expected = expectedError src $ unwords ["->", "expected:", msg]
  in  unpack parser state fOk fError'


instance Functor (Parser'S s) where
  fmap = smap

smap :: (a -> b) -> Parser'S s a -> Parser'S s b
smap f parser =
  Parser'S $ \state fOk fError -> unpack parser state (fOk . f) fError


instance Applicative (Parser'S s) where
  pure x = Parser'S $ \state ok _ -> ok x state
  (<*>) = sap

sap :: Parser'S s (a -> b) -> Parser'S s a -> Parser'S s b
sap f parser = Parser'S $ \state fOk fError ->
  let fOk' x state' = unpack parser state' (fOk . x) fError
  in  unpack f state fOk' fError


instance Monad (Parser'S s) where
  return = pure
  (>>=)  = sbind
  (>>)   = (*>)

sbind :: Parser'S s a -> (a -> Parser'S s b) -> Parser'S s b
sbind parser f = Parser'S $ \state fOk fError ->
  let fOk' x state' = unpack (f x) state' fOk fError
  in  unpack parser state fOk' fError

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
        in  unpack q state fOk fError''
  in  unpack p state fOk fError'


instance MonadFail (Parser'S s) where
  fail msg =
    Parser'S $ \state _ fError -> fError (ParseError mempty [Message msg]) state


parseFromFile
  :: Stream s => Parser'S s a -> FilePath -> IO (Either ParseError a)
parseFromFile parser file = do
  stream <- readStream file
  let state = initState file stream
  return . parse parser $ state

parse :: Parser'S s a -> State s -> Either ParseError a
parse parser state = case result of
  Ok    ok  -> Right ok
  Error err -> Left err
  where Return result state' = runParser parser state

runParser :: Parser'S s a -> State s -> Return a s
runParser parser state = unpack parser state fOk fError
 where
  fOk    = Return . Ok
  fError = Return . Error


-- | get a char parser satisfying given predicates
charParserOf :: Stream s => (Char -> Bool) -> Parser'S s Char
charParserOf predicate = Parser'S $ \state@(State stream src) fOk fError ->
  case unCons stream of
    Nothing ->
      fError (unexpectedError src "end of stream: nothing to parse") state
    Just (c, cs) | predicate c -> seq src' $ seq state' $ fOk c state'
                 | otherwise   -> seq state' $ fError error' state
     where
      src'   = updatePos src c
      state' = State cs src'
      error' = unexpectedError src $ unwords
        ["failed to satisfy predicate with char unexpected:", show c]


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
  show (Return result state) = join'LF [show result, show state]


instance Show ParseError where
  show err =
    join'LF'Tab
      $ show (errorSource err)
      : (message <$> (mergeMessages . errorMessages $ err))


instance (Stream s, Show s) => Show (State s) where
  show state@State {..} = join'LF
    [ join'LF'Tab ["source from:", sourceName stateSource]
    , join'LF'Tab ["stream:", showStream]
    ]
   where
    stream = show stateStream
    showStream | length stream < 200 = stream
               | otherwise           = reduceStream stateStream 60


join'LF'Tab :: [String] -> String
join'LF'Tab = intercalate "\n\t"

join'LF :: [String] -> String
join'LF = intercalate "\n"

reduceStream :: (Stream s, Show s) => s -> Int -> String
reduceStream stream n = join'LF'Tab
  [take n s, "", "... (omitted) ...", "", drop (length s - n) s]
  where s = show stream
