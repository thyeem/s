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
  , Message
  , ParseError(..)
  , mergeError
  , initError
  , newError
  , addErrorMessage
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
-- ParseError
-------------------------
type Message = String

data ParseError = ParseError
  { errorSource   :: !Source
  , errorMessages :: [Message]
  }
  deriving Eq

instance Semigroup ParseError where
  (<>) = mergeError

instance Monoid ParseError where
  mempty = ParseError (initSource mempty) []

mergeError :: ParseError -> ParseError -> ParseError
mergeError e1@(ParseError src1 msgs1) e2@(ParseError src2 msgs2) =
  case src1 `compare` src2 of
    LT -> e2
    GT -> e1
    EQ -> ParseError src1 $ msgs1 <> msgs2

initError :: Source -> ParseError
initError src = ParseError src []

newError :: Source -> Message -> ParseError
newError src = addErrorMessage (initError src)

addErrorMessage :: ParseError -> Message -> ParseError
addErrorMessage ParseError {..} msg =
  ParseError errorSource (msg : filter (msg /=) errorMessages)


-------------------------
-- State
-------------------------
data State s = State
  { stateStream      :: s
  , stateSource      :: !Source
  , stateParseErrors :: [ParseError]
  }
  deriving Eq

initState :: Stream s => FilePath -> s -> State s
initState file stream = State stream (initSource file) []


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
label parser msg = Parser'S $ \state@(State _ src _) fOk fError ->
  let fError' err = fError (err <> expected)
      expected = newError src (unwords ["->", "expected:", msg])
  in  unpack parser state fOk fError'


-- | Parser'S is Functor
instance Functor (Parser'S s) where
  fmap = smap

smap :: (a -> b) -> Parser'S s a -> Parser'S s b
smap f parser =
  Parser'S $ \state fOk fError -> unpack parser state (fOk . f) fError


-- | Parser'S is Applicative
instance Applicative (Parser'S s) where
  pure x = Parser'S $ \state ok _ -> ok x state
  (<*>) = sap
  a *> b = a `sbind` const b
  a <* b = b `sbind` const a

sap :: Parser'S s (a -> b) -> Parser'S s a -> Parser'S s b
sap f parser = Parser'S $ \state fOk fError ->
  let fOk' x state' = unpack parser state' (fOk . x) fError
  in  unpack f state fOk' fError


-- | Parser'S is Monad
instance Monad (Parser'S s) where
  return = pure
  (>>=)  = sbind
  (>>)   = (*>)

sbind :: Parser'S s a -> (a -> Parser'S s b) -> Parser'S s b
sbind parser f = Parser'S $ \state fOk fError ->
  let fOk' x state' = unpack (f x) state' fOk fError
  in  unpack parser state fOk' fError

-- | Parser'S is Alternative
instance Alternative (Parser'S s) where
  empty = mzero
  (<|>) = mplus


-- | Parser'S is MonadPlus
instance MonadPlus (Parser'S s) where
  mzero = szero
  mplus = splus


szero :: Parser'S s a
szero = Parser'S $ \state _ fError -> fError mempty state

splus :: Parser'S s a -> Parser'S s a -> Parser'S s a
splus p q = Parser'S $ \state fOk fError ->
  let fError' err' state' =
        let fError'' err'' state' = fError (err' <> err'') state'
        in  unpack q state fOk fError''
  in  unpack p state fOk fError'


parseFromFile
  :: Stream s => Parser'S s a -> FilePath -> IO (Either ParseError a)
parseFromFile parser file = do
  stream <- readStream file
  let state = initState file stream
  return . parse parser $ state

parse :: Stream s => Parser'S s a -> State s -> Either ParseError a
parse parser state = case result of
  Ok    ok  -> Right ok
  Error err -> Left err
  where Return result state' = runParser parser state

runParser :: Stream s => Parser'S s a -> State s -> Return a s
runParser parser state = unpack parser state fOk fError
 where
  fOk    = Return . Ok
  fError = Return . Error


-- | get a char parser satisfying given predicates
charParserOf :: Stream s => (Char -> Bool) -> Parser'S s Char
charParserOf predicate =
  Parser'S $ \state@(State stream src errors) fOk fError ->
    case unCons stream of
      Nothing -> fError (newError src "end of stream: nothing to parse") state
      Just (c, cs) | predicate c -> seq src' $ seq state' $ fOk c state'
                   | otherwise   -> seq state' $ fError error' state'
       where
        src'   = updatePos src c
        state' = State cs src' errors
        error' = newError src $ unwords
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
  show err = join'Tab $ show (errorSource err) : errorMessages err


instance (Stream s, Show s) => Show (State s) where
  show state@State {..} = join'LF
    [ join'Tab ["source from:", sourceName stateSource]
    , join'Tab ["stream:", showStream]
    , join'LF $ show <$> stateParseErrors
    ]
   where
    stream = show stateStream
    showStream | length stream < 200 = stream
               | otherwise           = reduceStream stateStream 60


join'Tab :: [String] -> String
join'Tab = Data.List.intercalate "\n\t"

join'LF :: [String] -> String
join'LF = Data.List.intercalate "\n"

reduceStream :: (Stream s, Show s) => s -> Int -> String
reduceStream stream n = join'Tab
  [take n s, "", "... (omitted) ...", "", drop (length s - n) s]
  where s = show stream
