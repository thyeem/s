{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module S where

import           Control.Applicative            ( Alternative(..) )
import           Control.Monad                  ( MonadPlus(..) )
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Char                      ( isAlpha
                                                , isAlphaNum
                                                , isDigit
                                                , isHexDigit
                                                , isLower
                                                , isSpace
                                                , isUpper
                                                )
import           Data.List                      ( intercalate )
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Text.RawString.QQ              ( r )

---------------------
-- Utils
---------------------

joinTab :: [String] -> String
joinTab = Data.List.intercalate "\n\t"

joinLF :: [String] -> String
joinLF = Data.List.intercalate "\n"


---------------------
-- Stream
---------------------
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

reduceStream :: (Stream s, Show s) => s -> Int -> String
reduceStream stream n = joinTab
  [take n s, "", "... (omitted) ...", "", drop (length s - n) s]
  where s = show stream


---------------------
-- Source
---------------------
data Source = Source
  { sourceName   :: FilePath
  , sourceLine   :: !Int
  , sourceColumn :: !Int
  }
  deriving (Eq, Ord)

instance Show Source where
  show src@Source {..} = unwords
    [ sourceName
    , "(line"
    , show sourceLine <> ","
    , "column"
    , show sourceColumn <> "):"
    ]

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

---------------------
-- State / Result
---------------------
data Result a = Ok a
              | Error ParseError
              deriving (Show, Eq)


data Return a s = Return (Result a) (State s)
  deriving Eq


instance (Stream s, Show a, Show s) => Show (Return a s) where
  show (Return result state) = joinLF [show result, show state]


data State s = State
  { stateStream      :: s
  , stateSource      :: !Source
  , stateParseErrors :: [ParseError]
  }
  deriving Eq

instance (Stream s, Show s) => Show (State s) where
  show state@State {..} = joinLF
    [ joinTab ["source from:", sourceName stateSource]
    , joinTab ["stream:", showStream]
    , joinLF $ show <$> stateParseErrors
    ]
   where
    stream = show stateStream
    showStream | length stream < 200 = stream
               | otherwise           = reduceStream stateStream 60


initState :: Stream s => FilePath -> s -> State s
initState file stream = State stream (initSource file) []


---------------------
-- ParseError
---------------------
type Message = String

data ParseError = ParseError
  { errorSource   :: !Source
  , errorMessages :: [Message]
  }
  deriving Eq

instance Show ParseError where
  show err = joinTab $ show (errorSource err) : errorMessages err

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


---------------------
-- Parser S internal
---------------------
-- A generailized parser combinator easy-to-use/read
-- The most simplified ever but robust.

-- | Parser-S: data type of self-describing the process of parsing work
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


---------------------
-- Main
---------------------

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

---------------------
-- string parser
---------------------
-- get a concrete-type parser
type Parser = Parser'S String
-- type Parser a = Parser'S C.ByteString a

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile parser file = do
  stream <- readFile file
  let state = initState file stream
  return . parse parser $ state


---------------------
-- primitive parser
---------------------

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


char :: Stream s => Char -> Parser'S s Char
char c = charParserOf (== c) <?> show [c]

anyChar :: Stream s => Parser'S s Char
anyChar = charParserOf (const True)

anyCharBut :: Stream s => Char -> Parser'S s Char
anyCharBut c = charParserOf (/= c)

-- string :: Stream s => String -> Parser'S s a
-- string []       = undefined

digit :: Stream s => Parser'S s Char
digit = charParserOf Data.Char.isDigit <?> "digit"

letter :: Stream s => Parser'S s Char
letter = charParserOf isAlpha <?> "letter"

alphaNum :: Stream s => Parser'S s Char
alphaNum = charParserOf isAlphaNum <?> "letter-or-digit"

hex :: Stream s => Parser'S s Char
hex = charParserOf isHexDigit <?> "hex-string"

lower :: Stream s => Parser'S s Char
lower = charParserOf isLower <?> "lowercase-letter"

upper :: Stream s => Parser'S s Char
upper = charParserOf isUpper <?> "uppercase-letter"

tab :: Stream s => Parser'S s Char
tab = char '\t' <?> "tab"

lf :: Stream s => Parser'S s Char
lf = char '\n' <?> "linefeed"

crlf :: Stream s => Parser'S s Char
crlf = char '\r' >> char '\n' <?> "carrige-return + linefeed"

eol :: Stream s => Parser'S s Char
eol = lf <|> crlf <?> "end-of-line"

space :: Stream s => Parser'S s Char
space = charParserOf isSpace <?> "space"

-- spaces

oneOf :: Stream s => [Char] -> Parser'S s Char
oneOf cs = charParserOf (`elem` cs) <?> label'oneof
  where label'oneof = unwords ["one of", show ((: []) <$> cs)]

noneOf :: Stream s => [Char] -> Parser'S s Char
noneOf cs = charParserOf (`notElem` cs) <?> label'noneof
  where label'noneof = unwords ["none of", show ((: []) <$> cs)]

-- many :: Parser'S s a -> Parser'S s [a]
-- many parser = undefined

---------------------
-- combinators
---------------------

choice :: Stream s => [Parser'S s a] -> Parser'S s a
choice = foldl (<|>) mzero

fallback :: Stream s => a -> Parser'S s a -> Parser'S s a
fallback x parser = parser <|> return x

optional :: Stream s => Parser'S s a -> Parser'S s ()
optional parser =
  do
    _ <- parser
    return ()
  <|> return ()

---------------------
-- debug section
---------------------

fakeSource :: Source
fakeSource = initSource "/fake/path/fakeName.s"

fakeErrorMessages :: [Message]
fakeErrorMessages =
  [ "Error: The Sun has become a blackhole."
  , "Error: The Riemann conjecture has just proved."
  ]

fakeError :: ParseError
fakeError = ParseError fakeSource fakeErrorMessages


testStream = [r|
  public class Simple {

    /* block comment
    */

    public static void main(String[] args) { /* Wow  */
        System.out.println("get/*ting better");
    }
    // line comment
}
|]


testSource = initSource "simple.java"

testErrors = replicate 3 fakeError

testState = State testStream testSource testErrors

debugs s = State s fakeSource []
