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
  ( ByteString
  , LazyByteString
  , Text
  , LazyText
  , Stream(..)
  , Source(..)
  , initSource
  , Message(..)
  , Result(..)
  , State(..)
  , initState
  , ParserS(..)
  , (<|>)
  , (<?>)
  , label
  , parse
  , parse'
  , parseFromFile
  , charParserOf
  , ahead
  , assert
  , t
  , t'
  , ts'
  , unwrap
  , (?)
  , CondExpr(..)
  ) where

import           Control.Applicative            ( Alternative(..)
                                                , liftA2
                                                )
import           Control.DeepSeq                ( NFData
                                                , force
                                                )
import           Control.Monad                  ( MonadPlus(..) )
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.List                      ( intercalate
                                                , uncons
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TLIO
import           GHC.Generics                   ( Generic )
import           System.IO                      ( readFile )


type ByteString = C.ByteString

type LazyByteString = CL.ByteString

type Text = T.Text

type LazyText = TL.Text


-------------------------
-- Stream
-------------------------
class Stream s where
  unCons :: s -> Maybe (Char, s)
  readStream :: FilePath -> IO s

instance Stream ByteString where
  unCons     = C.uncons
  readStream = C.readFile

instance Stream LazyByteString where
  unCons     = CL.uncons
  readStream = CL.readFile

instance Stream Text where
  unCons     = T.uncons
  readStream = TIO.readFile

instance Stream LazyText where
  unCons     = TL.uncons
  readStream = TLIO.readFile

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
  mempty = Source "-" 1 1


appendSource :: Source -> Source -> Source
appendSource s1@Source{} s2@Source{}
  | sourceName s1 /= sourceName s2 = error' "two source names do not match"
  | s1 < s2                        = s2
  | otherwise                      = s1
{-# INLINE appendSource #-}

initSource :: FilePath -> Source
initSource file = Source file 1 1


-------------------------
-- Message
-------------------------
data Message = Unexpected !String
             | Expected !String
             | Normal !String
             deriving (Eq, Show, Ord, Generic, NFData)

type Messages = [Message]

message :: Message -> String
message msg = case msg of
  Unexpected msg -> msg
  Expected   msg -> msg
  Normal     msg -> msg


-------------------------
-- State
-------------------------
data State s = State
  { stateStream    :: s
  , stateSource    :: !Source
  , stateMesssages :: !Messages
  }
  deriving (Eq, Generic, NFData)

initState :: FilePath -> s -> State s
initState file stream = State stream (initSource file) mempty

addMessage :: Message -> State s -> State s
addMessage msg state@State {..} =
  state { stateMesssages = stateMesssages <> [msg] }
{-# INLINE addMessage #-}

mergeState :: State s -> State s -> State s
mergeState s1@(State _ src1 _) s2@(State _ src2 _) | src1 > src2 = s1
                                                   | otherwise   = s2
{-# INLINE mergeState #-}


-------------------------
-- Result
-------------------------
data Result a s = Ok a (State s)
                | Error (State s)
                deriving (Eq, Generic, NFData)


-------------------------
-- Parser S
-------------------------
-- Defines a monad transformer 'ParserS' and its accessor 'runParser'.
--
-- It self-describes the outline of the parsing process this parser does.
--
newtype ParserS s a = ParserS {
    runParser :: forall b.
      State s ->                   -- state including stream input
      (a -> State s -> b) ->       -- call @Ok@ when somthing comsumed
      (State s -> b) ->            -- call @Error@ when nothing consumed
      b
    }


-- | Infix operator of flipped 'label'
(<?>) :: ParserS s a -> String -> ParserS s a
(<?>) = flip label

infixr 0 <?>


-- |
label :: String -> ParserS s a -> ParserS s a
label msg parser = ParserS $ \state@State{} fOk fError ->
  let fError' s@State {..} = fError $ addMessage expected s
      expected = Expected . unwords $ ["->", "expected:", msg]
  in  runParser parser state fOk fError'
{-# INLINABLE label #-}


instance Functor (ParserS s) where
  fmap = smap


smap :: (a -> b) -> ParserS s a -> ParserS s b
smap f parser =
  ParserS $ \state fOk fError -> runParser parser state (fOk . f) fError
{-# INLINE smap #-}


instance Applicative (ParserS s) where
  pure x = ParserS $ \state ok _ -> ok x state
  (<*>) = sap


sap :: ParserS s (a -> b) -> ParserS s a -> ParserS s b
sap f parser = ParserS $ \state fOk fError ->
  let fOk' x state' = runParser parser state' (fOk . x) fError
  in  runParser f state fOk' fError
{-# INLINE sap #-}


instance Monad (ParserS s) where
  return = pure
  (>>=)  = sbind


sbind :: ParserS s a -> (a -> ParserS s b) -> ParserS s b
sbind parser f = ParserS $ \state fOk fError ->
  let fOk' x state' = runParser (f x) state' fOk fError
  in  runParser parser state fOk' fError
{-# INLINE sbind #-}


instance Alternative (ParserS s) where
  empty = mzero
  (<|>) = mplus


instance MonadPlus (ParserS s) where
  mzero = szero
  mplus = splus


szero :: ParserS s a
szero = fail mempty
{-# INLINE szero #-}

splus :: ParserS s a -> ParserS s a -> ParserS s a
splus p q = ParserS $ \state fOk fError ->
  let fError' s'@State{} =
        let fError'' s''@State{} = fError $ mergeState s' s''
        in  runParser q state fOk fError''
  in  runParser p state fOk fError'
{-# INLINE splus #-}


instance MonadFail (ParserS s) where
  fail msg =
    ParserS $ \s@State{} _ fError -> fError $ addMessage (Normal msg) s


-- | Takes state and parser, then parses it.
parse :: ParserS s a -> State s -> Result a s
parse parser state = runParser parser state Ok Error

-- | The same as 'parse', but unwrap the @Result@ of the parse result
parse' :: (Stream s, Show s) => ParserS s a -> State s -> a
parse' parser = unwrap . parse parser

-- | The same as 'parse', but takes the stream from a given file
parseFromFile :: Stream s => ParserS s a -> FilePath -> IO (Result a s)
parseFromFile parser file = do
  stream <- readStream file
  let state = initState file stream
  return . parse parser $ state

-- | Gets a char parser that satisfies the given predicate.
--
-- This function describes every parsing work at a fundamental level.
--
-- Here is where each parsing job starts.
--
charParserOf :: (Stream s, NFData s) => (Char -> Bool) -> ParserS s Char
charParserOf predicate =
  ParserS $ \state@(State stream src msgs) fOk fError -> case unCons stream of
    Nothing ->
      fError $ addMessage (Unexpected "EOF: reached to end-of-stream") state
    Just (c, cs)
      | predicate c -> fOk c state'
      | otherwise -> fError $ addMessage
        (Unexpected $ unwords ["failed. got unexpected character:", show c])
        state
     where
      state' = force $ State cs src' msgs
      src'   = force $ jump src c
      jump (Source n ln col) c = case c of
        '\n' -> Source n (ln + 1) 1
        '\t' -> Source n ln (move col 8)
        _    -> Source n ln (col + 1)
        where move col size = col + size - ((col - 1) `mod` size)
{-# INLINABLE charParserOf #-}


-- | Tries to parse with @__parser__@ looking ahead without consuming any input.
-- This returns if the next parsing is successful instead of the parse result.
--
-- If succeeds then returns @__True__@, otherwise returns @__False__@.
--
-- See also 'assert'
--
ahead :: ParserS s a -> ParserS s Bool
ahead parser = ParserS $ \state fOk fError ->
  let fOk' x _ = fOk True state
      fError' _ = fOk False state
  in  runParser parser state fOk' fError'

-- | Tries to parse with @__parser__@ looking ahead without consuming any input.
--
-- In this case, not consuming any intut does not mean it does not fail at all.
-- Attempts to parse with the given parser, throwing an error if parsing fails.
--
-- See also 'ahead'
--
assert :: ParserS s a -> ParserS s a
assert parser = ParserS $ \state fOk fError ->
  let fOk' x _ = fOk x state in runParser parser state fOk' fError

-- | Tests parsers and its combinators with given strings
t :: ParserS String a -> String -> Result a String
t parser s = parse parser (State s mempty [])

-- | The same as 't', but unwrap the @Result@ of the parse result
t' :: ParserS String a -> String -> a
t' parser = unwrap . t parser

-- | The same as 't', but unwraps @Result a s@ to get the state @s@ only.
ts' :: ParserS String a -> String -> String
ts' parser = sOnly . t parser
 where
  sOnly (Ok _ (State s _ _)) = s
  sOnly (Error state       ) = error' . show . stateMesssages $ state

-- | Unwraps @Result a s@, then return the result @a@ only
unwrap :: (Stream s, Show s) => Result a s -> a
unwrap r = case r of
  Ok ok _     -> ok
  Error state -> error' . show $ state


-- | Conditional expression of if-then-else
data CondExpr a = a ::: a

infixl 1 ?
infixl 2 :::

-- | Tenary operator
--
-- (bool condition) ? (expression-if-true) ::: (expression-if-false)
(?) :: Bool -> CondExpr a -> a
True  ? (x ::: _) = x
False ? (_ ::: y) = y


-------------------------
-- Show instances
-------------------------
instance Show Source where
  show src@Source {..} = unwords
    [ "\n"
    , sourceName
    , "(line"
    , show sourceLine <> ","
    , "column"
    , show sourceColumn <> "):"
    ]


instance (Stream s, Show s) => Show (State s) where
  show state@State {..} = join'nn
    [ join'nt $ show stateSource : (message <$> stateMesssages)
    , join'nt ["remains:", showStream]
    ]
   where
    stream = show stateStream
    showStream | length stream < 200 = stream
               | otherwise           = reduceStream stateStream 60


instance (Stream s, Show s, Show a) => Show (Result a s) where
  show r = case r of
    Ok ok s -> join'n ["Ok " <> show ok, show s]
    Error s -> join'n ["Error", show s]


-- |
join'n :: [String] -> String
join'n = intercalate "\n"

-- |
join'nn :: [String] -> String
join'nn = intercalate "\n\n"

-- |
join'nt :: [String] -> String
join'nt = intercalate "\n\t"

-- |
reduceStream :: (Stream s, Show s) => s -> Int -> String
reduceStream stream n = join'nt
  [take n s, "", "... (omitted) ...", "", drop (length s - n) s]
  where s = show stream

-- |
error' :: String -> a
error' = errorWithoutStackTrace
