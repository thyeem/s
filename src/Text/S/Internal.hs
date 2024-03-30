{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Text.S.Internal
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module implements internal process the parser really does.
--
-- One of core works is to design the parser as a Monad instance
-- while defining several primitive data types surrounding it.
module Text.S.Internal
  ( Parser
  , Text
  , ByteString
  , LazyText
  , LazyByteString
  , Stream (..)
  , Source (..)
  , initSource
  , Message (..)
  , Result (..)
  , State (..)
  , initState
  , S (..)
  , void
  , (<?>)
  , label
  , try
  , forbid
  , charBy
  , parse
  , parse'
  , parseFile
  , t
  , ta
  , ts
  , die
  , liftA2
  , (<|>)
  , CondExpr (..)
  , (?)
  , Pretty (..)
  )
where

import Control.Applicative
  ( Alternative (..)
  , liftA2
  )
import Control.Monad (MonadPlus (..))
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Functor (void)
import Data.List
  ( intercalate
  , uncons
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Text.Pretty.Simple (pShowNoColor)

-- | Parser S currently supports stream types the following:
--
-- * 'Text'
-- * 'LazyText'
-- * 'String'
-- * 'ByteString'
-- * 'LazyByteString'.
--
-- By default, 'Stream' of Parser 'S' is set to 'Text'.
-- Choose a stream type according to your preference:
--
-- * Set the parser stream to 'LazyText' comes from 'Data.Text.Lazy'
--
-- >>> type Parser = S LazyText
--
-- * Set the parser stream to 'String' or ['Char']
--
-- >>> type Parser = S String
--
-- * Set the parser stream to 'ByteString' comes from 'Data.ByteString.Char8'
--
-- >>> type Parser = S ByteString
--
-- * Set the parser stream to 'LazyByteString' comes from 'Data.ByteString.Lazy.Char8'
--
-- >>> type Parser = S LazyByteString
type Parser = S Text

type Text = T.Text

type ByteString = C.ByteString

type LazyText = TL.Text

type LazyByteString = CL.ByteString

class Show s => Stream s where
  unCons :: s -> Maybe (Char, s)
  readStream :: FilePath -> IO s
  isEmpty :: s -> Bool

instance Stream ByteString where
  unCons = C.uncons
  {-# INLINE unCons #-}

  readStream = C.readFile
  {-# INLINE readStream #-}

  isEmpty = C.null
  {-# INLINE isEmpty #-}

instance Stream LazyByteString where
  unCons = CL.uncons
  {-# INLINE unCons #-}

  readStream = CL.readFile
  {-# INLINE readStream #-}

  isEmpty = CL.null
  {-# INLINE isEmpty #-}

instance Stream Text where
  unCons = T.uncons
  {-# INLINE unCons #-}

  readStream = TIO.readFile
  {-# INLINE readStream #-}

  isEmpty = T.null
  {-# INLINE isEmpty #-}

instance Stream LazyText where
  unCons = TL.uncons
  {-# INLINE unCons #-}

  readStream = TLIO.readFile
  {-# INLINE readStream #-}

  isEmpty = TL.null
  {-# INLINE isEmpty #-}

instance Stream String where
  unCons = uncons
  {-# INLINE unCons #-}

  readStream = readFile
  {-# INLINE readStream #-}

  isEmpty = null
  {-# INLINE isEmpty #-}

data Source = Source
  { sourceName :: !FilePath
  , sourceLine :: !Int
  , sourceColumn :: !Int
  }
  deriving (Show, Eq, Ord)

instance Semigroup Source where
  (<>) = appendSource
  {-# INLINE (<>) #-}

instance Monoid Source where
  mempty = Source "-" 1 1
  {-# INLINE mempty #-}

appendSource :: Source -> Source -> Source
appendSource s1@Source {} s2@Source {}
  | sourceName s1 /= sourceName s2 = die "two source names do not match"
  | s1 < s2 = s2
  | otherwise = s1
{-# INLINE appendSource #-}

initSource :: FilePath -> Source
initSource file = Source file 1 1

data Message
  = Unexpected !String
  | Expected !String
  | Normal !String
  deriving (Eq, Ord)

type Messages = [Message]

data State s = State
  { stateStream :: s
  , stateSource :: !Source
  , stateMesssages :: !Messages
  }
  deriving (Show, Eq)

initState :: FilePath -> s -> State s
initState file stream = State stream (initSource file) mempty

addMessage :: Message -> State s -> State s
addMessage msg state@State {..} =
  state {stateMesssages = stateMesssages <> [msg]}
{-# INLINE addMessage #-}

data Result a s
  = Ok !a !(State s)
  | Error !(State s)
  deriving (Show, Eq)

-- | Defines a monad transformer parser, 'S'
--
-- It self-describes the outline of the parsing process this parser does.
newtype S s a = S
  { unS
      :: forall b
       . State s -- state including stream input
      -> (a -> State s -> b) -- call @Ok@ when somthing comsumed
      -> (State s -> b) -- call @Error@ when nothing consumed
      -> b
  }

-- | Infix operator of flipped 'label'
(<?>) :: S s a -> String -> S s a
(<?>) = flip label

infixr 0 <?>

label :: String -> S s a -> S s a
label desc p = S $ \state@State {} fOk fError ->
  let fError' s@State {} = fError $ addMessage msg s
      msg = Expected . unwords $ ["->", "expected:", desc]
   in unS p state fOk fError'
{-# INLINE label #-}

instance Functor (S s) where
  fmap = smap
  {-# INLINE fmap #-}

  (<$) = fmap . const
  {-# INLINE (<$) #-}

smap :: (a -> b) -> S s a -> S s b
smap f p = S $ \state fOk fError -> unS p state (fOk . f) fError
{-# INLINE smap #-}

instance Applicative (S s) where
  pure x = S $ \state fOk _ -> fOk x state
  {-# INLINE pure #-}

  (<*>) = sap
  {-# INLINE (<*>) #-}

  liftA2 f x = (<*>) (fmap f x)
  {-# INLINE liftA2 #-}

  (<*) = liftA2 const
  {-# INLINE (<*) #-}

sap :: S s (a -> b) -> S s a -> S s b
sap f p = S $ \state fOk fError ->
  let fOk' x state' = unS p state' (fOk . x) fError
   in unS f state fOk' fError
{-# INLINE sap #-}

instance Monad (S s) where
  return = pure
  {-# INLINE return #-}

  (>>=) = sbind
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

sbind :: S s a -> (a -> S s b) -> S s b
sbind p f = S $ \state fOk fError ->
  let fOk' x state' = unS (f x) state' fOk fError
   in unS p state fOk' fError
{-# INLINE sbind #-}

instance Alternative (S s) where
  empty = mzero
  {-# INLINE empty #-}

  (<|>) = mplus
  {-# INLINE (<|>) #-}

instance MonadPlus (S s) where
  mzero = szero
  {-# INLINE mzero #-}

  mplus = splus
  {-# INLINE mplus #-}

szero :: S s a
szero = fail mempty
{-# INLINE szero #-}

splus :: S s a -> S s a -> S s a
splus p q = S $ \state fOk fError ->
  let fError' _ = unS q state fOk fError in unS p state fOk fError'
{-# INLINE splus #-}

instance MonadFail (S s) where
  fail msg =
    S $ \state _ fError -> fError $ addMessage (Normal msg) state
  {-# INLINE fail #-}

-- | Tries to parse with @__p__@ looking ahead without consuming any input.
--
-- If parsing fails, an error is raised even if no input is consumed.
try :: S s a -> S s a
try p = S $ \state fOk fError ->
  let fOk' x _ = fOk x state in unS p state fOk' fError
{-# INLINE try #-}

-- | Tries to parse with @__p__@ looking ahead without consuming any input.
--
-- Succeeds if the given parser does not match the next input.
-- Otherwise raises an error.
forbid :: Show a => S s a -> S s ()
forbid p = S $ \state fOk fError ->
  let fOk' a _ =
        let msg = Unexpected (unwords ["Error, found forbidden token:", show a])
         in fError $ addMessage msg state
      fError' _ = fOk () state
   in unS p state fOk' fError'
{-# INLINE forbid #-}

-- | Gets a char parser that satisfies the given predicate @(Char -> Bool)@.
--
-- This function describes every parsing work at a fundamental level.
--
-- Here is where each parsing job starts.
charBy :: Stream s => (Char -> Bool) -> S s Char
charBy p = S $ \state@(State stream src msgs) fOk fError ->
  case unCons stream of
    Nothing -> fError state
    Just (c, cs)
      | p c -> fOk c (State cs (jump src c) msgs)
      | otherwise ->
          fError $
            addMessage
              (Unexpected $ unwords ["Error, got unexpected character:", show c])
              state
{-# INLINE charBy #-}

-- | Update the cursor location in the Source by character
jump :: Source -> Char -> Source
jump (Source n ln col) = \case
  '\n' -> Source n (ln + 1) 1
  '\t' -> Source n ln (move col 4)
  _ -> Source n ln (col + 1)
 where
  move col size = col + size - ((col - 1) `mod` size)
{-# INLINE jump #-}

-- | Takes a state and a parser, then parses it.
parse :: Stream s => S s a -> State s -> Result a s
parse p state = unS p state Ok Error

-- | The same as 'parse', but takes a stream instead of a state.
parse' :: Stream s => S s a -> s -> Result a s
parse' p s = parse p (State s mempty mempty)

-- | The same as 'parse', but takes the stream from a given file
parseFile :: Stream s => S s a -> FilePath -> IO (Result a s)
parseFile p file = do
  stream <- readStream file
  let state = initState file stream
  return . parse p $ state

-- | Tests parsers and its combinators with given stream, then print it.
t :: (Stream s, Pretty s, Pretty a) => S s a -> s -> IO ()
t p = pp . parse' p

-- | The same as 't' but returns the unwrapped 'Result' @a@, 'Ok', or 'Error'
ta :: Stream s => S s a -> s -> a
ta p = unwrap . parse' p
 where
  unwrap = \case
    Ok ok _ -> ok
    Error state -> die . TL.unpack . pretty . stateMesssages $ state

-- | Tries to parse with 'parser' then returns 'Stream' @s@
ts :: Stream s => S s a -> s -> s
ts p = stateStream . state . parse' p
 where
  state = \case
    Ok _ s -> s
    Error s -> s

-- | Raise error without annoying stacktrace
die :: String -> a
die = errorWithoutStackTrace

-- | Conditional expression of if-then-else
data CondExpr a = a ::: a

infixl 1 ?

infixl 2 :::

-- | Tenary operator
--
-- (bool condition) ? (expression-if-true) ::: (expression-if-false)
(?) :: Bool -> CondExpr a -> a
True ? (x ::: _) = x
False ? (_ ::: y) = y

-- | Pretty-Show and Pretty-Printer
class Show a => Pretty a where
  pretty :: a -> TL.Text
  pretty = pShowNoColor

  pp :: a -> IO ()
  pp = TLIO.putStrLn . pretty

instance Show Message where
  show = \case
    Unexpected msg -> msg
    Expected msg -> msg
    Normal msg -> msg

instance Pretty Source where
  pretty Source {..} =
    TL.unwords
      [ TL.pack sourceName
      , "(line"
      , TL.pack $ show sourceLine <> ","
      , "column"
      , TL.pack $ show sourceColumn <> "):"
      ]

instance {-# OVERLAPPING #-} Pretty Messages where
  pretty msgs = TL.pack $ intercalate "\n\t" (show <$> msgs)

instance (Show s, Pretty s) => Pretty (State s) where
  pretty State {..} =
    TL.unlines
      [pretty stateSource, pretty stateMesssages, "remains: ", pretty stateStream]

instance (Show s, Pretty s, Pretty a) => Pretty (Result a s) where
  pretty = \case
    Ok ok s -> TL.unlines [pretty ok <> "\n", pretty s]
    Error s -> TL.unlines ["\n", pretty s]

instance Pretty Int where
  pretty = TL.pack . show

instance Pretty Integer where
  pretty = TL.pack . show

instance Pretty Float where
  pretty = TL.pack . show

instance Pretty Double where
  pretty = TL.pack . show

instance Pretty Rational where
  pretty = TL.pack . show

instance Show a => Pretty [a]

instance (Show a, Show b) => Pretty (a, b)

instance (Show a, Show b, Show c) => Pretty (a, b, c)

instance (Show a, Show b, Show c, Show d) => Pretty (a, b, c, d)

deriving instance Pretty Bool

deriving instance Pretty Char

deriving instance Pretty ()

deriving instance Pretty Text

deriving instance Pretty ByteString

deriving instance Pretty LazyText

deriving instance Pretty LazyByteString
