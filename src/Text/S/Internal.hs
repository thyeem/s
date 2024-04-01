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
  , Error
  , Result (..)
  , State (..)
  , S (..)
  , void
  , (<?>)
  , initState
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

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (MonadPlus (..))
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Functor (void, (<&>))
import Data.List (intercalate, uncons)
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
  takeStream :: (Char -> Bool) -> s -> [Char]
  takeStream p s = case unCons s of
    Just (c, cs) | p c -> c : takeStream p cs
    _ -> mempty
  {-# MINIMAL unCons, readStream, isEmpty #-}

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
  s1 <> s2
    | sourceName s1 /= sourceName s2 = die "two source names do not match"
    | s1 < s2 = s2
    | otherwise = s1
  {-# INLINE (<>) #-}

instance Monoid Source where
  mempty = Source mempty 1 1
  {-# INLINE mempty #-}

updateSrc :: Source -> Char -> Source
updateSrc src@Source {..} = \case
  '\n' -> src {sourceLine = sourceLine + 1, sourceColumn = 1}
  '\t' -> src {sourceColumn = move sourceColumn 4}
  _ -> src {sourceColumn = sourceColumn + 1}
 where
  move col size = col + size - ((col - 1) `mod` size)

type Error = String

joinError :: State s -> Error -> State s
joinError state@State {..} error = state {stateErrors = stateErrors ++ [error]}

data State s = State
  { stateStream :: s
  , stateSource :: !Source
  , stateErrors :: ![Error]
  , stateBuffer :: Text
  }
  deriving (Show, Eq)

-- | Initialize state
initState :: Stream s => String -> s -> State s
initState srcName s =
  State s (mempty {sourceName = srcName}) mempty (updateBuf mempty s)

updateBuf :: Stream s => Text -> s -> Text
updateBuf buf s = case unCons s of
  Just (c, cs)
    | c == '\n' || isEmpty buf -> T.pack $ takeStream (/= '\n') s
    | otherwise -> buf
  _ -> mempty

data Result a s
  = Ok !a !(State s)
  | Err !(State s)
  deriving (Show, Eq)

-- | Defines a monad transformer parser, 'S'
--
-- It self-describes the outline of the parsing process this parser does.
newtype S s a = S
  { unS
      :: forall b
       . State s -- state including stream input
      -> (a -> State s -> b) -- call @Ok@ when somthing comsumed
      -> (State s -> b) -- call @Err@ when nothing consumed
      -> b
  }

-- | Infix operator of flipped 'label'
(<?>) :: S s a -> String -> S s a
(<?>) = flip label

infixr 0 <?>

label :: String -> S s a -> S s a
label desc p = S $ \state fOk fErr ->
  let fErr' s
        | null desc = fErr s
        | otherwise = fErr . joinError s $ unwords ["->", "expected:", desc]
   in unS p state fOk fErr'
{-# INLINE label #-}

instance Functor (S s) where
  fmap = smap
  {-# INLINE fmap #-}

  (<$) = fmap . const
  {-# INLINE (<$) #-}

smap :: (a -> b) -> S s a -> S s b
smap f p = S $ \state fOk fErr -> unS p state (fOk . f) fErr
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
sap f p = S $ \state fOk fErr ->
  let fOk' x state' = unS p state' (fOk . x) fErr
   in unS f state fOk' fErr
{-# INLINE sap #-}

instance Monad (S s) where
  return = pure
  {-# INLINE return #-}

  (>>=) = sbind
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

sbind :: S s a -> (a -> S s b) -> S s b
sbind p f = S $ \state fOk fErr ->
  let fOk' x state' = unS (f x) state' fOk fErr
   in unS p state fOk' fErr
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
splus p q = S $ \state fOk fErr ->
  let fErr' state' =
        let fErr'' state'' = fErr (merge state' state'')
         in unS q state fOk fErr''
   in unS p state fOk fErr'
 where
  merge s1@State {stateSource = src1} s2@State {stateSource = src2}
    | src1 > src2 = s1
    | src1 < src2 = s2
    | otherwise = s1 {stateErrors = stateErrors s1 ++ stateErrors s2}
{-# INLINE splus #-}

instance MonadFail (S s) where
  fail error =
    S $ \state _ fErr -> fErr $ joinError state error
  {-# INLINE fail #-}

-- | Tries to parse with @__p__@ looking ahead without consuming any input.
--
-- If parsing fails, an error is raised even if no input is consumed.
try :: S s a -> S s a
try p = S $ \state fOk fErr ->
  let fOk' x _ = fOk x state in unS p state fOk' fErr
{-# INLINE try #-}

-- | Tries to parse with @__p__@ looking ahead without consuming any input.
--
-- Succeeds if the given parser does not match the next input.
-- Otherwise raises an error.
forbid :: Show a => S s a -> S s ()
forbid p = S $ \state fOk fErr ->
  let fOk' a _ =
        let error = unwords ["Error, found forbidden token:", show a]
         in fErr $ joinError state error
      fErr' _ = fOk () state
   in unS p state fOk' fErr'
{-# INLINE forbid #-}

-- | Gets a char parser that satisfies the given predicate @(Char -> Bool)@.
--
-- This function describes every parsing work at a fundamental level.
--
-- Here is where each parsing job starts.
charBy :: Stream s => (Char -> Bool) -> S s Char
charBy p = S $ \state@(State s src msgs buf) fOk fErr ->
  case unCons s of
    Nothing -> fErr state
    Just (c, cs)
      | p c -> fOk c (State cs (updateSrc src c) msgs (updateBuf buf s))
      | otherwise ->
          fErr . joinError state $
            unwords ["Error, got unexpected token:", show c]
{-# INLINE charBy #-}

-- | Takes a state and a parser, then parses it.
parse :: Stream s => S s a -> State s -> Result a s
parse p state = unS p state Ok Err

-- | The same as 'parse', but takes a in-memory stream instead of a state.
parse' :: Stream s => S s a -> s -> Result a s
parse' p s = parse p (initState "<in-memory>" s)

-- | The same as 'parse', but takes the stream from a given file
parseFile :: Stream s => S s a -> FilePath -> IO (Result a s)
parseFile p file = readStream file <&> parse p . initState file

-- | Tests parsers and its combinators with given stream, then print it.
t :: (Stream s, Pretty s, Pretty a) => S s a -> s -> IO ()
t p = pp . parse' p

-- | The same as 't' but returns the unwrapped 'Result' @a@, 'Ok', or 'Err'
ta :: Stream s => S s a -> s -> a
ta p = unwrap . parse' p
 where
  unwrap = \case
    Ok ok _ -> ok
    Err state -> die . TL.unpack . pretty . stateErrors $ state

-- | Tries to parse with 'parser' then returns 'Stream' @s@
ts :: Stream s => S s a -> s -> s
ts p = stateStream . state . parse' p
 where
  state = \case
    Ok _ s -> s
    Err s -> s

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

instance Pretty Source where
  pretty Source {..} =
    TL.unwords
      [ TL.pack sourceName
      , TL.pack "(line"
      , TL.pack $ show sourceLine ++ ","
      , "column"
      , TL.pack $ show sourceColumn ++ ")"
      ]

instance {-# OVERLAPPING #-} Pretty [Error] where
  pretty msgs = TL.pack $ concatMap ("\n\t" ++) msgs

instance (Show s, Pretty s) => Pretty (State s) where
  pretty s@State {..} =
    TL.unlines
      [ "Source: "
      , pretty stateSource
      , mempty
      , "Errors: "
      , pretty $ filter (not . null) stateErrors
      , mempty
      , "Stream remaining (limited to 80 chars): "
      , TL.take 80 (pretty stateStream)
      ]

-- | Print where errors occurred using caret (^)
caretError :: State s -> String
caretError State {stateSource = Source {..}, ..} =
  intercalate "\n" [x, o ++ T.unpack stateBuffer, x ++ caret]
 where
  x = replicate ((length . show $ sourceLine) + 1) ' ' ++ " | "
  o = concat [" ", show sourceLine, " | "]
  caret = replicate (sourceColumn - 1) ' ' ++ "^"

-- | Print the number of chars remaining in the given stream
charsLeft :: (Show s, Pretty s) => s -> String
charsLeft s
  | len == 0 = remains
  | otherwise = remains ++ cut ++ ":\n\t" ++ take 80 (show s)
 where
  len = subtract 2 . TL.length $ pretty s
  remains = concat ["remains ", show len, " char(s)"]
  cut = if len > 80 then " (first 80 chars only)" else mempty

instance (Show s, Pretty s, Pretty a) => Pretty (Result a s) where
  pretty = \case
    Ok ok s@State {..} ->
      TL.unlines
        [ pretty ok
        , pretty stateSource
        , mempty
        , pretty $ charsLeft stateStream
        ]
    Err s@State {..} ->
      TL.unlines
        [ pretty stateSource
        , TL.pack $ caretError s
        , pretty $ filter (not . null) stateErrors
        , mempty
        , TL.pack $ charsLeft stateStream
        ]

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
