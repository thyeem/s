{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
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
--
-----------------------------------------------------------------------------

module Text.S.Internal
  ( Parser
  , Text
  , ByteString
  , LazyText
  , LazyByteString
  , Stream(..)
  , Source(..)
  , initSource
  , Message(..)
  , Result(..)
  , State(..)
  , initState
  , ParserS(..)
  , ($>)
  , void
  , (<|>)
  , (<?>)
  , label
  , stream
  , state
  , try
  , ahead
  , charParserOf
  , parse
  , parse'
  , parseFile
  , t
  , tt
  , ts
  , unwrap
  , error'
  , CondExpr(..)
  , (?)
  , Pretty(..)
  ) where

import           Control.Applicative            ( Alternative(..)
                                                , liftA2
                                                )
import           Control.Monad                  ( MonadPlus(..) )
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.List                      ( intercalate
                                                , uncons
                                                )
import           Data.Proxy
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TLIO
import           GHC.Generics                   ( Generic )
import           System.IO                      ( readFile )
import           Text.Pretty.Simple             ( pShowNoColor )




-- | ParserS currently supports stream types the following:
--
-- * 'Text'
-- * 'LazyText'
-- * 'String'
-- * 'ByteString'
-- * 'LazyByteString'.
--
-- By default, the 'ParserS' 'Stream' is set to 'Text'.
-- Choose a stream type according to your preference:
--
-- * Set the parser stream to 'LazyText' comes from 'Data.Text.Lazy'
--
-- >>> type Parser = ParserS LazyText
--
-- * Set the parser stream to 'String' or ['Char']
--
-- >>> type Parser = ParserS String
--
-- * Set the parser stream to 'ByteString' comes from 'Data.ByteString.Char8'
--
-- >>> type Parser = ParserS ByteString
--
-- * Set the parser stream to 'LazyByteString' comes from 'Data.ByteString.Lazy.Char8'
--
-- >>> type Parser = ParserS LazyByteString
--
type Parser = ParserS Text

type Text = T.Text

type ByteString = C.ByteString

type LazyText = TL.Text

type LazyByteString = CL.ByteString


-------------------------
-- Stream
-------------------------
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


-------------------------
-- Source
-------------------------
data Source = Source
  { sourceName   :: FilePath
  , sourceLine   :: !Int
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
             deriving (Eq, Ord)

type Messages = [Message]


-------------------------
-- State
-------------------------
data State s = State
  { stateStream    :: s
  , stateSource    :: !Source
  , stateMesssages :: !Messages
  }
  deriving (Show, Eq)

initState :: FilePath -> s -> State s
initState file stream = State stream (initSource file) mempty

addMessage :: Message -> State s -> State s
addMessage msg state@State {..} =
  state { stateMesssages = stateMesssages <> [msg] }
{-# INLINE addMessage #-}


-------------------------
-- Result
-------------------------
data Result a s = Ok a !(State s)
                | Error !(State s)
                deriving (Show, Eq)


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
{-# INLINE label #-}


instance Functor (ParserS s) where
  fmap = smap
  {-# INLINE fmap #-}

  (<$) = fmap . const
  {-# INLINE (<$) #-}


smap :: (a -> b) -> ParserS s a -> ParserS s b
smap f parser =
  ParserS $ \state fOk fError -> runParser parser state (fOk . f) fError
{-# INLINE smap #-}


instance Applicative (ParserS s) where
  pure x = ParserS $ \state ok _ -> ok x state
  {-# INLINE pure #-}

  (<*>) = sap
  {-# INLINE (<*>) #-}

  liftA2 f x = (<*>) (fmap f $! x)
  {-# INLINE liftA2 #-}

  (<*) = liftA2 const
  {-# INLINE (<*) #-}


sap :: ParserS s (a -> b) -> ParserS s a -> ParserS s b
sap f parser = ParserS $ \state fOk fError ->
  let fOk' x state' = runParser parser state' (fOk . x) fError
  in  runParser f state fOk' fError
{-# INLINE sap #-}


instance Monad (ParserS s) where
  return = pure
  {-# INLINE return #-}

  (>>=) = sbind
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

sbind :: ParserS s a -> (a -> ParserS s b) -> ParserS s b
sbind parser f = ParserS $ \state fOk fError ->
  let fOk' x state' = runParser (f $! x) state' fOk fError
  in  runParser parser state fOk' fError
{-# INLINE sbind #-}


instance Alternative (ParserS s) where
  empty = mzero
  {-# INLINE empty #-}

  (<|>) = mplus
  {-# INLINE (<|>) #-}


instance MonadPlus (ParserS s) where
  mzero = szero
  {-# INLINE mzero #-}

  mplus = splus
  {-# INLINE mplus #-}


szero :: ParserS s a
szero = fail mempty
{-# INLINE szero #-}

splus :: ParserS s a -> ParserS s a -> ParserS s a
splus p q = ParserS $ \state fOk fError ->
  let fError' _ = runParser q state fOk fError in runParser p state fOk fError'
{-# INLINE splus #-}


instance MonadFail (ParserS s) where
  fail msg =
    ParserS $ \s@State{} _ fError -> fError $ addMessage (Normal msg) s
  {-# INLINE fail #-}

-- |
state :: Stream s => ParserS s (State s)
state = ParserS $ \state@State{} fOk _ -> fOk state state

-- |
stream :: Stream s => ParserS s s
stream = ParserS $ \state@(State stream _ _) fOk _ -> fOk stream state

-- |
-- take'while :: Stream s => (a -> Bool) -> ParserS s [a]
-- take'while p = ParserS $ \state@(State stream src msgs) fOk fError ->
-- let cs = takeWhile p stream
-- in  if null cs
-- then fError
-- $ addMessage (Unexpected $ unwords ["failed to match:"]) state
-- else undefined

-- | Tries to parse with @__parser__@ looking ahead without consuming any input.
--
-- In this case, not consuming any intut does not mean it does not fail at all.
-- Attempts to parse with the given parser, throwing an error if parsing fails.
--
-- See also 'ahead'
--
try :: ParserS s a -> ParserS s a
try parser = ParserS $ \state fOk fError ->
  let fOk' x _ = fOk x state in runParser parser state fOk' fError
{-# INLINE try #-}

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
{-# INLINE ahead #-}

-- | Gets a char parser that satisfies the given predicate @(Char -> Bool)@.
--
-- This function describes every parsing work at a fundamental level.
--
-- Here is where each parsing job starts.
--
charParserOf :: Stream s => (Char -> Bool) -> ParserS s Char
charParserOf p = ParserS $ \state@(State stream src msgs) fOk fError ->
  case unCons stream of
    Nothing ->
      fError $ addMessage (Unexpected "EOF: reached to end-of-stream") state
    Just (c, cs)
      | p c -> fOk c (State cs (jump src c) msgs)
      | otherwise -> fError $ addMessage
        (Unexpected $ unwords ["failed. got unexpected character:", show c])
        state
{-# INLINE charParserOf #-}

-- | Update the cursor location in the Source by character
jump :: Source -> Char -> Source
jump (Source n ln col) = \case
  '\n' -> Source n (ln + 1) 1
  '\t' -> Source n ln (move col 8)
  _    -> Source n ln (col + 1)
  where move col size = col + size - ((col - 1) `mod` size)
{-# INLINE jump #-}

-- | Takes a state and a parser, then parses it.
parse :: Stream s => ParserS s a -> State s -> Result a s
parse parser state = runParser parser state Ok Error

-- | The same as 'parse', but takes a stream instead of a state.
parse' :: Stream s => ParserS s a -> s -> Result a s
parse' parser s = parse parser (State s mempty mempty)

-- | The same as 'parse', but takes the stream from a given file
parseFile :: Stream s => ParserS s a -> FilePath -> IO (Result a s)
parseFile parser file = do
  stream <- readStream file
  let state = initState file stream
  return . parse parser $ state

-- | Tests parsers and its combinators with given stream
-- and then pretty-print the parse result.
t :: (Stream s, Pretty a) => ParserS s a -> s -> IO ()
t parser = pp . parse' parser

-- | The same as 't' but returns the parse result, 'Ok', or 'Error'
-- instead of pretty-printing it.
tt :: Stream s => ParserS s a -> s -> a
tt parser = unwrap . parse' parser

-- | The same as 't' but returns 'Stream' @s@ only
-- instead of pretty-printing it.
ts :: Stream s => ParserS s a -> s -> s
ts parser = sOnly . parse' parser
 where
  sOnly (Ok _ (State s _ _)) = s
  sOnly (Error state       ) = error' . show . stateMesssages $ state

-- | Unwraps 'Result' @a s@, then gets 'Ok' @ok@ or 'Error' @err@.
unwrap :: Stream s => Result a s -> a
unwrap = \case
  Ok ok _     -> ok
  Error state -> error' . show $ state

-- | Raise error without annoying stacktrace
error' :: String -> a
error' = errorWithoutStackTrace

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
-- Simple Pretty Print
-------------------------
-- | Pretty-Show and Pretty-Printer
class Show a => Pretty a where
  pretty :: a -> TL.Text
  pretty = pShowNoColor

  pp :: a -> IO ()
  pp = TLIO.putStrLn . pretty


instance Show Message where
  show = \case
    Unexpected msg -> msg
    Expected   msg -> msg
    Normal     msg -> msg


instance Pretty Source where
  pretty src@Source {..} = TL.unwords
    [ TL.pack sourceName
    , "(line"
    , TL.pack $ show sourceLine <> ","
    , "column"
    , TL.pack $ show sourceColumn <> "):"
    ]


instance {-# OVERLAPPING #-} Pretty Messages where
  pretty msgs = TL.pack $ intercalate "\n\t" (show <$> msgs)


instance Show s => Pretty (State s) where
  pretty state@State {..} = TL.unlines
    [ pretty stateSource
    , pretty stateMesssages
    , "remains: " <> TL.pack showStream
    ]
   where
    stream = show stateStream
    showStream | length stream < 200 = stream
               | otherwise           = reduceStream stateStream 60

    reduceStream stream n = intercalate
      "\n\t"
      [take n s, "", "... (omitted) ...", "", drop (length s - n) s]
      where s = show stream


instance (Show s, Pretty a) => Pretty (Result a s) where
  pretty = \case
    Ok ok s -> TL.unlines [pretty ok <> "\n", pretty s]
    Error s -> TL.unlines ["Error\n", pretty s]


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


instance Show a => Pretty [a] where
instance (Show a, Show b) => Pretty (a,b) where
instance (Show a, Show b, Show c) => Pretty (a,b,c) where
instance (Show a, Show b, Show c, Show d) => Pretty (a,b,c,d) where

deriving instance Pretty Bool
deriving instance Pretty Char
deriving instance Pretty ()
