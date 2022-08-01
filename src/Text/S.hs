-----------------------------------------------------------------------------
-- |
-- Module      : Text.S
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- The parser-S is a generalized parser and its combinator easy-to-read.
-- It is designed to focused on usability, scalability and non-verbosity.
--
-----------------------------------------------------------------------------

module Text.S
  ( module Text.S.Base
  , module Text.S.Combinator
  , module Text.S.Internal
  , module Text.S.Lexer
  , module Text.S.Language
  , module Text.S
  ) where

import           Text.S.Base
import           Text.S.Combinator
import           Text.S.Internal
import           Text.S.Language
import           Text.S.Lexer


-- | Parser using String or [Char]
type Parser = Parser'S String

-- | Parser using ByteString in Data.ByteString.Char8
type ParserB = Parser'S ByteString'

-- | Parser using Lazy ByteString in Data.ByteString.Lazy.Char8
type ParserB' = Parser'S LazyByteString'

-- | Parser using Text in Data.Text
type ParserT = Parser'S Text'

-- | Parser using Text in Data.Text.Lazy
type ParserT' = Parser'S LazyText'


-- | test function (will be erased)
tf :: Parser a -> IO (Return a String)
tf parser = do
  let file = "simple.java"
  s <- readStream file
  let state = initState file s
  return . parse parser $ state

-----------------------------------------------------

tp :: Parser'S String a -> String -> Return a String
tp = t

tl :: (LanguageDef -> Parser'S String a) -> String -> Return a String
tl l = t (l defaultDef)

q = do
  _   <- token' "(" defaultDef
  str <- identifier' defaultDef
  _   <- token' ")" defaultDef
  return str

m :: Return String String
m = tp q "( '''Hahaha''' francis    )"
