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
  , module Text.S.Lexeme
  , module Text.S.Language
  , module Text.S.Expr
  , module Text.S
  ) where

import           Text.S.Base
import           Text.S.Combinator
import           Text.S.Expr
import           Text.S.Internal
import           Text.S.Language
import           Text.S.Lexeme


-- | ParserS currently supports stream types the following:
--
-- 'Text', 'LazyText', 'String', 'ByteString', and 'LazyByteString'.
--
-- By default, the Parser stream type is set to 'Text'.
-- Choose a stream type according to your preference like:
--
-- Set Parser stream to LazyText in Data.Text.Lazy
-- >>> type Parser = ParserS LazyText
--
-- Set Parser stream to String or [Char]
-- >>> type Parser= ParserS String
--
-- Set Parser stream to ByteString in Data.ByteString.Char8
-- >>> type Parser = ParserS ByteString
--
-- Set Parser stream to Lazy ByteString in Data.ByteString.Lazy.Char8
-- >>> type Parser = ParserS LazyByteString
--
type Parser = ParserS Text


-- | tests parsers from files (will be erased)
tf :: Parser a -> IO (Result a Text)
tf parser = do
  let file = "simple.java"
  s <- readStream file
  let state = initState file s
  return . parse parser $ state

-- | tests lexeme parsers
tl :: (LanguageDef -> Parser a) -> Text -> Result a Text
tl l = t (l defDef)
