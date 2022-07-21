-----------------------------------------------------------------------------
-- |
-- Module      : Text.S
-- Description : Parser S is a generalized parser combinator easy-to-read.
--               Focused on usability, scalability and non-verbosity.
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-----------------------------------------------------------------------------

module Text.S
  ( module Text.S.Internal
  , module Text.S.Combinator
  , module Text.S.Lexer
  , module Text.S.Language
  , module Text.S
  ) where

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
