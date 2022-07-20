-----------------------------------------------------------------------------
-- |
-- Module      : Text.S
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Parser S is a generalized parser combinator easy-to-read.
-- Intensively focused on usability, scalability and non-verbosity.
--
-----------------------------------------------------------------------------

module Text.S
  ( module Text.S.Internal
  , module Text.S.Combinator
  , module Text.S.Token
  , module Text.S.Language
  , module Text.S
  ) where

import           Text.S.Combinator
import           Text.S.Internal
import           Text.S.Language
import           Text.S.Token


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


-----------------------------------
-- debug section (will be erased)
-----------------------------------

testFromFile :: Parser a -> IO (Return a String)
testFromFile parser = do
  let file = "simple.java"
  s <- readStream file
  let state = initState file s
  return . parse parser $ state

test :: Parser a -> String -> Return a String
test = parseTest
