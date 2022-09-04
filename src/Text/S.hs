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

import           Control.DeepSeq                ( NFData )
import           Text.S.Base
import           Text.S.Combinator
import           Text.S.Expr
import           Text.S.Internal
import           Text.S.Language
import           Text.S.Lexeme


-- | Parser using String or [Char]
type Parser = ParserS String

-- | Parser using ByteString in Data.ByteString.Char8
type Parser'B = ParserS ByteString

-- | Parser using Lazy ByteString in Data.ByteString.Lazy.Char8
type Parser'BL = ParserS LazyByteString

-- | Parser using Text in Data.Text
type Parser'T = ParserS Text

-- | Parser using Text in Data.Text.Lazy
type Parser'TL = ParserS LazyText


-- | tests parsers from files (will be erased)
tf :: Parser a -> IO (Result a String)
tf parser = do
  let file = "simple.java"
  s <- readStream file
  let state = initState file s
  return . parse parser $ state

-- | tests lexeme parsers
tl :: (LanguageDef -> ParserS String a) -> String -> Result a String
tl l = t (l defDef)

-- |
calc'bl :: (Stream s, NFData s) => ParserS s Double
calc'bl = expr atom table
 where
  atom = strip float <|> parens calc'bl
  table =
    [ [prefixU "-" negate, prefixU "+" id]
    , [postfixU "++" (+ 1), postfixU "--" (subtract 1)]
    , [infixL "^" (**)]
    , [infixL "*" (*), infixL "/" (/)]
    , [infixL "+" (+), infixL "-" (-)]
    ]

calc'br :: (Stream s, NFData s) => ParserS s Double
calc'br = expr atom table
 where
  atom = strip float <|> parens calc'br
  table =
    [ [prefixU "-" negate]
    , [postfixU "++" (+ 1), postfixU "--" (subtract 1)]
    , [infixR "^" (**)]
    , [infixR "*" (*), infixR "/" (/)]
    , [infixR "+" (+), infixR "-" (-)]
    ]

calc'bp :: (Stream s, NFData s) => ParserS s Double
calc'bp = expr atom table
 where
  atom = strip float
  table =
    [ [ prefixB "^" (**)
      , prefixB "*" (*)
      , prefixB "/" (/)
      , prefixB "+" (+)
      , prefixB "-" (-)
      ]
    ]

calc'bq :: (Stream s, NFData s) => ParserS s Double
calc'bq = expr atom table
 where
  atom = strip float
  table =
    [ [ postfixB "^" (**)
      , postfixB "*" (*)
      , postfixB "/" (/)
      , postfixB "+" (+)
      , postfixB "-" (-)
      ]
    ]

calc :: (Stream s, NFData s) => ParserS s Double
calc = expr atom table
 where
  atom  = strip float <|> parens calc
  table = [[infixR "+" (+)], [infixL "*" (*)]]
