-- |
-- Module      : Text.S.Lexer
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module is reponsible for providing a default lexer
module Text.S.Lexer
  ( defSpec
  , tidy
  , gap
  , cmtL
  , cmtB
  , token
  , symbol
  , parens
  , braces
  , angles
  , squares
  , identifier
  , operator
  , charLit
  , stringLit
  )
where

import Text.S.Base
import Text.S.Internal
import Text.S.Language
import Text.S.Lexeme

-- | Default Language specification
defSpec :: Stream s => LanguageSpec s
defSpec =
  LanguageSpec
    { caseSensitive = True
    , charLiteral = "'"
    , stringLiteral = "\""
    , commentBegin = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , idenBegin = alpha <|> char '_'
    , idenLetter = alphaNum <|> char '_'
    , reservedOps = []
    , reservedWords = []
    }

-- | Construct a lexer using the default language spec, 'defSpec'
lexer :: Stream s => Lexer s a
lexer = genLexer defSpec

-- | Default 'tidy' parser
--
-- >>> ta (tidy *> anystring) "// LINE \n /* BLOCK */ tidy-up"
-- "tidy-up"
--
-- >>> ta (tidy *> anystring) "stay-the-same"
-- "stay-the-same"
tidy :: Stream s => S s ()
tidy = tidy' lexer

-- | Default 'gap' parser
--
-- >>> ta (alphas *> gap *> identifier) "string /* COMMENT */ name;"
-- "name"
gap :: Stream s => S s ()
gap = gap' lexer

-- | Default 'cmtL' parser
--
-- >>> ta cmtL "// LINE \n"
-- " LINE "
cmtL :: Stream s => S s String
cmtL = cmtL' lexer

-- | Default 'cmtB' parser
--
-- >>> ta cmtB "/* BLOCK */"
-- " BLOCK "
cmtB :: Stream s => S s String
cmtB = cmtB' lexer

-- | Default 'token' parser
--
-- >>> iden = token identifier
-- >>> ta (iden *> iden) "int number;"
-- "number"
token :: Stream s => S s a -> S s a
token = token' lexer

-- | Default 'symbol' parser
--
-- >>> iden = token identifier
-- >>> ta (iden *> iden *> symbol ";") "int number;"
-- ";"
symbol :: Stream s => String -> S s String
symbol = symbol' lexer

-- | Default 'parens' parser
--
-- >>> ta (parens alphas) "(parser)"
-- "parser"
parens :: Stream s => S s a -> S s a
parens = parens' lexer

-- | Default 'braces' parser
--
-- >>> ta (braces alphas) "{parser}"
-- "parser"
braces :: Stream s => S s a -> S s a
braces = braces' lexer

-- | Default 'angles' parser
--
-- >>> ta (angles alphas) "<parser>"
-- "parser"
angles :: Stream s => S s a -> S s a
angles = angles' lexer

-- | Default 'squares' parser
--
-- >>> ta (squares alphas) "[parser]"
-- "parser"
squares :: Stream s => S s a -> S s a
squares = squares' lexer

-- | Default 'identifier' parser
--
-- >>> ta identifier "int number;"
-- "int"
identifier :: Stream s => S s String
identifier = identifier' lexer

-- | Default 'operator' parser
--
-- >>> ta (digits *> tidy *> operator) "3 + 4"
-- "+"
operator :: Stream s => S s String
operator = operator' lexer

-- | Default 'charLit' parser
--
-- >>> stream = "'\r', a carriage-return or '\n', a line-feed?"
-- >>> ta charLit stream
-- '\r'
charLit :: Stream s => S s Char
charLit = charLit' lexer

-- | Default 'stringLit' parser
--
-- >>> stream = "\"'\\r', a carriage-return or '\\n', a line-feed?\""
-- >>> ta stringLit stream
-- "'\r', a carriage-return or '\n', a line-feed?"
stringLit :: Stream s => S s String
stringLit = stringLit' lexer
