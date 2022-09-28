-----------------------------------------------------------------------------
-- |
-- Module      : Text.S.Lexeme
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module constructs parsers for @__lexemes__@ or @__lexical units__@
-- that parses lexemes combinationg char parsers with parser combinators.
--
-----------------------------------------------------------------------------

module Text.S.Lexeme
  ( module Text.S.Lexeme
  ) where

import           Data.Char                      ( digitToInt
                                                , isAlpha
                                                , readLitChar
                                                , toLower
                                                , toUpper
                                                )
import           Data.List                      ( foldl'
                                                , foldl1'
                                                )
import qualified Data.Set                      as S
import           Text.S.Base
import           Text.S.Combinator
import           Text.S.Internal
import           Text.S.Language

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( MonadPlus(..)
                                                , mzero
                                                )
import           Data.Maybe                     ( fromMaybe )

-- |
lexeme :: Stream s => ParserS s a -> ParserS s a
lexeme p = p <* skip

-- | Parses any string symbol to comsume. The same as 'string'
symbol :: Stream s => String -> ParserS s String
symbol = string
{-# INLINE symbol #-}

-- |
letters :: (Stream s) => ParserS s String
letters = some letter
{-# INLINE letters #-}

-- |
alphaNums :: (Stream s) => ParserS s String
alphaNums = some alphaNum
{-# INLINE alphaNums #-}

-- |
digits :: (Stream s) => ParserS s String
digits = some digit
{-# INLINE digits #-}

-- |
specials :: (Stream s) => ParserS s String
specials = some special
{-# INLINE specials #-}

-- |
spaces :: (Stream s) => ParserS s String
spaces = some space
{-# INLINE spaces #-}

-- | Parses string between parentheses
--
-- >>> tt (parens letters) "(parser)"
-- "parser"
--
parens :: (Stream s) => ParserS s a -> ParserS s a
parens = between (symbol "(") (symbol ")")
{-# INLINE parens #-}

-- | Parses string between curly braces
--
-- >>> tt (braces letters) "{parser}"
-- "parser"
--
braces :: (Stream s) => ParserS s a -> ParserS s a
braces = between (symbol "{") (symbol "}")
{-# INLINE braces #-}

-- | Parses string between angle brackets
--
-- >>> tt (angles letters) "<parser>"
-- "parser"
--
angles :: (Stream s) => ParserS s a -> ParserS s a
angles = between (symbol "<") (symbol ">")
{-# INLINE angles #-}

-- | Parses string between square brackets
--
-- >>> tt (squares letters) "[parser]"
-- "parser"
--
squares :: Stream s => ParserS s a -> ParserS s a
squares = between (symbol "[") (symbol "]")
{-# INLINE squares #-}

-- | Parses natural numbers or decimal digits (base-10)
-- This includes numbers with leading zeros
--
-- >>> tt decimal "00123456789"
-- 123456789
--
decimal :: (Stream s, Num a) => ParserS s a
decimal = num 10 digits
{-# INLINE decimal #-}

-- | Parses hexadecimal digits (base-16)
--
-- >>> tt hexadecimal "0xCOVID-19"
-- 12
--
hexadecimal :: (Stream s, Num a) => ParserS s a
hexadecimal = skipOptional (string "0x") *> num 16 (some hexDigit)
{-# INLINE hexadecimal #-}

-- | Parses numbers with leading-zeros
--
-- >>> tt zeros "000002022"
-- 2022
--
zeros :: (Stream s, Num a) => ParserS s a
zeros = char '0' *> decimal
{-# INLINE zeros #-}

-- | Parses natural numbers (non-leading zeros and signs)
--
-- >>> tt natural "27182818284"
-- 27182818284
--
natural :: Stream s => ParserS s Integer
natural = try digit *> try (anycharBut '0') *> decimal
{-# INLINE natural #-}

-- | Parses a sign (@+@ or @-@) and lift the corresponding function.
--
-- >>> tt (sign <*> floating)  "-273.15 in Celsius"
-- -273.15
--
sign :: (Stream s, Num a) => ParserS s (a -> a)
sign = (char '-' $> negate) <|> (char '+' $> id) <|> pure id
{-# INLINE sign #-}

-- | Parses an integer (sign + numbers, sign if any)
--
-- >>> tt (sign <*> integer)  "-273.15 in Celsius"
-- -273
--
integer :: Stream s => ParserS s Integer
integer = sign <*> decimal
{-# INLINE integer #-}

-- | Convert a string parser into integer parser by evaluating the parsed with base
num :: (Stream s, Num a) => a -> ParserS s String -> ParserS s a
num base parser = foldl' f 0 <$> parser
  where f x d = base * x + fromIntegral (digitToInt d)
{-# INLINE num #-}

-- | Parses general form of number (including float and integer)
number :: Stream s => ParserS s Double
number = float <|> (fromIntegral <$> integer)
{-# INLINE number #-}

-- | Parses floating numbers (including scientific form)
--
-- >>> tt float  "3.1415926535e-8"
-- 3.1415926535e-8
--
float :: Stream s => ParserS s Double
float = scientific <|> floating
 where
  scientific = read <$> liftA2 (<>) base exponent'
  base       = show <$> (floating <|> fromIntegral <$> integer)
  exponent'  = liftA2 (:) (oneOf "eE") (show <$> integer)
{-# INLINE float #-}

-- | Parses floating number in format of @'decimals'.'decimals'@
-- (decimal + decimal-point(.) + decimal fractions)
--
-- >>> tt floating  "3.1415926535"
-- 3.1415926535
--
floating :: Stream s => ParserS s Double
floating = read
  <$> foldl1' (liftA2 (<>)) [sign, show <$> decimal, string ".", digits]
  where sign = option mempty (string "-" <|> (string "+" $> mempty))
{-# INLINE floating #-}

-- | Remove any leading and trailing whitespaces when parsing with @p@
--
-- Peeling whitespaces off is independent of any language syntax.
-- Use this when you just want to strip whitespaces around targets
--
-- >>> tt (strip float) "  3.1415926535"
-- 3.1415926535
--
strip :: (Stream s) => ParserS s a -> ParserS s a
strip = rstrip . lstrip
{-# INLINE strip #-}

-- | Remove any leading whitespaces when parsing with @p@
--
lstrip :: (Stream s) => ParserS s a -> ParserS s a
lstrip p = skip *> p
{-# INLINE lstrip #-}

-- | Remove any trailing whitespaces when parsing with @p@
--
rstrip :: (Stream s) => ParserS s a -> ParserS s a
rstrip p = p <* (skip <|> eof)
{-# INLINE rstrip #-}

-- | Guarantees one or more spaces, or @EOF@
gap :: (Stream s) => ParserS s ()
gap = skipSome space <|> eof

-- | Skips whitespaces
skip :: (Stream s) => ParserS s ()
skip = skipMany space
{-# INLINE skip #-}

-- | Skips successive blanks
skipb :: (Stream s) => ParserS s ()
skipb = skipMany blank
{-# INLINE skipb #-}

-- | Skips unnecesary whitespaces and comments
--
-- >>> input = "// LINE-COMMENT\n\r\n /*INNER BLOCK COMMENT*/ MUST-BE-HERE"
-- >>> ts (skips def) input
-- "MUST-BE-HERE"
--
skips :: Stream s => LanguageDef s -> ParserS s ()
skips def = skipMany $ choice [spaces, linec def, blockc def]
{-# INLINE skips #-}

-- | Skips line and block comments
skipc :: Stream s => LanguageDef s -> ParserS s ()
skipc def = skipMany $ linec def <|> blockc def
{-# INLINABLE skipc #-}

-- | Parses a single line comment
linec :: Stream s => LanguageDef s -> ParserS s String
linec def = some p *> (manyTill eol anychar <|> manyTill eof anychar)
  where p = defCommentLine def
{-# INLINABLE linec #-}

-- | Parses a multi-line block comment
blockc :: Stream s => LanguageDef s -> ParserS s String
blockc def = some bra *> manyTill ket anychar
 where
  bra = defCommentBlockBegin def
  ket = defCommentBlockEnd def
{-# INLINABLE blockc #-}

-- | Parses an identifier
identifier :: Stream s => LanguageDef s -> ParserS s String
identifier def = do
  let begin     = defIdentifierBegin def
  let remainder = defIdentifierName def
  found <- liftA2 (:) begin remainder

  if isReserved found def
    then fail $ unwords ["reserved identifier used:", show found]
    else skips def $> found

 where
  isReserved name def | caseSensitive = S.member name set
                      | otherwise     = S.member (lower name) set
  lower         = (toLower <$>)
  caseSensitive = defCaseSensitive def
  reservedNames = defKeywords def
  set | caseSensitive = S.fromList reservedNames
      | otherwise     = S.fromList $ lower <$> reservedNames
{-# INLINABLE identifier #-}

-- | Parses operators or special-chars based on the given 'LanguageDef'
--
-- >>> tt (digits *> skip *> operator def) "3 + 4"
-- "+"
--
operator :: Stream s => LanguageDef s -> ParserS s String
operator def = do
  op <- specials
  if isReserved op def
    then fail $ unwords ["reserved operator used:", show op]
    else skips def $> op
 where
  isReserved o def = S.member o set
  set = S.fromList (defReservedOps def)
{-# INLINABLE operator #-}

-- | Parses a single @char literal@
--
-- >>> stream = "'\r', a carriage-return or '\n', a line-feed?"
-- >>> tt charLit stream
-- '\r'
--
charLit :: Stream s => ParserS s Char
charLit = charLit' def
{-# INLINABLE charLit #-}

-- | The same as 'charLit', but this reads 'defCharLiteralMark' from 'LanguageDef'
charLit' :: Stream s => LanguageDef s -> ParserS s Char
charLit' = charLiteral . defCharLiteralMark
{-# INLINABLE charLit' #-}

-- | Character literal parser builder
charLiteral :: Stream s => ParserS s String -> ParserS s Char
charLiteral mark = between mark (mark <?> "end-of-char-literal") readChar
{-# INLINABLE charLiteral #-}

-- |
readChar :: Stream s => ParserS s Char
readChar = do
  s <- try $ count 4 anychar <|> manyTill eof anychar
  case readLitChar s of
    [(a, s')] -> a <$ skipCount (length s - length s') anychar
    _         -> fail "failed to read any char literal"
{-# INLINABLE readChar #-}

-- | Parses a single @string literal@
--
-- >>> stream = "\"'\\r', a carriage-return or '\\n', a line-feed?\""
-- >>> tt stringLit stream
-- "'\\r', a carriage-return or '\\n', a line-feed?"
--
-- The following can be used, but not very efficient.
-- >>> stringLit = string "\"" *> manyTill readChar (string "\"")
--
stringLit :: Stream s => ParserS s String
stringLit = stringLit' def
{-# INLINABLE stringLit #-}

-- | The same as 'stringLit', but this reads 'defStringLiteralMark' from 'LanguageDef'.
stringLit' :: Stream s => LanguageDef s -> ParserS s String
stringLit' = stringLiteral . defStringLiteralMark
{-# INLINABLE stringLit' #-}

-- | String literal parser builder
stringLiteral :: Stream s => ParserS s String -> ParserS s String
stringLiteral mark = concat
  <$> between mark (mark <?> "end-of-string-literal") (many character)
 where
  character = choice
    [ pure <$> noneOf "\\\"\0\n\r\t\b\v\f"
    , sequence [char '\\', oneOf "\\\"0nrtbvf"]
    ]
{-# INLINABLE stringLiteral #-}
