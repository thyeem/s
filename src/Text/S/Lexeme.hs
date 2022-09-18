-----------------------------------------------------------------------------
-- |
-- Module      : Text.S.Lexeme
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module constructs a __lexeme parser__ to parse a lexical unit
-- combinating char-parsers and parser-combinators.
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
import           Data.Functor                   ( void )
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



-- | Defines a data type of lexeme parser
--
-- - A @Parser@ is a superset of a @Lexer@
--
-- +-----------+----------------------------------------------------------------+
-- | @Parsing@ | Doing parse-job with primitive parsers and theirs combinations |
-- +-----------+----------------------------------------------------------------+
-- | @Lexing@  | Doing parse-job with parsers and language definitions          |
-- +-----------+----------------------------------------------------------------+
--
-- - Main difference between @Lexer@ and @Parser@
--
-- +----------+--------------------------------------------------------------------------+
-- | @Parser@ | Independent of 'LanguageDef' and has a name that looks like __funcName__ |
-- +----------+--------------------------------------------------------------------------+
-- | @Lexer@  | Dependent on 'LanguageDef' and has a name that looks like __funcName'__  |
-- +----------+--------------------------------------------------------------------------+
--
-- @
-- (Applying language definitions to the Lexer)
-- => 'Lexer' 'LanguageDef'
-- => ('LanguageDef' -> 'ParserS' s a) 'LanguageDef'
-- => 'ParserS' s a
-- @
--
type ParserS' s a = LanguageDef -> ParserS s a


-- | Make a given parser @p@ a lexeme parser
-- based on the given language definition
lexeme' :: Stream s => ParserS s a -> ParserS' s a
lexeme' p def = p <* skip' def
{-# INLINE lexeme' #-}

-- | Skips unnecesary whitespaces and comments
--
-- >>> input = "# line-comment\n\r\n '''inner block''' should-be-here"
-- >>> ts' (skip' defDef) input
-- "should-be-here"
--
skip' :: (Stream s) => ParserS' s ()
skip' def = skipMany $ spaces <|> commentLine' def <|> commentBlock' def
{-# INLINE skip' #-}

-- | Skips whitespaces
skipSpaces :: (Stream s) => ParserS s ()
skipSpaces = skipMany space
{-# INLINE skipSpaces #-}

-- | Skips line and block comments
skipComments' :: (Stream s) => ParserS' s ()
skipComments' def = skipMany $ commentLine' def <|> commentBlock' def
{-# INLINABLE skipComments' #-}

-- | Parses single-line comment
commentLine' :: (Stream s) => ParserS' s String
commentLine' def = p *> manyTill eol anychar
  where p = choice $ string <$> defCommentLine def
{-# INLINABLE commentLine' #-}

-- | Parses block comment
commentBlock' :: (Stream s) => ParserS' s String
commentBlock' def = bra *> manyTill ket anychar
 where
  bra = choice $ string <$> defCommentBlockBegin def
  ket = choice $ string <$> defCommentBlockEnd def
{-# INLINABLE commentBlock' #-}

-- | Remove any leading and trailing whitespaces when parsing with @p@
--
-- Peeling whitespaces off is independent of any language syntax.
-- Use this when you just want to strip whitespaces around targets
--
-- >>> t' (strip float) "  3.1415926535"
-- 3.1415926535
--
strip :: (Stream s) => ParserS s a -> ParserS s a
strip = rstrip . lstrip
{-# INLINE strip #-}

-- | Remove any leading whitespaces when parsing with @p@
--
lstrip :: (Stream s) => ParserS s a -> ParserS s a
lstrip p = skipSpaces *> p
{-# INLINE lstrip #-}

-- | Remove any trailing whitespaces when parsing with @p@
--
rstrip :: (Stream s) => ParserS s a -> ParserS s a
rstrip p = p <* (skipSpaces <|> void eof)
{-# INLINE rstrip #-}

-- | Parses any string symbol to comsume. The same as 'string'
symbol :: (Stream s) => String -> ParserS s String
symbol = string
{-# INLINE symbol #-}

-- | The same as 'symbol', but in the form of  t'ParserS''.
symbol' :: (Stream s) => String -> ParserS' s String
symbol' t = lexeme' (symbol t)
{-# INLINE symbol' #-}

-- |
letters :: (Stream s) => ParserS s String
letters = some letter
{-# INLINE letters #-}

-- | The same as 'letters', but int the form of t'ParserS''.
letters' :: (Stream s) => ParserS' s String
letters' = lexeme' letters
{-# INLINE letters' #-}

-- |
alphaNums :: (Stream s) => ParserS s String
alphaNums = some alphaNum
{-# INLINE alphaNums #-}

-- | The t'ParserS'' form of 'alphaNums'
alphaNums' :: (Stream s) => ParserS' s String
alphaNums' = lexeme' alphaNums
{-# INLINE alphaNums' #-}

-- |
digits :: (Stream s) => ParserS s String
digits = some digit
{-# INLINE digits #-}

-- | The t'ParserS'' form of 'digits'
digits' :: (Stream s) => ParserS' s String
digits' = lexeme' digits
{-# INLINE digits' #-}

-- |
specials :: (Stream s) => ParserS s String
specials = some special
{-# INLINE specials #-}

-- | The t'ParserS'' form of 'specials'
specials' :: (Stream s) => ParserS' s String
specials' = lexeme' specials
{-# INLINE specials' #-}

-- |
spaces :: (Stream s) => ParserS s String
spaces = some space
{-# INLINE spaces #-}

-- | The t'ParserS'' form of 'spaces'
spaces' :: (Stream s) => ParserS' s String
spaces' = lexeme' spaces
{-# INLINE spaces' #-}

-- | Parses string between parentheses
--
-- >>> t' (parens letters) "(parser)"
-- "parser"
--
parens :: (Stream s) => ParserS s a -> ParserS s a
parens = between (symbol "(") (symbol ")")
{-# INLINE parens #-}

-- | The t'ParserS'' form of 'parens'
parens' :: (Stream s) => ParserS s a -> ParserS' s a
parens' p = lexeme' (parens p)
{-# INLINE parens' #-}

-- | Parses string between curly braces
--
-- >>> t' (braces letters) "{parser}"
-- "parser"
--
braces :: (Stream s) => ParserS s a -> ParserS s a
braces = between (symbol "{") (symbol "}")
{-# INLINE braces #-}

-- | The t'ParserS'' form of 'braces'
braces' :: (Stream s) => ParserS s a -> ParserS' s a
braces' p = lexeme' (braces p)
{-# INLINE braces' #-}

-- | Parses string between angle brackets
--
-- >>> t' (angles letters) "<parser>"
-- "parser"
--
angles :: (Stream s) => ParserS s a -> ParserS s a
angles = between (symbol "<") (symbol ">")
{-# INLINE angles #-}

-- | The t'ParserS'' form of 'angles'
angles' :: (Stream s) => ParserS s a -> ParserS' s a
angles' p = lexeme' (angles p)
{-# INLINE angles' #-}

-- | Parses string between square brackets
--
-- >>> t' (squares letters) "[parser]"
-- "parser"
--
squares :: Stream s => ParserS s a -> ParserS s a
squares = between (symbol "[") (symbol "]")
{-# INLINE squares #-}

-- | The t'ParserS'' form of 'squares'
squares' :: Stream s => ParserS s a -> ParserS' s a
squares' p = lexeme' (squares p)
{-# INLINE squares' #-}

-- | Parses natural numbers or decimal digits (base-10)
-- This includes numbers with leading zeros
--
-- >>> t' decimals "00123456789"
-- 123456789
--
decimals :: (Stream s, Num a) => ParserS s a
decimals = numbers 10 digits
{-# INLINE decimals #-}

-- | The t'ParserS'' form of 'decimals'
decimals' :: (Stream s, Num a) => ParserS' s a
decimals' = lexeme' decimals
{-# INLINE decimals' #-}

-- | Parses hexadecimal digits (base-16)
--
-- >>> t' hexadecimals "0xCOVID-19"
-- 12
--
hexadecimals :: (Stream s, Num a) => ParserS s a
hexadecimals = skipOptional (string "0x") *> numbers 16 (some hexDigit)
{-# INLINE hexadecimals #-}

-- | The t'ParserS'' form of 'hexadecimals'
hexadecimals' :: (Stream s, Num a) => ParserS' s a
hexadecimals' = lexeme' hexadecimals
{-# INLINE hexadecimals' #-}

-- | Parses numbers with leading-zeros
--
-- >>> t' zeros "000002022"
-- 2022
--
zeros :: (Stream s, Num a) => ParserS s a
zeros = char '0' *> decimals
{-# INLINE zeros #-}

-- | The t'ParserS'' form of 'zeros'
zeros' :: (Stream s, Num a) => ParserS' s a
zeros' = lexeme' zeros
{-# INLINE zeros' #-}

-- | Parses natural numbers (non-leading zeros and signs)
--
-- >>> t' natural "27182818284"
-- 27182818284
--
natural :: Stream s => ParserS s Integer
natural = try digit *> try (anycharBut '0') *> decimals
{-# INLINE natural #-}

-- | The t'ParserS'' form of 'natural'
natural' :: Stream s => ParserS' s Integer
natural' = lexeme' natural
{-# INLINE natural' #-}

-- | Parses a sign (@+@ or @-@) and lift the corresponding function.
--
-- >>> t' (sign <*> floating)  "-273.15 in Celsius"
-- -273.15
--
sign :: (Stream s, Num a) => ParserS s (a -> a)
sign = (char '-' $> negate) <|> (char '+' $> id) <|> pure id
{-# INLINE sign #-}

-- | The same as 'sign' but strip whitespaces between sign and numbers.
--
-- >>> t' (sign' defDef <*> floating)  "-  273.15 in Celsius"
-- -273.15
--
sign' :: (Stream s, Num a) => ParserS' s (a -> a)
sign' = lexeme' sign
{-# INLINE sign' #-}

-- | Parses an integer (sign + numbers, sign if any)
--
-- >>> t' (sign <*> integer)  "-273.15 in Celsius"
-- -273
--
integer :: Stream s => ParserS s Integer
integer = sign <*> decimals
{-# INLINE integer #-}

-- | The t'ParserS'' form of 'integer'
integer' :: Stream s => ParserS' s Integer
integer' def = sign <*> decimals' def
{-# INLINE integer' #-}

-- | Convert a string parser into integer parser by evaluating the parsed with base
numbers :: (Stream s, Num a) => a -> ParserS s String -> ParserS s a
numbers base parser = foldl' f 0 <$> parser
  where f x d = base * x + fromIntegral (digitToInt d)
{-# INLINE numbers #-}

-- | Parses general form of floating numbers (including scientific form)
--
-- >>> t' float  "3.1415926535e-8"
-- 3.1415926535e-8
--
float :: Stream s => ParserS s Double
float = read <$> (scientific <|> decimalPoint)
 where
  scientific   = (<>) <$> decimalPoint <*> exponent'
  decimalPoint = show <$> (floating <|> fromIntegral <$> integer)
  exponent'    = (:) <$> oneOf "eE" <*> (show <$> integer)
{-# INLINE float #-}

-- | The t'ParserS'' form of 'float'
float' :: Stream s => ParserS' s Double
float' = lexeme' float
{-# INLINE float' #-}

-- | Parses format of @'decimals'.'decimals'@
-- (decimals + decimal point + decimal fractions)
--
-- >>> t' floating  "3.1415926535"
-- 3.1415926535
--
floating :: Stream s => ParserS s Double
floating = read <$> foldl1' (liftA2 (<>)) [sign, digits, string ".", digits]
 where
  sign   = option "" (string "-" <|> (string "+" $> ""))
  digits = show <$> decimals
{-# INLINE floating #-}

-- | The t'ParserS'' form of 'floating'
floating' :: Stream s => ParserS' s Double
floating' = lexeme' floating
{-# INLINE floating' #-}

-- | Parses identifiers based on the given 'LanguageDef'
--
-- >>> t' (identifier' defDef) "function(arg1, arg2)"
-- "function"
--
identifier' :: Stream s => ParserS' s String
identifier' def = do
  let begin     = choice $ some . selectp <$> defIdentifierBegin def
  let remainder = choice $ many . selectp <$> defIdentifierName def
  found <- (<>) <$> begin <*> remainder

  if isReserved found def
    then fail $ unwords ["reserved identifier used:", show found]
    else skip' def $> found

 where
  isReserved name def | caseSensitive = S.member name set
                      | otherwise     = S.member (lower name) set
  lower         = (toLower <$>)
  caseSensitive = defCaseSensitive def
  reservedNames = defReservedNames def
  set | caseSensitive = S.fromList reservedNames
      | otherwise     = S.fromList $ lower <$> reservedNames
{-# INLINABLE identifier' #-}

-- | Parses operators or special-chars based on the given 'LanguageDef'
--
-- >>> t' (digits *> skipSpaces *> operator' defDef) "3 + 4"
-- "+"
--
operator' :: Stream s => ParserS' s String
operator' def = do
  op <- specials
  if isReserved op def
    then fail $ unwords ["reserved operator used:", show op]
    else skip' def $> op
 where
  isReserved o def = S.member o set
  set = S.fromList (defReservedSpecials def)
{-# INLINABLE operator' #-}

-- | Parses a single @char literal@
--
-- >>> stream = "'\r', a carriage-return or '\n', a line-feed?"
-- >>> t' charLit stream
-- '\r'
--
charLit :: Stream s => ParserS s Char
charLit = genCharLit "'"
{-# INLINABLE charLit #-}

-- | The same as 'charLit', but this reads 'defCharLiteralMark' from 'LanguageDef'
charLit' :: Stream s => ParserS' s Char
charLit' def = lexeme' (genCharLit mark) def
  where mark = defCharLiteralMark def
{-# INLINABLE charLit' #-}

-- | Character literal parser builder
genCharLit :: Stream s => String -> ParserS s Char
genCharLit mark =
  between (string mark) (string mark <?> "end-of-char-literal") readChar
{-# INLINABLE genCharLit #-}

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
-- >>> t' stringLit stream
-- "'\\r', a carriage-return or '\\n', a line-feed?"
--
-- The following can be used, but not very efficient.
-- >>> stringLit = string "\"" *> manyTill readChar (string "\"")
--
stringLit :: Stream s => ParserS s String
stringLit = genStringLit "\""
{-# INLINABLE stringLit #-}

-- | The same as 'stringLit', but this reads 'defStringLiteralMark' from 'LanguageDef'
stringLit' :: Stream s => ParserS' s String
stringLit' def = lexeme' (genStringLit mark) def
  where mark = defStringLiteralMark def
{-# INLINABLE stringLit' #-}

-- | String literal parser builder
genStringLit :: Stream s => String -> ParserS s String
genStringLit mark = concat <$> between
  (string mark)
  (string mark <?> "end-of-string-literal")
  (many character)
 where
  character = choice
    [ pure <$> noneOf "\\\"\0\n\r\t\b\v\f"
    , sequence [char '\\', oneOf "\\\"0nrtbvf"]
    ]
{-# INLINABLE genStringLit #-}
