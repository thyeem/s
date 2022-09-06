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

import           Control.DeepSeq                ( NFData
                                                , force
                                                )
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
lexeme' :: (Stream s, NFData s) => ParserS s a -> ParserS' s a
lexeme' p def = p <* skip' def
{-# INLINABLE lexeme' #-}

-- | Skips unnecesary whitespaces and comments
--
-- >>> input = "# line-comment\n\r\n '''inner block''' should-be-here"
-- >>> ts' (skip' defDef) input
-- "should-be-here"
--
skip' :: (Stream s, NFData s) => ParserS' s ()
skip' def = skipMany $ spaces <|> commentLine' def <|> commentBlock' def
{-# INLINABLE skip' #-}

-- | Skips whitespaces
skipSpaces :: (Stream s, NFData s) => ParserS s ()
skipSpaces = skipMany space
{-# INLINABLE skipSpaces #-}

-- | Skips line and block comments
skipComments' :: (Stream s, NFData s) => ParserS' s ()
skipComments' def = skipMany $ commentLine' def <|> commentBlock' def
{-# INLINABLE skipComments' #-}

-- | Parses single-line comment
commentLine' :: (Stream s, NFData s) => ParserS' s String
commentLine' def = p *> manyTill eol anychar
  where p = choice $ string <$> defCommentLine def
{-# INLINABLE commentLine' #-}

-- | Parses block comment
commentBlock' :: (Stream s, NFData s) => ParserS' s String
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
strip :: (Stream s, NFData s) => ParserS s a -> ParserS s a
strip = rstrip . lstrip
{-# INLINABLE strip #-}

-- | Remove any leading whitespaces when parsing with @p@
--
lstrip :: (Stream s, NFData s) => ParserS s a -> ParserS s a
lstrip p = skipSpaces *> p
{-# INLINABLE lstrip #-}

-- | Remove any trailing whitespaces when parsing with @p@
--
rstrip :: (Stream s, NFData s) => ParserS s a -> ParserS s a
rstrip p = p <* (skipSpaces <|> void eof)
{-# INLINABLE rstrip #-}

-- | Parses any string symbol to comsume. The same as 'string'
symbol :: (Stream s, NFData s) => String -> ParserS s String
symbol = string
{-# INLINABLE symbol #-}

-- | The same as 'symbol', but in the form of  t'ParserS''.
symbol' :: (Stream s, NFData s) => String -> ParserS' s String
symbol' t = lexeme' (symbol t)
{-# INLINABLE symbol' #-}

-- |
letters :: (Stream s, NFData s) => ParserS s String
letters = some letter
{-# INLINABLE letters #-}

-- | The same as 'letters', but int the form of t'ParserS''.
letters' :: (Stream s, NFData s) => ParserS' s String
letters' = lexeme' letters
{-# INLINABLE letters' #-}

-- |
alphaNums :: (Stream s, NFData s) => ParserS s String
alphaNums = some alphaNum
{-# INLINABLE alphaNums #-}

-- | The t'ParserS'' form of 'alphaNums'
alphaNums' :: (Stream s, NFData s) => ParserS' s String
alphaNums' = lexeme' alphaNums
{-# INLINABLE alphaNums' #-}

-- |
digits :: (Stream s, NFData s) => ParserS s String
digits = some digit
{-# INLINABLE digits #-}

-- | The t'ParserS'' form of 'digits'
digits' :: (Stream s, NFData s) => ParserS' s String
digits' = lexeme' digits
{-# INLINABLE digits' #-}

-- |
specials :: (Stream s, NFData s) => ParserS s String
specials = some special
{-# INLINABLE specials #-}

-- | The t'ParserS'' form of 'specials'
specials' :: (Stream s, NFData s) => ParserS' s String
specials' = lexeme' specials
{-# INLINABLE specials' #-}

-- |
spaces :: (Stream s, NFData s) => ParserS s String
spaces = some space
{-# INLINABLE spaces #-}

-- | The t'ParserS'' form of 'spaces'
spaces' :: (Stream s, NFData s) => ParserS' s String
spaces' = lexeme' spaces
{-# INLINABLE spaces' #-}

-- | Parses string between parentheses
--
-- >>> t' (parens letters) "(parser)"
-- "parser"
--
parens :: (Stream s, NFData s) => ParserS s a -> ParserS s a
parens = between (symbol "(") (symbol ")")
{-# INLINABLE parens #-}

-- | The t'ParserS'' form of 'parens'
parens' :: (Stream s, NFData s) => ParserS s a -> ParserS' s a
parens' p = lexeme' (parens p)
{-# INLINABLE parens' #-}

-- | Parses string between curly braces
--
-- >>> t' (braces letters) "{parser}"
-- "parser"
--
braces :: (Stream s, NFData s) => ParserS s a -> ParserS s a
braces = between (symbol "{") (symbol "}")
{-# INLINABLE braces #-}

-- | The t'ParserS'' form of 'braces'
braces' :: (Stream s, NFData s) => ParserS s a -> ParserS' s a
braces' p = lexeme' (braces p)
{-# INLINABLE braces' #-}

-- | Parses string between angle brackets
--
-- >>> t' (angles letters) "<parser>"
-- "parser"
--
angles :: (Stream s, NFData s) => ParserS s a -> ParserS s a
angles = between (symbol "<") (symbol ">")
{-# INLINABLE angles #-}

-- | The t'ParserS'' form of 'angles'
angles' :: (Stream s, NFData s) => ParserS s a -> ParserS' s a
angles' p = lexeme' (angles p)
{-# INLINABLE angles' #-}

-- | Parses string between square brackets
--
-- >>> t' (squares letters) "[parser]"
-- "parser"
--
squares :: (Stream s, NFData s) => ParserS s a -> ParserS s a
squares = between (symbol "[") (symbol "]")
{-# INLINABLE squares #-}

-- | The t'ParserS'' form of 'squares'
squares' :: (Stream s, NFData s) => ParserS s a -> ParserS' s a
squares' p = lexeme' (squares p)
{-# INLINABLE squares' #-}

-- | Parses natural numbers or decimal digits (base-10)
-- This includes numbers with leading zeros
--
-- >>> t' decimals "00123456789"
-- 123456789
--
decimals :: (Stream s, NFData s, Num a) => ParserS s a
decimals = numbers 10 digits
{-# INLINABLE decimals #-}

-- | The t'ParserS'' form of 'decimals'
decimals' :: (Stream s, NFData s, Num a) => ParserS' s a
decimals' = lexeme' decimals
{-# INLINABLE decimals' #-}

-- | Parses hexadecimal digits (base-16)
--
-- >>> t' hexadecimals "0xCOVID-19"
-- 12
--
hexadecimals :: (Stream s, NFData s, Num a) => ParserS s a
hexadecimals = skipOptional (string "0x") *> numbers 16 (some hexDigit)
{-# INLINABLE hexadecimals #-}

-- | The t'ParserS'' form of 'hexadecimals'
hexadecimals' :: (Stream s, NFData s, Num a) => ParserS' s a
hexadecimals' = lexeme' hexadecimals
{-# INLINABLE hexadecimals' #-}

-- | Parses numbers with leading-zeros
--
-- >>> t' zeros "000002022"
-- 2022
--
zeros :: (Stream s, NFData s, Num a) => ParserS s a
zeros = char '0' *> decimals
{-# INLINABLE zeros #-}

-- | The t'ParserS'' form of 'zeros'
zeros' :: (Stream s, NFData s, Num a) => ParserS' s a
zeros' = lexeme' zeros
{-# INLINABLE zeros' #-}

-- | Parses natural numbers (non-leading zeros and signs)
--
-- >>> t' natural "27182818284"
-- 27182818284
--
natural :: (Stream s, NFData s) => ParserS s Integer
natural = assert digit *> assert (anycharBut '0') *> decimals
{-# INLINABLE natural #-}

-- | The t'ParserS'' form of 'natural'
natural' :: (Stream s, NFData s) => ParserS' s Integer
natural' = lexeme' natural
{-# INLINABLE natural' #-}

-- | Parses a sign (@+@ or @-@) and lift the corresponding function.
--
-- >>> t' (sign <*> floating)  "-273.15 in Celsius"
-- -273.15
--
sign :: (Stream s, NFData s, Num a) => ParserS s (a -> a)
sign = (char '-' $> negate) <|> (char '+' $> id) <|> pure id
{-# INLINABLE sign #-}

-- | The same as 'sign' but strip whitespaces between sign and numbers.
--
-- >>> t' (sign' defDef <*> floating)  "-  273.15 in Celsius"
-- -273.15
--
sign' :: (Stream s, NFData s, Num a) => ParserS' s (a -> a)
sign' = lexeme' sign
{-# INLINABLE sign' #-}

-- | Parses an integer (sign + numbers, sign if any)
--
-- >>> t' (sign <*> integer)  "-273.15 in Celsius"
-- -273
--
integer :: (Stream s, NFData s) => ParserS s Integer
integer = sign <*> decimals
{-# INLINABLE integer #-}

-- | The t'ParserS'' form of 'integer'
integer' :: (Stream s, NFData s) => ParserS' s Integer
integer' def = sign <*> decimals' def
{-# INLINABLE integer' #-}

-- | Convert a string parser into integer parser by evaluating the parsed with base
numbers :: (Stream s, NFData s, Num a) => a -> ParserS s String -> ParserS s a
numbers base parser = foldl' f 0 <$> parser
  where f x d = base * x + fromIntegral (digitToInt d)
{-# INLINABLE numbers #-}

-- | Parses general form of floating numbers (including scientific form)
--
-- >>> t' float  "3.1415926535e-8"
-- 3.1415926535e-8
--
float :: (Stream s, NFData s) => ParserS s Double
float = read <$> (scientific <|> decimalPoint)
 where
  scientific   = (<>) <$> decimalPoint <*> exponent'
  decimalPoint = show <$> (floating <|> fromIntegral <$> integer)
  exponent'    = (:) <$> oneOf "eE" <*> (show <$> integer)
{-# INLINABLE float #-}

-- | The t'ParserS'' form of 'float'
float' :: (Stream s, NFData s) => ParserS' s Double
float' = lexeme' float
{-# INLINABLE float' #-}

-- | Parses format of @'decimals'.'decimals'@
-- (decimals + decimal point + decimal fractions)
--
-- >>> t' floating  "3.1415926535"
-- 3.1415926535
--
floating :: (Stream s, NFData s) => ParserS s Double
floating = read <$> foldl1' (liftA2 (<>)) [sign, digits, string ".", digits]
 where
  sign   = option "" (string "-" <|> (string "+" $> ""))
  digits = show <$> decimals
{-# INLINABLE floating #-}

-- | The t'ParserS'' form of 'floating'
floating' :: (Stream s, NFData s) => ParserS' s Double
floating' = lexeme' floating
{-# INLINABLE floating' #-}

-- | Parses identifiers based on the given 'LanguageDef'
--
-- >>> t' (identifier' defDef) "function(arg1, arg2)"
-- "function"
--
identifier' :: (Stream s, NFData s) => ParserS' s String
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
operator' :: (Stream s, NFData s) => ParserS' s String
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
charLit :: (Stream s, NFData s) => ParserS s Char
charLit = genCharLit "'"
{-# INLINABLE charLit #-}

-- | The same as 'charLit', but this reads 'defCharLiteralMark' from 'LanguageDef'
charLit' :: (Stream s, NFData s) => ParserS' s Char
charLit' def = lexeme' (genCharLit mark) def
  where mark = defCharLiteralMark def
{-# INLINABLE charLit' #-}

-- | Character literal parser builder
genCharLit :: (Stream s, NFData s) => String -> ParserS s Char
genCharLit mark =
  between (string mark) (string mark <?> "end-of-char-literal") readChar
{-# INLINABLE genCharLit #-}

-- |
readChar :: (Stream s, NFData s) => ParserS s Char
readChar = do
  s <- assert $ count 5 (eof <|> anychar)
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
stringLit :: (Stream s, NFData s) => ParserS s String
stringLit = genStringLit "\""
{-# INLINABLE stringLit #-}

-- | The same as 'stringLit', but this reads 'defStringLiteralMark' from 'LanguageDef'
stringLit' :: (Stream s, NFData s) => ParserS' s String
stringLit' def = lexeme' (genStringLit mark) def
  where mark = defStringLiteralMark def
{-# INLINABLE stringLit' #-}

-- | String literal parser builder
genStringLit :: (Stream s, NFData s) => String -> ParserS s String
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
