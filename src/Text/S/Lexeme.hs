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
import           Data.Foldable                  ( foldl' )
import           Data.Functor                   ( void )
import qualified Data.Set                      as S
import           Text.S.Base
import           Text.S.Combinator
import           Text.S.Internal
import           Text.S.Language

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( mzero )



-- | Defines a data type of lexeme parser
--
-- - A @Parser@ is a superset of a @Lexer@
--
-- +-----------+----------------------------------------------------------------+
-- | @Parsing@ | Doing parse-job with primitive parsers and theirs combinations |
-- +-----------+----------------------------------------------------------------+
-- | @Lexing@  | Doing parse-job with parsers and /language definitions/        |
-- +-----------+----------------------------------------------------------------+
--
-- - Main difference between @Lexer@ and @Parser@
--
-- +----------+--------------------------------------------------------------------------+
-- | @Parser@ | Independent of `LanguageDef` and has a name that looks like __funcName__ |
-- +----------+--------------------------------------------------------------------------+
-- | @Lexer@  | Dependent on `LanguageDef` and has a name that looks like __funcName'__  |
-- +----------+--------------------------------------------------------------------------+
--
-- @
-- (/Applying language definitions to the Lexer/)
-- => `Lexer` `LanguageDef`
-- => (`LanguageDef` -> `ParserS` s a) `LanguageDef`
-- => `ParserS` s a
-- @
--
type ParserS' s a = LanguageDef -> ParserS s a


-- | Make a given parser @p@ a lexeme parser
-- based on the given language definition
lexeme' :: (Stream s, NFData s) => ParserS s a -> ParserS' s a
lexeme' p def = p <* skip' def

-- | Skips unnecesary whitespaces and comments
--
-- >>> input = "# line-comment\n\r\n '''inner block''' should-be-here"
-- >>> unwrap' $ t (skip' defDef) input
-- "should-be-here"
--
skip' :: (Stream s, NFData s) => ParserS' s ()
skip' def = skipMany $ spaces <|> commentLine' def <|> commentBlock' def

-- | Skips whitespaces
skipSpaces :: (Stream s, NFData s) => ParserS s ()
skipSpaces = skipMany space

-- | Skips line and block comments
skipComments' :: (Stream s, NFData s) => ParserS' s ()
skipComments' def = skipMany $ commentLine' def <|> commentBlock' def

-- | Parses single-line comment
commentLine' :: (Stream s, NFData s) => ParserS' s String
commentLine' def = p *> manyTill anychar eol
  where p = choice $ string <$> defCommentLine def

-- | Parses block comment
commentBlock' :: (Stream s, NFData s) => ParserS' s String
commentBlock' def = bra *> manyTill anychar ket
 where
  bra = choice $ string <$> defCommentBlockBegin def
  ket = choice $ string <$> defCommentBlockEnd def

-- | Remove any leading and trailing whitespaces when parsing with @p@
--
-- Peeling whitespaces off is independent of any language syntax.
-- Use this when you just want to strip whitespaces around targets
--
-- >>> t' (strip float) "  3.1415926535"
-- Right 3.1415926535
--
strip :: (Stream s, NFData s) => ParserS s a -> ParserS s a
strip = rstrip . lstrip
-- strip p = skipSpaces *> p <* (skipSpaces <|> void eof)

-- | Remove any leading whitespaces when parsing with @p@
--
lstrip :: (Stream s, NFData s) => ParserS s a -> ParserS s a
lstrip p = skipSpaces *> p

-- | Remove any trailing whitespaces when parsing with @p@
--
rstrip :: (Stream s, NFData s) => ParserS s a -> ParserS s a
rstrip p = p <* (skipSpaces <|> void eof)

-- | Parses any string symbol to comsume. The same as `string`
symbol :: (Stream s, NFData s) => String -> ParserS s String
symbol = string

-- | The same as `symbol`, but in the form of  t'ParserS''.
symbol' :: (Stream s, NFData s) => String -> ParserS' s String
symbol' t = lexeme' (symbol t)

-- |
letters :: (Stream s, NFData s) => ParserS s String
letters = some letter

-- | The same as `letters`, but int the form of t'ParserS''.
letters' :: (Stream s, NFData s) => ParserS' s String
letters' = lexeme' letters

-- |
alphaNums :: (Stream s, NFData s) => ParserS s String
alphaNums = some alphaNum

-- | The t'ParserS'' form of `alphaNums`
alphaNums' :: (Stream s, NFData s) => ParserS' s String
alphaNums' = lexeme' alphaNums

-- |
digits :: (Stream s, NFData s) => ParserS s String
digits = some digit

-- | The t'ParserS'' form of `digits`
digits' :: (Stream s, NFData s) => ParserS' s String
digits' = lexeme' digits

-- |
specials :: (Stream s, NFData s) => ParserS s String
specials = some special

-- | The t'ParserS'' form of `specials`
specials' :: (Stream s, NFData s) => ParserS' s String
specials' = lexeme' specials

-- |
spaces :: (Stream s, NFData s) => ParserS s String
spaces = some space

-- | The t'ParserS'' form of `spaces`
spaces' :: (Stream s, NFData s) => ParserS' s String
spaces' = lexeme' spaces

-- | Parses string between parentheses
--
-- >>> t' (parens letters) "(parser)"
-- Right "parser"
--
parens :: (Stream s, NFData s) => ParserS s String -> ParserS s String
parens = between (symbol "(") (symbol ")")

-- | The t'ParserS'' form of `parens`
parens' :: (Stream s, NFData s) => ParserS s String -> ParserS' s String
parens' p = lexeme' (parens p)

-- | Parses string between curly braces
--
-- >>> t' (braces letters) "{parser}"
-- Right "parser"
--
braces :: (Stream s, NFData s) => ParserS s String -> ParserS s String
braces = between (symbol "{") (symbol "}")

-- | The t'ParserS'' form of `braces`
braces' :: (Stream s, NFData s) => ParserS s String -> ParserS' s String
braces' p = lexeme' (braces p)

-- | Parses string between angle brackets
--
-- >>> t' (angles letters) "<parser>"
-- Right "parser"
--
angles :: (Stream s, NFData s) => ParserS s String -> ParserS s String
angles = between (symbol "<") (symbol ">")

-- | The t'ParserS'' form of `angles`
angles' :: (Stream s, NFData s) => ParserS s String -> ParserS' s String
angles' p = lexeme' (angles p)

-- | Parses string between square brackets
--
-- >>> t' (squares letters) "[parser]"
-- Right "parser"
--
squares :: (Stream s, NFData s) => ParserS s String -> ParserS s String
squares = between (symbol "[") (symbol "]")

-- | The t'ParserS'' form of `squares`
squares' :: (Stream s, NFData s) => ParserS s String -> ParserS' s String
squares' p = lexeme' (squares p)

-- | Parses natural numbers or decimal digits (base-10)
-- This includes numbers with leading zeros
--
-- >>> t' decimals "00123456789"
-- Right 123456789
--
decimals :: (Stream s, NFData s) => ParserS s Integer
decimals = numbers 10 digits

-- | The t'ParserS'' form of `decimals`
decimals' :: (Stream s, NFData s) => ParserS' s Integer
decimals' = lexeme' decimals

-- | Parses hexadecimal digits (base-16)
--
-- >>> t' hexadecimals "0xCOVID-19"
-- Right 12
--
hexadecimals :: (Stream s, NFData s) => ParserS s Integer
hexadecimals = skipOptional (string "0x") *> numbers 16 (some hexDigit)

-- | The t'ParserS'' form of `hexadecimals`
hexadecimals' :: (Stream s, NFData s) => ParserS' s Integer
hexadecimals' = lexeme' hexadecimals

-- | Parses numbers with leading-zeros
--
-- >>> t' zeros "000002022"
-- Right 2022
--
zeros :: (Stream s, NFData s) => ParserS s Integer
zeros = char '0' *> decimals

-- | The t'ParserS'' form of `zeros`
zeros' :: (Stream s, NFData s) => ParserS' s Integer
zeros' = lexeme' zeros

-- | Parses natural numbers (non-leading zeros and signs)
--
-- >>> t' natural "27182818284"
-- Right 27182818284
--
natural :: (Stream s, NFData s) => ParserS s Integer
natural = assert digit *> assert (anycharBut '0') *> decimals

-- | The t'ParserS'' form of `natural`
natural' :: (Stream s, NFData s) => ParserS' s Integer
natural' = lexeme' natural

-- | Parses a sign (@+@ or @-@) and lift the corresponding function.
--
-- >>> t' (sign <*> floating)  "-273.15 in Celsius"
-- Right (-273.15)
--
sign :: (Stream s, NFData s, Num a) => ParserS s (a -> a)
sign = (char '-' $> negate) <|> (char '+' $> id) <|> pure id

-- | The same as `sign` but strip whitespaces between sign and numbers.
--
-- >>> t' (sign' defDef <*> floating)  "-  273.15 in Celsius"
-- Right (-273.15)
--
sign' :: (Stream s, NFData s, Num a) => ParserS' s (a -> a)
sign' = lexeme' sign

-- | Parses an integer (sign + numbers, sign if any)
--
-- >>> t' (sign <*> integer)  "-273.15 in Celsius"
-- Right (-273)
--
integer :: (Stream s, NFData s) => ParserS s Integer
integer = sign <*> decimals

-- | The t'ParserS'' form of `integer`
integer' :: (Stream s, NFData s) => ParserS' s Integer
integer' def = sign <*> decimals' def

-- | Convert a string parser into integer parser by evaluating the parsed with base
numbers
  :: (Stream s, NFData s) => Integer -> ParserS s String -> ParserS s Integer
numbers base parser = foldl' f 0 <$> parser
  where f x d = base * x + toInteger (digitToInt d)

-- | Parses general form of floating numbers (including scientific form)
--
-- >>> t' float  "3.1415926535e-8"
-- Right 3.1415926535e-8
--
float :: (Stream s, NFData s) => ParserS s Double
float = read <$> (scientific <|> floatOnly)
 where
  scientific = (<>) <$> floatOnly <*> exponent'
  floatOnly  = show <$> floating
  exponent'  = (:) <$> oneOf "eE" <*> (show <$> integer)

-- | The t'ParserS'' form of `float`
float' :: (Stream s, NFData s) => ParserS' s Double
float' = lexeme' float

-- | Parses format of @'decimals'.'decimals'@
-- (decimals + decimal point + decimal fractions)
--
-- >>> t' floating  "3.1415926535"
-- Right 3.1415926535
--
floating :: (Stream s, NFData s) => ParserS s Double
floating = read <$> foldl1 (liftA2 (<>)) [digits, string ".", digits]
  where digits = show <$> decimals

-- | The t'ParserS'' form of `floating`
floating' :: (Stream s, NFData s) => ParserS' s Double
floating' = lexeme' floating

-- | Parses identifiers based on the given `LanguageDef`
--
-- >>> t' (identifier' defDef) "function(arg1, arg2)"
-- Right "function"
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

-- | Parses operators or special-chars based on the given `LanguageDef`
--
-- >>> t' (digits *> skipSpaces *> operator' defDef) "3 + 4"
-- Right "+"
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

-- |
--
-- split-by-comma = splitBy (symbol ",")
-- split-by-semi  = splitBy (symbol ";")
-- split-by-space = splitBy spaces
--
-- >>> parserArgs = splitBy (symbol ",") alphaNums
-- >>> t' (symbol "(" *> parserArgs <* symbol ")") "(arg1,arg2,arg3)"
-- Right ["arg1","arg2","arg3"]
--
splitBy
  :: (Stream s, NFData s) => ParserS s String -> ParserS s a -> ParserS s [a]
splitBy = flip sepBy1

-- | Parses a single @char literal@
--
-- >>> stream = "'\CR', a carriage-return or '\LF', a line-feed?"
-- >>> t' charLit stream
-- Right '\r'
--
charLit :: (Stream s, NFData s) => ParserS s Char
charLit = string "'" *> readChar <* string "'"

-- | The same as `charLit`, but this reads /char-literal mark/ from `LanguageDef`
charLit' :: (Stream s, NFData s) => ParserS' s Char
charLit' def = lexeme' (string mark *> readChar <* string mark) def
  where mark = defCharLiteralMark def

-- | Parses a single @string literal@
--
-- >>> stream = "\"'\CR', a carriage-return or '\LF', a line-feed?\""
-- >>> t' stringLit stream
-- Right "'\r', a carriage-return or '\n', a line-feed?"
--
stringLit :: (Stream s, NFData s) => ParserS s String
stringLit = string "\"" *> manyTill readChar (string "\"")

-- | The same as `stringLit`, but this reads /string-literal mark/ from `LanguageDef`
stringLit' :: (Stream s, NFData s) => ParserS' s String
stringLit' def = lexeme' (string mark *> manyTill readChar (string mark)) def
  where mark = defStringLiteralMark def

-- |
readChar :: (Stream s, NFData s) => ParserS s Char
readChar = do
  s <- assert $ count 5 (eof <|> anychar)
  case readLitChar s of
    [(a, s')] -> a <$ skipCount (length s - length s') anychar
    _         -> fail "failed to read any char literal"
