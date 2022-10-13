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

import           Control.Applicative            ( (<|>)
                                                , liftA2
                                                )
import           Data.Char                      ( digitToInt
                                                , readLitChar
                                                , toLower
                                                )
import           Data.Functor                   ( ($>) )
import           Data.List                      ( foldl'
                                                , foldl1'
                                                )
import qualified Data.Set                      as S
import           Text.S.Base
import           Text.S.Combinator
import           Text.S.Internal
import           Text.S.Language


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

-- | Parses floating numbers including scientific notations.
-- Every part in the coefficient is strictly included.
--
-- [strict 'floating'] or [strict 'scientific']
--
-- See also 'floating' and 'scientific''.
--
-- >>> tt float "3.1415926535e-5"
-- 3.1415926535e-5
--
float :: Stream s => ParserS s Double
float = scientific <|> floating
{-# INLINE float #-}

-- | Parses floating numbers including scientific notations.
-- The coefficients in scientific format allow the omitted forms.
-- The whole-number-part in floating form is optional.
--
-- [permissive 'floating'] or [permissive 'scientific']
--
-- See also 'floatingA' and 'scientific''.
--
-- >>> tt floatA "3.e-5"
-- 3.0e-5
--
floatA :: Stream s => ParserS s Double
floatA = scientific' <|> floatingA
{-# INLINE floatA #-}

-- | Parses floating numbers including scientific notations.
-- The coefficients in scientific format allow the omitted forms.
-- The deciaml-point-part in floating form is optional.
--
-- [permissive 'floating'] or [permissive 'scientific']
--
-- See also 'floatingB' and 'scientific''.
--
-- >>> tt floatB ".1415926535e-5"
-- 1.415926535e-6
--
floatB :: Stream s => ParserS s Double
floatB = scientific' <|> floatingB
{-# INLINE floatB #-}

-- | Parses floating number in format of
-- [whole-number] + [decimal-point(.)] + [decimal-part]
--
-- This is the most strict form and every part must be included.
--
-- >>> tt floating "3.1415926535"
-- 3.1415926535
--
floating :: Stream s => ParserS s Double
floating = read <$> genFloating digits digits
{-# INLINE floating #-}

-- | Parses floating number in format of
-- [whole-number] + [decimal-point(.)] + [(optional) decimal-part]
--
-- This is the floating form with optional decimal part.
--
-- >>> tt floatingA "3."
-- 3.0
--
floatingA :: Stream s => ParserS s Double
floatingA = read <$> genFloating digits (option "0" digits)
{-# INLINE floatingA #-}

-- | Parses floating number in format of
-- [(optional) whole-number] + [decimal-point(.)] + [decimal-part]
--
-- This is the floating form with optional whole number part.
--
-- >>> tt floatingB ".1415926535"
-- 0.1415926535
--
floatingB :: Stream s => ParserS s Double
floatingB = read <$> genFloating (option "0" digits) digits
{-# INLINE floatingB #-}

-- | Parser builder for several types of floating numbers
genFloating
  :: Stream s => ParserS s String -> ParserS s String -> ParserS s String
genFloating wholeNumber decimalPart = foldl1'
  (liftA2 (<>))
  [sign, wholeNumber, string ".", decimalPart]
  where sign = option mempty (string "-" <|> (string "+" $> mempty))
{-# INLINE genFloating #-}


-- | Parses floating number in scientific notations of
-- [strict coefficient] + [e or E] + [exponent]
--
-- Every part in the coefficient must be strict.
--
-- See 'floating'.
--
-- >>> tt scientific "2.7182818284E3"
-- 2718.2818284
--
scientific :: Stream s => ParserS s Double
scientific = read <$> genScientific (genFloating digits digits)
{-# INLINE scientific #-}

-- | Parses floating number in scientific notations of
-- [floating or int coefficient] + [e or E] + [exponent]
--
-- This allow the omitted forms in the coefficient with
-- optional whole number parts or optional decimal parts.
--
-- >>> tt scientific' "2.E3"
-- 2000.0
--
-- >>> tt scientific' ".7182818284E3"
-- 718.2818284
--
scientific' :: Stream s => ParserS s Double
scientific' =
  read
    <$> (   genScientific (genFloating digits (option "0" digits))
        <|> genScientific (genFloating (option "0" digits) digits)
        )
{-# INLINE scientific' #-}

-- | Parser builder for several types of numbers in scientific format
genScientific :: Stream s => ParserS s String -> ParserS s String
genScientific flt = liftA2 (<>) coeff expt
 where
  coeff = flt <|> int
  expt  = liftA2 (:) (oneOf "eE") int
  int   = liftA2 (<>) sign digits
  sign  = option mempty (string "-" <|> (string "+" $> mempty))


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

  if isReserved found
    then fail $ unwords ["reserved identifier used:", show found]
    else skips def $> found

 where
  isReserved name | caseSensitive = S.member name set
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
  if isReserved op
    then fail $ unwords ["reserved operator used:", show op]
    else skips def $> op
 where
  isReserved o = S.member o set
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
charLit' = genCharLit . defCharLiteralMark
{-# INLINABLE charLit' #-}

-- | Character literal parser builder
genCharLit :: Stream s => ParserS s String -> ParserS s Char
genCharLit mark = between mark (mark <?> "end-of-char-literal") readChar
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
stringLit' = genStringLit . defStringLiteralMark
{-# INLINABLE stringLit' #-}

-- | String literal parser builder
genStringLit :: Stream s => ParserS s String -> ParserS s String
genStringLit mark = concat
  <$> between mark (mark <?> "end-of-string-literal") (many character)
 where
  character = choice
    [ pure <$> noneOf "\\\"\0\n\r\t\b\v\f"
    , sequence [char '\\', oneOf "\\\"0nrtbvf"]
    ]
{-# INLINABLE genStringLit #-}
