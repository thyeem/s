-----------------------------------------------------------------------------
-- |
-- Module      : Text.S.Lexer
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module constructs a lexer or a tokenizer parser to parse
-- __lexical units__ (or /lexemes/) based on parser combinators.
--
-----------------------------------------------------------------------------

module Text.S.Lexer
  ( module Text.S.Lexer
  ) where

import           Control.DeepSeq                ( NFData
                                                , force
                                                )
import           Data.Char                      ( digitToInt
                                                , isAlpha
                                                , toLower
                                                , toUpper
                                                )
import           Data.Foldable                  ( foldl' )
import qualified Data.Set                      as S
import           Text.S.Base
import           Text.S.Combinator
import           Text.S.Internal
import           Text.S.Language

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( mzero )



-- | Defines lexical-token-parsers: Lexer
--
-- - A @Parser@ is a superset of a @Lexer@
--
-- +---------+----------------------------------------------------------------+
-- | parsing | doing parse-job with primitive parsers and theirs combinations |
-- +---------+----------------------------------------------------------------+
-- | lexing  | doing parse-job with parsers and /language definitions/        |
-- +---------+----------------------------------------------------------------+
--
-- - Main difference between @Lexer@ and @Parser@
--
-- +----------+--------------------------------------------------------+
-- | @Parser@ | `LanguageDef`-independent and has a name of /funcName/ |
-- +----------+--------------------------------------------------------+
-- | @Lexer@  | `LanguageDef`-dependent and has a name of /funcName'/  |
-- +----------+--------------------------------------------------------+
--
-- @
-- __lexemeFunc__ /langDef/
-- = `Lexer` `LanguageDef`
-- = (`LanguageDef` -> `Parser'S`) `LanguageDef`
-- = `Parser'S`
-- @
--
type Lexer s a = LanguageDef -> Parser'S s a


-- | Make a given parser @p@ a lexer or lexical-unit parser
-- based on the given language definition
lexeme :: (Stream s, NFData s) => Parser'S s a -> Lexer s a
lexeme p def = p <* skip def

-- | Skips unnecesary whitespaces and comments
--
-- >>> input = "# line-comment\n\r\n '''inner block''' should-be-here"
-- >>> unwrap' $ t (skip defDef) input
-- "should-be-here"
--
skip :: (Stream s, NFData s) => Lexer s ()
skip def = skipMany $ skipSpaces <|> skipComments' def

-- | Skips whitespaces
skipSpaces :: (Stream s, NFData s) => Parser'S s ()
skipSpaces = skipSome space

-- | Skips line and block comments
skipComments' :: (Stream s, NFData s) => Lexer s ()
skipComments' def =
  skipSome (commentLine' def) <|> skipSome (commentBlock' def)

-- | Parses single-line comment
commentLine' :: (Stream s, NFData s) => Lexer s String
commentLine' def = p *> manyTill anychar eol
  where p = choice $ string <$> defCommentLine def

-- | Parses block comment
commentBlock' :: (Stream s, NFData s) => Lexer s String
commentBlock' def = bra *> manyTill anychar ket
 where
  bra = choice $ string <$> defCommentBlockBegin def
  ket = choice $ string <$> defCommentBlockEnd def

-- | Parses any token to comsume. The same to `string`
token :: (Stream s, NFData s) => String -> Parser'S s String
token = string

-- | Lexer form of `token`
token' :: (Stream s, NFData s) => String -> Lexer s String
token' t = lexeme (token t)

-- |
letters :: (Stream s, NFData s) => Parser'S s String
letters = some letter

-- | Lexer form of `letters`
letters' :: (Stream s, NFData s) => Lexer s String
letters' = lexeme letters

-- |
alphaNums :: (Stream s, NFData s) => Parser'S s String
alphaNums = some alphaNum

-- | Lexer form of `alphaNums`
alphaNums' :: (Stream s, NFData s) => Lexer s String
alphaNums' = lexeme alphaNums

-- |
digits :: (Stream s, NFData s) => Parser'S s String
digits = some digit

-- | Lexer form of `digits`
digits' :: (Stream s, NFData s) => Lexer s String
digits' = lexeme digits

-- | Parses string between parentheses
--
-- >>> t' (parens letters) "(parser)"
-- Right "parser"
--
parens :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
parens = between (token "(") (token ")")

-- | Lexer form of `parens`
parens' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
parens' p = lexeme (parens p)

-- | Parses string between curly braces
--
-- >>> t' (braces letters) "{parser}"
-- Right "parser"
--
braces :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
braces = between (token "{") (token "}")

-- | Lexer form of `braces`
braces' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
braces' p = lexeme (braces p)

-- | Parses string between angle brackets
--
-- >>> t' (angles letters) "<parser>"
-- Right "parser"
--
angles :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
angles = between (token "<") (token ">")

-- | Lexer form of `angles`
angles' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
angles' p = lexeme (angles p)

-- | Parses string between square brackets
--
-- >>> t' (squares letters) "[parser]"
-- Right "parser"
--
squares :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
squares = between (token "[") (token "]")

-- | Lexer form of `squares`
squares' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
squares' p = lexeme (squares p)

-- | Parses natural numbers or decimal digits (base-10)
-- This includes numbers with leading zeros
--
-- >>> t' decimals "00123456789"
-- Right 123456789
--
decimals :: (Stream s, NFData s) => Parser'S s Integer
decimals = numbers 10 digits

-- | Lexer form of `decimals`
decimals' :: (Stream s, NFData s) => Lexer s Integer
decimals' = lexeme decimals

-- | Parses hexadecimal digits (base-16)
--
-- >>> t' hexadecimals "0xCOVID-19"
-- Right 12
--
hexadecimals :: (Stream s, NFData s) => Parser'S s Integer
hexadecimals = skipOptional (string "0x") *> numbers 16 (some hexDigit)

-- | Lexer form of `hexadecimals`
hexadecimals' :: (Stream s, NFData s) => Lexer s Integer
hexadecimals' = lexeme hexadecimals

-- | Parses numbers with leading-zeros
--
-- >>> t' zeros "000002022"
-- Right 2022
--
zeros :: (Stream s, NFData s) => Parser'S s Integer
zeros = char '0' *> decimals

-- | Lexer form of `zeros`
zeros' :: (Stream s, NFData s) => Lexer s Integer
zeros' = lexeme zeros

-- | Parses natural numbers (non-leading zeros and signs)
--
-- >>> t' natural "27182818284"
-- Right 27182818284
--
natural :: (Stream s, NFData s) => Parser'S s Integer
natural = try digit *> try (anycharBut '0') *> decimals

-- | Lexer form of `natural`
natural' :: (Stream s, NFData s) => Lexer s Integer
natural' = lexeme natural

-- | Parses a sign (@+@ or @-@) from an integer and lift its operation.
--
-- >>> t' (sign <*> decimals)  "-2022"
-- Right (-2022)
--
sign :: (Stream s, NFData s) => Parser'S s (Integer -> Integer)
sign = (char '-' $> negate) <|> (char '+' $> id) <|> pure id

-- | The same to `sign` but strip whitespaces between sign and digits.
--
-- >>> t' (sign' defDef <*> decimals) "-  2022"
-- Right (-2022)
--
sign' :: (Stream s, NFData s) => Lexer s (Integer -> Integer)
sign' = lexeme sign

-- | Parses an integer (sign + numbers, sign if any)
--
-- >>> t' integer  "-2022"
-- Right (-2022)
--
integer :: (Stream s, NFData s) => Parser'S s Integer
integer = sign <*> decimals

-- | Lexer form of `integer`
integer' :: (Stream s, NFData s) => Lexer s Integer
integer' def = sign <*> decimals' def

-- | Convert a string parser into integer parser by evaluating the parsed with base
numbers
  :: (Stream s, NFData s) => Integer -> Parser'S s String -> Parser'S s Integer
numbers base parser = foldl' f 0 <$> parser
  where f x d = base * x + toInteger (digitToInt d)

-- | Parses general form of floating number (including scientific form)
--
-- >>> t' float  "314.15926535e-10"
-- Right 3.1415926535e-8
--
float :: (Stream s, NFData s) => Parser'S s Double
float = read <$> (scientific <|> floatOnly)
 where
  scientific = (<>) <$> floatOnly <*> exponent'
  floatOnly  = show <$> floating
  exponent'  = (:) <$> oneOf "eE" <*> (show <$> integer)

-- | Lexer form of `float`
float' :: (Stream s, NFData s) => Lexer s Double
float' = lexeme float

-- | Parses format of @'decimals'.'decimals'@
-- (decimals + decimal point + decimal fractions)
--
-- >>> t' floating  "3.1415926535"
-- Right 3.1415926535
--
floating :: (Stream s, NFData s) => Parser'S s Double
floating = read <$> foldl1 (liftA2 (<>)) [digits, string ".", digits]
  where digits = show <$> decimals

-- | Lexer form of `floating`
floating' :: (Stream s, NFData s) => Lexer s Double
floating' = lexeme floating

-- | Parses identifiers based on the given `LanguageDef`
--
-- >>> t' (identifier' defDef) "function(arg1, arg2)"
-- Right "function"
--
identifier' :: (Stream s, NFData s) => Lexer s String
identifier' def = do
  let begin     = choice $ some . selectp <$> defIdentifierBegin def
  let remainder = choice $ many . selectp <$> defIdentifierName def
  found <- (<>) <$> begin <*> remainder

  if isReserved found def
    then fail $ unwords ["reserved identifier used:", show found]
    else skip def $> found

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
-- >>> t' (digit *> skipSpaces *> operator' defDef) "3 + 4"
-- Right "+"
--
operator' :: (Stream s, NFData s) => Lexer s String
operator' def = do
  op <- some special
  if isReserved op def
    then fail $ unwords ["reserved operator used:", show op]
    else skip def $> op
 where
  isReserved o def = S.member o set
  set = S.fromList (defReservedSpecials def)

-- |
sepByComma :: (Stream s, NFData s) => Parser'S s a -> Parser'S s [a]
sepByComma p = sepBy p (token ",")
