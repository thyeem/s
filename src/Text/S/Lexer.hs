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
                                                , readLitChar
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
-- => (`LanguageDef` -> `Parser'S` s a) `LanguageDef`
-- => `Parser'S` s a
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

-- | The `Lexer` form of `token`
token' :: (Stream s, NFData s) => String -> Lexer s String
token' t = lexeme (token t)

-- |
letters :: (Stream s, NFData s) => Parser'S s String
letters = some letter

-- | The `Lexer` form of `letters`
letters' :: (Stream s, NFData s) => Lexer s String
letters' = lexeme letters

-- |
alphaNums :: (Stream s, NFData s) => Parser'S s String
alphaNums = some alphaNum

-- | The `Lexer` form of `alphaNums`
alphaNums' :: (Stream s, NFData s) => Lexer s String
alphaNums' = lexeme alphaNums

-- |
digits :: (Stream s, NFData s) => Parser'S s String
digits = some digit

-- | The `Lexer` form of `digits`
digits' :: (Stream s, NFData s) => Lexer s String
digits' = lexeme digits

-- |
specials :: (Stream s, NFData s) => Parser'S s String
specials = some special

-- | The `Lexer` form of `specials`
specials' :: (Stream s, NFData s) => Lexer s String
specials' = lexeme specials

-- |
spaces :: (Stream s, NFData s) => Parser'S s String
spaces = some space

-- | The `Lexer` form of `spaces``
spaces' :: (Stream s, NFData s) => Lexer s String
spaces' = lexeme spaces

-- | Parses string between parentheses
--
-- >>> t' (parens letters) "(parser)"
-- Right "parser"
--
parens :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
parens = between (token "(") (token ")")

-- | The `Lexer` form of `parens`
parens' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
parens' p = lexeme (parens p)

-- | Parses string between curly braces
--
-- >>> t' (braces letters) "{parser}"
-- Right "parser"
--
braces :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
braces = between (token "{") (token "}")

-- | The `Lexer` form of `braces`
braces' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
braces' p = lexeme (braces p)

-- | Parses string between angle brackets
--
-- >>> t' (angles letters) "<parser>"
-- Right "parser"
--
angles :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
angles = between (token "<") (token ">")

-- | The `Lexer` form of `angles`
angles' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
angles' p = lexeme (angles p)

-- | Parses string between square brackets
--
-- >>> t' (squares letters) "[parser]"
-- Right "parser"
--
squares :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
squares = between (token "[") (token "]")

-- | The `Lexer` form of `squares`
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

-- | The `Lexer` form of `decimals`
decimals' :: (Stream s, NFData s) => Lexer s Integer
decimals' = lexeme decimals

-- | Parses hexadecimal digits (base-16)
--
-- >>> t' hexadecimals "0xCOVID-19"
-- Right 12
--
hexadecimals :: (Stream s, NFData s) => Parser'S s Integer
hexadecimals = skipOptional (string "0x") *> numbers 16 (some hexDigit)

-- | The `Lexer` form of `hexadecimals`
hexadecimals' :: (Stream s, NFData s) => Lexer s Integer
hexadecimals' = lexeme hexadecimals

-- | Parses numbers with leading-zeros
--
-- >>> t' zeros "000002022"
-- Right 2022
--
zeros :: (Stream s, NFData s) => Parser'S s Integer
zeros = char '0' *> decimals

-- | The `Lexer` form of `zeros`
zeros' :: (Stream s, NFData s) => Lexer s Integer
zeros' = lexeme zeros

-- | Parses natural numbers (non-leading zeros and signs)
--
-- >>> t' natural "27182818284"
-- Right 27182818284
--
natural :: (Stream s, NFData s) => Parser'S s Integer
natural = assert digit *> assert (anycharBut '0') *> decimals

-- | The `Lexer` form of `natural`
natural' :: (Stream s, NFData s) => Lexer s Integer
natural' = lexeme natural

-- | Parses a sign (@+@ or @-@) and lift the corresponding function.
--
-- >>> t' (sign <*> floating)  "-273.15 in Celsius"
-- Right (-273.15)
--
sign :: (Stream s, NFData s, Num a) => Parser'S s (a -> a)
sign = (char '-' $> negate) <|> (char '+' $> id) <|> pure id

-- | The same to `sign` but strip whitespaces between sign and numbers.
--
-- >>> t' (sign' defDef <*> floating)  "-  273.15 in Celsius"
-- Right (-273.15)
--
sign' :: (Stream s, NFData s, Num a) => Lexer s (a -> a)
sign' = lexeme sign

-- | Parses an integer (sign + numbers, sign if any)
--
-- >>> t' (sign <*> integer)  "-273.15 in Celsius"
-- Right (-273)
--
integer :: (Stream s, NFData s) => Parser'S s Integer
integer = sign <*> decimals

-- | The `Lexer` form of `integer`
integer' :: (Stream s, NFData s) => Lexer s Integer
integer' def = sign <*> decimals' def

-- | Convert a string parser into integer parser by evaluating the parsed with base
numbers
  :: (Stream s, NFData s) => Integer -> Parser'S s String -> Parser'S s Integer
numbers base parser = foldl' f 0 <$> parser
  where f x d = base * x + toInteger (digitToInt d)

-- | Parses general form of floating numbers (including scientific form)
--
-- >>> t' float  "3.1415926535e-8"
-- Right 3.1415926535e-8
--
float :: (Stream s, NFData s) => Parser'S s Double
float = read <$> (scientific <|> floatOnly)
 where
  scientific = (<>) <$> floatOnly <*> exponent'
  floatOnly  = show <$> floating
  exponent'  = (:) <$> oneOf "eE" <*> (show <$> integer)

-- | The `Lexer` form of `float`
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

-- | The `Lexer` form of `floating`
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
-- >>> t' (digits *> skipSpaces *> operator' defDef) "3 + 4"
-- Right "+"
--
operator' :: (Stream s, NFData s) => Lexer s String
operator' def = do
  op <- specials
  if isReserved op def
    then fail $ unwords ["reserved operator used:", show op]
    else skip def $> op
 where
  isReserved o def = S.member o set
  set = S.fromList (defReservedSpecials def)

-- |
--
-- split-by-comma = splitBy (token ",")
-- split-by-semi  = splitBy (token ";")
-- split-by-space = splitBy spaces
--
-- >>> parserArgs = splitBy (token ",") alphaNums
-- >>> t' (token "(" *> parserArgs <* token ")") "(arg1,arg2,arg3)"
-- Right ["arg1","arg2","arg3"]
--
splitBy
  :: (Stream s, NFData s) => Parser'S s String -> Parser'S s a -> Parser'S s [a]
splitBy = flip sepBy1

-- |
charLit' :: (Stream s, NFData s) => Lexer s Char
charLit' = lexeme (char '\'' *> undefined <* char '\'')

-- |
stringLit' :: (Stream s, NFData s) => Lexer s String
stringLit' def = char '"' *> manyTill (charLit' def) (char '"')

-- |
-- charLit :: (Stream s, NFData s) => Parser'S s Char
-- charLit = do
  -- _ <- char '\''
  -- readLitChar
  -- _ <- char '\''

-- |
-- stringLit :: (Stream s, NFData s) => Parser'S s String
-- stringLit def = char '"' *> manyTill charLit (char '"')
