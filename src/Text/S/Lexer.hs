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
-- - Main difference between `Lexer` and `Parser`
--
-- +----------+--------------------------------------------------------+
-- | @Parser@ | `LanguageDef`-independent and has a name of /funcName/ |
-- +----------+--------------------------------------------------------+
-- | @Lexer@  | `LanguageDef`-dependent and has a name of /funcName'/  |
-- +----------+--------------------------------------------------------+
--
-- @Parser@ = @Lexer@ + @LanguageDef@
--
type Lexer s a = LanguageDef -> Parser'S s a


-- | Make a given parser @p@ a lexer or lexical-unit parser
-- based on the given language definition
lexeme :: (Stream s, NFData s) => Parser'S s a -> Lexer s a
lexeme p def = p <* jump def

-- | Skips unnecesary whitespaces and comments
--
-- >>> input = "# line-comment\n\r\n '''inner block''' end-of-jump"
-- >>> unwrap' $ t (jump defaultDef) input
-- "end-of-jump"
--
jump :: (Stream s, NFData s) => Lexer s ()
jump def = skipMany $ jumpSpaces <|> jumpComments def

-- | Skips whitespaces
jumpSpaces :: (Stream s, NFData s) => Parser'S s ()
jumpSpaces = skipSome space

-- | Skips line and block comments
jumpComments :: (Stream s, NFData s) => Lexer s ()
jumpComments def = skipSome (commentLine def) <|> skipSome (commentBlock def)

-- | Lexemizes single-line comment
commentLine :: (Stream s, NFData s) => Lexer s String
commentLine def = p *> manyTill anychar eol
  where p = choice $ string <$> defCommentLine def

-- | Lexemizes block comment
commentBlock :: (Stream s, NFData s) => Lexer s String
commentBlock def = bra *> manyTill anychar ket
 where
  bra = choice $ string <$> defCommentBlockBegin def
  ket = choice $ string <$> defCommentBlockEnd def

-- | Lexemizes an identifier based on the given `LanguageDef`
identifier' :: (Stream s, NFData s) => Lexer s String
identifier' def = do
  let begin     = choice $ selectParser True <$> defIdentifierBegin def
  let remainder = choice $ selectParser False <$> defIdentifierName def
  found <- (<>) <$> begin <*> remainder
  if isReserved found def
    then fail $ unwords ["reserved identifier used:", show found]
    else jump def $> found

-- | Check if a given name is already reserved in the language definition
isReserved :: String -> LanguageDef -> Bool
isReserved name def | caseSensitive = S.member name set
                    | otherwise     = S.member (lower name) set
 where
  lower         = (toLower <$>)
  caseSensitive = defCaseSensitive def
  reservedNames = defReservedNames def
  set | caseSensitive = S.fromList reservedNames
      | otherwise     = S.fromList $ lower <$> reservedNames

-- | Helper function:
caseGuard :: (Stream s, NFData s) => Bool -> String -> Parser'S s String
caseGuard caseSensitive s | caseSensitive = string s
                          | otherwise     = mapM char' s
 where
  char' c | isAlpha c = char (toLower c) <|> char (toUpper c)
          | otherwise = char c

-- | Helper function:
selectParser :: (Stream s, NFData s) => Bool -> String -> Parser'S s String
selectParser beg x = case x of
  "alpha"    -> f alpha
  "alphaNum" -> f alphaNum
  "letter"   -> f letter
  "digit"    -> f digit
  "lower"    -> f lower
  "upper"    -> f upper
  c          -> string c
  where f = if beg then some else many

-- | Parses any token to comsume. The same to `string`
token :: (Stream s, NFData s) => String -> Parser'S s String
token = string

-- | Lexemizes any lexemes to comsume.
token' :: (Stream s, NFData s) => String -> Lexer s String
token' t = lexeme (token t)

-- | Parses string between parentheses
--
-- >>> t' (parens (some letter)) "(parser)"
-- Right "parser"
--
parens :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
parens = between (token "(") (token ")")

-- | Lexemizes string between parentheses
--
-- >>> t' (parens' (some letter) defaultDef) "(lexer)\n\r\n\t  "
-- Right "lexer"
--
parens' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
parens' p = lexeme (parens p)

-- | Parses string between curly braces
--
-- >>> t' (braces (some letter)) "{parser}"
-- Right "parser"
--
braces :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
braces = between (token "{") (token "}")

-- | Lexemizes string between curly braces
--
-- >>> t' (braces' (some letter) defaultDef) "{lexer}\n\r\n\t  "
-- Right "lexer"
--
braces' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
braces' p = lexeme (braces p)

-- | Parses string between angle brackets
--
-- >>> t' (angles (some letter)) "<parser>"
-- Right "parser"
--
angles :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
angles = between (token "<") (token ">")

-- | Lexemizes string between angle brackets
--
-- >>> t' (angles' (some letter) defaultDef) "<lexer>\n\r\n\t  "
-- Right "lexer"
--
angles' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
angles' p = lexeme (angles p)

-- | Parses string between square brackets
--
-- >>> t' (squares (some letter)) "[parser]"
-- Right "parser"
--
squares :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
squares = between (token "[") (token "]")

-- | Lexemizes string between square brackets
--
-- >>> t' (squares' (some letter) defaultDef) "[lexer]\n\r\n\t  "
-- Right "lexer"
--
squares' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
squares' p = lexeme (squares p)

-- | Parses natural numbers or decimal digits (base-10)
-- This includes numbers with leading zeros
--
-- >>> t' decimals "00123456789"
-- Right 123456789
--
decimals :: (Stream s, NFData s) => Parser'S s Integer
decimals = numbers 10 (some digit)

-- | Lexemizes natural numbers or decimal digits (base-10)
-- This includes numbers with leading zeros
--
-- >>> t' (decimals' defaultDef) "00123456789   "
-- Right 123456789
--
decimals' :: (Stream s, NFData s) => Lexer s Integer
decimals' = lexeme decimals

-- | Parses hexadecimal digits (base-16)
--
-- >>> t' hexadecimals "0xCOVID-19"
-- Right 12
--
hexadecimals :: (Stream s, NFData s) => Parser'S s Integer
hexadecimals = skipOptional (string "0x") *> numbers 16 (some hexDigit)

-- | Lexemizes hexadecimal digits (base-16)
--
-- >>> t' (hexadecimals' defaultDef) "0xCOVID-19"
-- Right 12
--
hexadecimals' :: (Stream s, NFData s) => Lexer s Integer
hexadecimals' = lexeme hexadecimals

-- | Parses numbers with leading-zeros
--
-- >>> t' zeros "000002022"
-- Right 2022
--
zeros :: (Stream s, NFData s) => Parser'S s Integer
zeros = char '0' *> decimals

-- | Lexemizes numbers with leading-zeros
--
-- >>> t' (zeros' defaultDef) "000002022   "
-- Right 2022
--
zeros' :: (Stream s, NFData s) => Lexer s Integer
zeros' = lexeme zeros

-- | Parses natural numbers (non-leading zeros and signs)
--
-- >>> t' natural "27182818284"
-- Right 27182818284
--
natural :: (Stream s, NFData s) => Parser'S s Integer
natural = try digit *> try (anycharBut '0') *> decimals

-- | Lexemizes natural numbers (non-leading zeros and signs)
--
-- >>> t' (natural' defaultDef) "2718284"
-- Right 2718284
--
natural' :: (Stream s, NFData s) => Lexer s Integer
natural' = lexeme natural

-- | Parses a `sign` (+ or -) from an integer and lift it.
--
-- >>> t' (sign <*> decimals)  "-2022"
-- Right (-2022)
--
sign :: (Stream s, NFData s) => Parser'S s (Integer -> Integer)
sign = (char '-' $> negate) <|> (char '+' $> id) <|> pure id

-- | Lexemizes a `sign` (+ or -) from an integer and lift it.
--
-- >>> t' (sign' defaultDef <*> decimals' defaultDef) "-  2022  "
-- Right (-2022)
--
sign' :: (Stream s, NFData s) => Lexer s (Integer -> Integer)
sign' = lexeme sign

-- |
-- >>> t' integer  "-2022"
-- Right (-2022)
--
integer :: (Stream s, NFData s) => Parser'S s Integer
integer = sign <*> decimals

-- |
-- >>> t' (integer' defaultDef) "-  2022  "
-- Right (-2022)
--
integer' :: (Stream s, NFData s) => Lexer s Integer
integer' def = sign' def <*> decimals' def

-- | Parses consecutive (many) number-strings
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

-- | Lexemes general form of floating number (including scientific form)
--
-- >>> t' (float' defaultDef)  "314.15926535e-10   "
-- Right 3.1415926535e-8
--
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

-- | Lexemizes format of @'decimals'.'decimals'@
-- (decimals + decimal point + decimal fractions)
--
-- >>> t' (floating' defaultDef) "3.1415926535   "
-- Right 3.1415926535
--
floating' :: (Stream s, NFData s) => Lexer s Double
floating' = lexeme floating


sepByComma :: (Stream s, NFData s) => Parser'S s a -> Parser'S s [a]
sepByComma p = sepBy p (token ",")
