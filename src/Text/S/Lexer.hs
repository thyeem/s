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
    else return found

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

-- | helper function:
caseGuard :: (Stream s, NFData s) => Bool -> String -> Parser'S s String
caseGuard caseSensitive s | caseSensitive = string s
                          | otherwise     = mapM char' s
 where
  char' c | isAlpha c = char (toLower c) <|> char (toUpper c)
          | otherwise = char c

-- | helper function:
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

-- |
token' :: (Stream s, NFData s) => String -> Lexer s String
token' t = lexeme (token t)

-- |
decimals :: (Stream s, NFData s) => Parser'S s Integer
decimals = many (char '0') *> numbers 10 (some digit)

-- |
decimals' :: (Stream s, NFData s) => Lexer s Integer
decimals' = lexeme decimals

-- |
hexadecimals :: (Stream s, NFData s) => Parser'S s Integer
hexadecimals = skipOptional (string "0x") *> numbers 16 (some hexDigit)

-- |
hexadecimals' :: (Stream s, NFData s) => Lexer s Integer
hexadecimals' = lexeme decimals

-- | parses string between parentheses
--
-- >>> t' (parens (some letter)) "(parser)"
-- Right "parser"
--
parens :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
parens = between (token "(") (token ")")

-- | lexemizes string between parentheses
--
-- >>> t' (parens' (some letter) defaultDef) "(lexer)\n\r\n\t  "
-- Right "lexer"
--
parens' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
parens' p = lexeme (parens p)

-- | parses string between curly braces
--
-- >>> t' (braces (some letter)) "{parser}"
-- Right "parser"
--
braces :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
braces = between (token "{") (token "}")

-- | lexemizes string between curly braces
--
-- >>> t' (braces' (some letter) defaultDef) "{lexer}\n\r\n\t  "
-- Right "lexer"
--
braces' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
braces' p = lexeme (braces p)

-- | parses string between angle brackets
--
-- >>> t' (angles (some letter)) "<parser>"
-- Right "parser"
--
angles :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
angles = between (token "<") (token ">")

-- | lexemizes string between angle brackets
--
-- >>> t' (angles' (some letter) defaultDef) "<lexer>\n\r\n\t  "
-- Right "lexer"
--
angles' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
angles' p = lexeme (angles p)

-- | parses string between square brackets
--
-- >>> t' (squares (some letter)) "[parser]"
-- Right "parser"
--
squares :: (Stream s, NFData s) => Parser'S s String -> Parser'S s String
squares = between (token "[") (token "]")

-- | lexemizes string between square brackets
--
-- >>> t' (squares' (some letter) defaultDef) "[lexer]\n\r\n\t  "
-- Right "lexer"
--
squares' :: (Stream s, NFData s) => Parser'S s String -> Lexer s String
squares' p = lexeme (squares p)

-- |

-- |

-- |
sign :: (Stream s, NFData s) => Parser'S s (Integer -> Integer)
sign = (char '-' $> negate) <|> (char '+' $> id) <|> pure id

-- |
sign' :: (Stream s, NFData s) => Lexer s (Integer -> Integer)
sign' = lexeme sign

-- |
integer :: (Stream s, NFData s) => Parser'S s Integer
integer = sign <*> decimals

-- |
integer' :: (Stream s, NFData s) => Lexer s Integer
integer' def = sign' def <*> decimals

-- | parses consecutive (many) number-strings
numbers
  :: (Stream s, NFData s) => Integer -> Parser'S s String -> Parser'S s Integer
numbers base parser = foldl' f 0 <$> parser
  where f x d = base * x + toInteger (digitToInt d)
