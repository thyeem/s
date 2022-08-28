-----------------------------------------------------------------------------
-- |
-- Module      : Text.S.Combinator
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module defines a primitive char/string parser to be used for
-- extensions such as lexeme parsers and more complex structured parsers.
--
-----------------------------------------------------------------------------

module Text.S.Base
  ( module Text.S.Base
  ) where

import           Control.Applicative            ( liftA2 )
import           Control.DeepSeq                ( NFData )
import           Control.Monad                  ( mapM )
import           Data.Char                      ( isAlpha
                                                , isAlphaNum
                                                , isDigit
                                                , isHexDigit
                                                , isLower
                                                , isPunctuation
                                                , isSpace
                                                , isSymbol
                                                , isUpper
                                                )

import           Text.S.Combinator
import           Text.S.Internal


-------------------------
-- primitive parsers
-------------------------

-- | Parses a given single character
--
-- >>> t' (char 'p') "parser"
-- 'p'
--
char :: (Stream s, NFData s) => Char -> ParserS s Char
char c = charParserOf (== c) <?> show [c]

-- | Parses any single character
--
-- >>> t' anychar "$parser"
-- '$'
--
anychar :: (Stream s, NFData s) => ParserS s Char
anychar = charParserOf (const True) <?> "any character"

-- | Parses every single character except for a given character
--
-- >>> t' (some $ anycharBut 's') "parser"
-- "par"
--
anycharBut :: (Stream s, NFData s) => Char -> ParserS s Char
anycharBut c =
  charParserOf (/= c) <?> unwords ["any character except for", show c]

-- | Parses a given string
--
-- >>> t' (string "par") "parser"
-- "par"
--
string :: (Stream s, NFData s) => String -> ParserS s String
string s = mapM char s <?> show s

-- | Parses any string and consumes everything
--
-- >>> t' anystring "stop COVID-19"
-- "stop COVID-19"
--
anystring :: (Stream s, NFData s) => ParserS s String
anystring = some anychar <?> "any string"

-- | Parses any string except for a given string.
--
-- Probably this would be one of the most useful string parser for real-world use cases.
--
-- >>> t' (anystringBut "ID") "stop COVID-19"
-- "stop COV"
--
anystringBut :: (Stream s, NFData s) => String -> ParserS s String
anystringBut s = go
  where go = (assert (string s <|> eof') $> []) <|> liftA2 (:) anychar go

-- | Parses any single digit, the same as @__[0-9]__@
--
-- >>> t' digit "3.1415926535"
-- '3'
--
digit :: (Stream s, NFData s) => ParserS s Char
digit = charParserOf isDigit <?> "digit"

-- | Parses any single hexadecimal number, the same as @__[0-9a-f]__@
--
-- >>> t' (some hexDigit) "f8f8f8xyz"
-- "f8f8f8"
--
hexDigit :: (Stream s, NFData s) => ParserS s Char
hexDigit = charParserOf isHexDigit <?> "hex-digit"

-- | Parses any single alphabetical character, the same as @__[a-zA-Z]__@
--
-- >>> t' (some alpha) "stop COVID-19"
-- "stop"
--
alpha :: (Stream s, NFData s) => ParserS s Char
alpha = charParserOf isAlpha <?> "letter"

-- | The same as @__alpha__@
--
-- >>> t' (some letter) "COVID-19"
-- "COVID"
--
letter :: (Stream s, NFData s) => ParserS s Char
letter = alpha

-- | Parses any alphabetical or numeric character, the same as @__[0-9a-zA-Z]__@
--
-- >>> t' (some alphaNum) "year2022"
-- "year2022"
--
alphaNum :: (Stream s, NFData s) => ParserS s Char
alphaNum = charParserOf isAlphaNum <?> "letter-or-digit"

-- | Parses any single lowercase letter, the same as @__[a-z]__@
--
-- >>> t' (some lower) "covID-19"
-- "cov"
--
lower :: (Stream s, NFData s) => ParserS s Char
lower = charParserOf isLower <?> "lowercase-letter"

-- | Parses any single uppercase letter, the same as @__[A-Z]__@
--
-- >>> t' (some upper) "COVID-19"
-- "COVID"
--
upper :: (Stream s, NFData s) => ParserS s Char
upper = charParserOf isUpper <?> "uppercase-letter"

-- | Parses a single special character, @__anychar := alphaNum <|> special__@
--
-- >>> t' special "# stop COVID-19 -->"
-- '#'
--
special :: (Stream s, NFData s) => ParserS s Char
special = charParserOf isSpecial <?> "special-character"
  where isSpecial c = or $ ($ c) <$> [isPunctuation, isSymbol]

-- | Parses tab character, \t
--
-- >>> t' (string "stop" >> tab) "stop\tCOVID-19"
-- '\t'
--
tab :: (Stream s, NFData s) => ParserS s Char
tab = char '\t' <?> "tab"

-- | Parses LF or linefeed character, \n
--
-- >>> t' (string "stop" >> lf) "stop\nCOVID-19"
-- '\n'
--
lf :: (Stream s, NFData s) => ParserS s Char
lf = char '\n' <?> "linefeed"

-- | Parses CRLF or carrige return with linefeed, \r\n
--
-- >>> t' (string "stop" >> crlf) "stop\r\nCOVID-19"
-- '\n'
--
crlf :: (Stream s, NFData s) => ParserS s Char
crlf = (char '\r' *> char '\n') <?> "carriage-return + linefeed"

-- | Parses end-of-line character, the same as @__[LF | CRLF]__@
--
-- >>> t' (string "stop" >> some eol) "stop\n\r\nCOVID-19"
-- "\n\n"
--
eol :: (Stream s, NFData s) => ParserS s Char
eol = (lf <|> crlf) <?> "end-of-line"

-- | Parses a single whitespace character
--
-- >>> t' (some space) "  \n\tstop COVID-19"
-- "  \n\t"
--
space :: (Stream s, NFData s) => ParserS s Char
space = charParserOf isSpace <?> "space"

-- | Checks if the `State` applied to the parser is reached to
-- @__EOF__@ or /End-of-Stream/
--
-- When reached to @__EOF__@, it returns @\\__NUL__@.
--
-- It simply originated from @__(minBound::Char) == '\\NUL'__@.
--
-- >>> t' (anychar <|> eof) ""
-- '\NUL'
--
eof :: (Stream s, NFData s) => ParserS s Char
eof = label "end-of-stream" $ do
  s <- assert $ many anychar
  if null s
    then return '\NUL'
    else fail $ unwords ["EOF not found. Found char:", show . head $ s]

-- | The same as `eof`, but in the form of a string parser
--
-- >>> t' (anystring <|> eof') ""
-- "\NUL"
--
eof' :: (Stream s, NFData s) => ParserS s String
eof' = count 1 eof

-- | Parses if a character on parsing is in the given char-list
--
-- >>> t' (some $ oneOf "francis") "ascii-character-table"
-- "ascii"
--
oneOf :: (Stream s, NFData s) => [Char] -> ParserS s Char
oneOf cs = charParserOf (`elem` cs) <?> label'oneof
  where label'oneof = unwords ["one of", show ((: []) <$> cs)]

-- | Parses if a character on parsing is NOT in the given char-list
--
-- >>> t' (some $ noneOf "francis") "goldberg-variation"
-- "goldbe"
--
noneOf :: (Stream s, NFData s) => [Char] -> ParserS s Char
noneOf cs = charParserOf (`notElem` cs) <?> label'noneof
  where label'noneof = unwords ["none of", show ((: []) <$> cs)]

-- | Picks up one of prepared char-parsers by a string name
--
-- >>> t' (some $ selectp "special") "@${select} parsers by strings"
-- "@${"
--
selectp :: (Stream s, NFData s) => String -> ParserS s Char
selectp x = case x of
  "alpha"       -> alpha
  "alphaNum"    -> alphaNum
  "letter"      -> letter
  "digit"       -> digit
  "hexadecimal" -> hexDigit
  "lower"       -> lower
  "upper"       -> upper
  "space"       -> space
  "eol"         -> eol
  "special"     -> special
  "anychar"     -> anychar
  c | length c == 1 -> char . head $ c
    | otherwise     -> fail $ unwords ["not found parser such as: ", c]
