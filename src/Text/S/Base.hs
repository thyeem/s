-----------------------------------------------------------------------------
-- |
-- Module      : Text.S.Combinator
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module defines a primitive char/string parser to be used for
-- extensions such as lexers more complex structured parsers.
--
-----------------------------------------------------------------------------

module Text.S.Base
  ( module Text.S.Base
  ) where

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
-- Right 'p'
--
char :: (Stream s, NFData s) => Char -> Parser'S s Char
char c = charParserOf (== c) <?> show [c]

-- | Parses any single character
--
-- >>> t' anychar "$parser"
-- Right '$'
--
anychar :: (Stream s, NFData s) => Parser'S s Char
anychar = charParserOf (const True) <?> "all kinds of character"

-- | Parses every single character except for a given character
--
-- >>> t' (some $ anycharBut 's') "parser"
-- Right "par"
--
anycharBut :: (Stream s, NFData s) => Char -> Parser'S s Char
anycharBut c =
  charParserOf (/= c) <?> unwords ["any character except for", show c]

-- | Parses a given string
--
-- >>> t' (string "par") "parser"
-- Right "par"
--
string :: (Stream s, NFData s) => String -> Parser'S s String
string = mapM char

-- | Parses any string and consumes everything
--
-- >>> t' anystring "stop COVID-19"
-- Right "stop COVID-19"
--
anystring :: (Stream s, NFData s) => Parser'S s String
anystring = many anychar

-- | Parses any single digit, @[0-9]@
--
-- >>> t' digit "3.1415926535"
-- Right '3'
--
digit :: (Stream s, NFData s) => Parser'S s Char
digit = charParserOf isDigit <?> "digit"

-- | Parses any single hexadecimal number, @[0-9a-f]@
--
-- >>> t' (some hexDigit) "f8f8f8xyz"
-- Right "f8f8f8"
--
hexDigit :: (Stream s, NFData s) => Parser'S s Char
hexDigit = charParserOf isHexDigit <?> "hex-digit"

-- | Parses any single alphabetical character, @[a-zA-Z]@
--
-- >>> t' (some alpha) "stop COVID-19"
-- Right "stop"
--
alpha :: (Stream s, NFData s) => Parser'S s Char
alpha = charParserOf isAlpha <?> "letter"

-- | The same to @alpha@
--
-- >>> t' (some letter) "COVID-19"
-- Right "COVID"
--
letter :: (Stream s, NFData s) => Parser'S s Char
letter = alpha

-- | Parses any alphabetical or numeric character. @[0-9a-zA-Z]@
--
-- >>> t' (some alphaNum) "year2022"
-- Right "year2022"
--
alphaNum :: (Stream s, NFData s) => Parser'S s Char
alphaNum = charParserOf isAlphaNum <?> "letter-or-digit"

-- | Parses any single lowercase letter, @[a-z]@
--
-- >>> t' (some lower) "covID-19"
-- Right "cov"
--
lower :: (Stream s, NFData s) => Parser'S s Char
lower = charParserOf isLower <?> "lowercase-letter"

-- | Parses any single uppercase letter, @[A-Z]@
--
-- >>> t' (some upper) "COVID-19"
-- Right "COVID"
--
upper :: (Stream s, NFData s) => Parser'S s Char
upper = charParserOf isUpper <?> "uppercase-letter"

-- | Parses a single special character, anychar = alphaNum <|> special
--
-- >>> t' special "# stop COVID-19 -->"
-- Right '#'
--
special :: (Stream s, NFData s) => Parser'S s Char
special = charParserOf isSpecial <?> "special-character"
  where isSpecial c = or $ ($ c) <$> [isPunctuation, isSymbol]

-- | Parses tab character, \t
--
-- >>> t' (string "stop" >> tab) "stop\tCOVID-19"
-- Right '\t'
--
tab :: (Stream s, NFData s) => Parser'S s Char
tab = char '\t' <?> "tab"

-- | Parses LF or linefeed character, \n
--
-- >>> t' (string "stop" >> lf) "stop\nCOVID-19"
-- Right '\n'
--
lf :: (Stream s, NFData s) => Parser'S s Char
lf = char '\n' <?> "linefeed"

-- | Parses CRLF or carrige return with linefeed, \r\n
--
-- >>> t' (string "stop" >> crlf) "stop\r\nCOVID-19"
-- Right '\n'
--
crlf :: (Stream s, NFData s) => Parser'S s Char
crlf = (char '\r' *> char '\n') <?> "carriage-return + linefeed"

-- | Parses end-of-line character, @[LF | CRLF]@
--
-- >>> t' (string "stop" >> some eol) "stop\n\r\nCOVID-19"
-- Right "\n\n"
--
eol :: (Stream s, NFData s) => Parser'S s Char
eol = (lf <|> crlf) <?> "end-of-line"

-- | Parses a single whitespace character
--
-- >>> t' (some space) "  \n\tstop COVID-19"
-- Right "  \n\t"
--
space :: (Stream s, NFData s) => Parser'S s Char
space = charParserOf isSpace <?> "space"

-- | Checks if the `State` applied to the parser is reached to @EOF@ or /End-of-Stream/
--
-- >>> t' eof ""
-- Right '\NUL'
--
eof :: (Stream s, NFData s) => Parser'S s Char
eof = label "end-of-stream" $ do
  s <- assert anystring
  if null s
    then return '\NUL'
    else fail $ unwords ["EOF not found. Found char:", show . head $ s]

-- | Parses if a character on parsing is in the given char-list
--
-- >>> t' (some $ oneOf "francis") "ansi"
-- Right "ansi"
--
oneOf :: (Stream s, NFData s) => [Char] -> Parser'S s Char
oneOf cs = charParserOf (`elem` cs) <?> label'oneof
  where label'oneof = unwords ["one of", show ((: []) <$> cs)]

-- | Parses if a character on parsing is NOT in the given char-list
--
-- >>> t' (some $ noneOf "francis") "gold"
-- Right "gold"
--
noneOf :: (Stream s, NFData s) => [Char] -> Parser'S s Char
noneOf cs = charParserOf (`notElem` cs) <?> label'noneof
  where label'noneof = unwords ["none of", show ((: []) <$> cs)]

-- | Picks up one of prepared char-parsers by a string name
--
-- >>> t' (some $ selectp "special") "@${select} parsers by strings"
-- Right "@${"
--
selectp :: (Stream s, NFData s) => String -> Parser'S s Char
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
    | otherwise     -> error $ unwords ["not found parser such as: ", c]
