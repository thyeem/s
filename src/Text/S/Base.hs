-- |
-- Module      : Text.S.Combinator
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module defines a primitive char/string parser to be used for
-- extensions such as lexeme parsers and more complex structured parsers.
module Text.S.Base
  ( module Text.S.Base
  )
where

import Data.Char
  ( isPunctuation
  , isSymbol
  )
import Data.Functor (($>))
import Text.S.Combinator
import Text.S.Internal

-- | Parses a given single character
--
-- >>> ta (char 'p') "parser"
-- 'p'
char :: Stream s => Char -> S s Char
char c = charBy (== c) <?> show [c]
{-# INLINE char #-}

-- | Parses any single character
--
-- >>> ta anychar "$parser"
-- '$'
anychar :: Stream s => S s Char
anychar = charBy (const True) <?> "any character"
{-# INLINE anychar #-}

-- | Parses every single character except for a given character
--
-- >>> ta (some $ anycharBut 's') "parser"
-- "par"
anycharBut :: Stream s => Char -> S s Char
anycharBut c =
  charBy (/= c) <?> unwords ["any character except for", show c]
{-# INLINE anycharBut #-}

-- | Parses a given string
--
-- >>> ta (string "par") "parser"
-- "par"
string :: Stream s => String -> S s String
string s = mapM char s <?> show s
{-# INLINE string #-}

-- | Parses any string and consumes everything
--
-- >>> ta anystring "stop COVID-19"
-- "stop COVID-19"
anystring :: Stream s => S s String
anystring = some anychar <?> "any string"
{-# INLINE anystring #-}

-- | Parses any string except for a given string.
--
-- >>> ta (anystringBut "ID") "stop COVID-19"
-- "stop COV"
anystringBut :: Stream s => String -> S s String
anystringBut s = go
 where
  go = choice [try (string s) $> [], try eof $> [], liftA2 (:) anychar go]
{-# INLINE anystringBut #-}

-- | Parses any single digit, the same as @__[0-9]__@
--
-- >>> ta digit "3.1415926535"
-- '3'
digit :: Stream s => S s Char
digit = charBy isDigit <?> "digit"
{-# INLINE digit #-}

-- | Parses any single hexadecimal number, the same as @__[0-9a-f]__@
--
-- >>> ta (some hexDigit) "f8f8f8xyz"
-- "f8f8f8"
hexDigit :: Stream s => S s Char
hexDigit = charBy isHexDigit <?> "hex-digit"
{-# INLINE hexDigit #-}

-- | Parses any single alphabetical character, the same as @__[a-zA-Z]__@
--
-- >>> ta (some alpha) "stop COVID-19"
-- "stop"
alpha :: Stream s => S s Char
alpha = charBy isAlpha <?> "letter"
{-# INLINE alpha #-}

-- | The same as @__alpha__@
--
-- >>> ta (some letter) "COVID-19"
-- "COVID"
letter :: Stream s => S s Char
letter = alpha
{-# INLINE letter #-}

-- | Parses any alphabetical or numeric character, the same as @__[0-9a-zA-Z]__@
--
-- >>> ta (some alphaNum) "year2022"
-- "year2022"
alphaNum :: Stream s => S s Char
alphaNum = charBy isAlphaNum <?> "letter-or-digit"
{-# INLINE alphaNum #-}

-- | Parses any single lowercase letter, the same as @__[a-z]__@
--
-- >>> ta (some lower) "covID-19"
-- "cov"
lower :: Stream s => S s Char
lower = charBy isLower <?> "lowercase-letter"
{-# INLINE lower #-}

-- | Parses any single uppercase letter, the same as @__[A-Z]__@
--
-- >>> ta (some upper) "COVID-19"
-- "COVID"
upper :: Stream s => S s Char
upper = charBy isUpper <?> "uppercase-letter"
{-# INLINE upper #-}

-- | Parses a single special character, @__anychar := alphaNum <|> special__@
--
-- >>> ta special "# stop COVID-19 -->"
-- '#'
special :: Stream s => S s Char
special =
  charBy isPunctuation <|> charBy isSymbol <?> "special-character"
{-# INLINE special #-}

-- | Parses tab character, \t
--
-- >>> ta (string "stop" >> tab) "stop\tCOVID-19"
-- '\t'
tab :: Stream s => S s Char
tab = char '\t' <?> "tab"
{-# INLINE tab #-}

-- | Parses LF or linefeed character, \n
--
-- >>> ta (string "stop" >> lf) "stop\nCOVID-19"
-- '\n'
lf :: Stream s => S s Char
lf = char '\n' <?> "linefeed"
{-# INLINE lf #-}

-- | Parses CRLF or carrige return with linefeed, \r\n
--
-- >>> ta (string "stop" >> crlf) "stop\r\nCOVID-19"
-- '\n'
crlf :: Stream s => S s Char
crlf = (char '\r' *> char '\n') <?> "carriage-return + linefeed"
{-# INLINE crlf #-}

-- | Parses end-of-line character, the same as @__[LF | CRLF]__@
--
-- >>> ta (string "stop" >> some eol) "stop\n\r\nCOVID-19"
-- "\n\n"
eol :: Stream s => S s Char
eol = (lf <|> crlf) <?> "end-of-line"
{-# INLINE eol #-}

-- | Parses a single empty character
--
-- >>> ta (some space) "  \n\tstop COVID-19"
-- "  \n\t"
blank :: Stream s => S s Char
blank = char ' ' <?> "blank"
{-# INLINE blank #-}

-- | Parses a single whitespace character
--
-- >>> ta (some space) "  \n\tstop COVID-19"
-- "  \n\t"
space :: Stream s => S s Char
space = charBy isSpace <?> "space"
{-# INLINE space #-}

-- | Checks if the `State` applied to the parser is reached to
-- @__EOF__@ or /End-of-Stream/
eof :: Stream s => S s ()
eof = forbid anychar <?> "end-of-stream"
{-# INLINE eof #-}

-- | Parses if a character on parsing is in the given char-list
--
-- >>> ta (some $ oneOf "francis") "ascii-character-table"
-- "ascii"
oneOf :: Stream s => [Char] -> S s Char
oneOf cs = charBy (`elem` cs) <?> label'oneof
 where
  label'oneof = unwords ["one of", show ((: []) <$> cs)]
{-# INLINE oneOf #-}

-- | Parses if a character on parsing is NOT in the given char-list
--
-- >>> ta (some $ noneOf "francis") "goldberg-variation"
-- "goldbe"
noneOf :: Stream s => [Char] -> S s Char
noneOf cs = charBy (`notElem` cs) <?> label'noneof
 where
  label'noneof = unwords ["none of", show ((: []) <$> cs)]
{-# INLINE noneOf #-}

-- | Check if a given char is one of whitespaces
--
-- This predicate gives correct answers for the ASCII encoding only.
isSpace :: Char -> Bool
isSpace c = (' ' == c) || ('\t' <= c && c <= '\r')
{-# INLINE isSpace #-}

-- | Check if a given char is one of decimal digits.
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
{-# INLINE isDigit #-}

-- | Check if a given char is one of hexadecimals
isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F'
{-# INLINE isHexDigit #-}

-- | Check if a given char is an ASCII letter.
isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c
{-# INLINE isAlpha #-}

-- | Check if a given char is an ASCII letter or a decimal digit.
isAlphaNum :: Char -> Bool
isAlphaNum c = isDigit c || isAlpha c
{-# INLINE isAlphaNum #-}

-- | Check if a given char is an ASCII upper-case letter.
isUpper :: Char -> Bool
isUpper c = c <= 'Z' && c >= 'A'
{-# INLINE isUpper #-}

-- | Check if a given char is an ASCII lower-case letter.
isLower :: Char -> Bool
isLower c = c <= 'z' && c >= 'a'
{-# INLINE isLower #-}
