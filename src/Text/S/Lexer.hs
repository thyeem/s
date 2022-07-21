module Text.S.Lexer
  ( module Text.S.Lexer
  ) where

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
                                                , toLower
                                                , toUpper
                                                )
import           Text.S.Internal                ( (<?>)
                                                , (<|>)
                                                , Parser'S
                                                , Stream
                                                , charParserOf
                                                )

import           Text.S.Combinator



-------------------------
-- primitive parsers
-------------------------
-- | Parses a given single character
--
-- >>> t' (char 'p') "parser"
-- Right 'p'
--
char :: Stream s => Char -> Parser'S s Char
char c = charParserOf (== c) <?> show [c]

-- | Parses any single character
--
-- >>> t' anychar "$parser"
-- Right '$'
--
anychar :: Stream s => Parser'S s Char
anychar = charParserOf (const True) <?> "all kinds of character"

-- | Parses every single character except for a given character
--
-- >>> t' (many $ anycharBut 's') "parser"
-- Right "par"
--
anycharBut :: Stream s => Char -> Parser'S s Char
anycharBut c =
  charParserOf (/= c) <?> unwords ["any character except for", show c]

-- | Parses a given string
-- >>> t' (string "par") "parser"
-- Right "par"
--
string :: Stream s => String -> Parser'S s String
string = mapM char

-- | Parses any string and consumes everything
-- >>> t' anystring "stop COVID-19"
-- Right "stop COVID-19"
--
anystring :: Stream s => Parser'S s String
anystring = many anychar

-- | Parses any single digit, [0-9]
-- >>> t' digit "3.1415926535"
-- Right '3'
--
digit :: Stream s => Parser'S s Char
digit = charParserOf isDigit <?> "digit"

-- | Parses any single hexadecimal number, [0-9a-f]
-- >>> t' (many hexDigit) "f8f8f8xyz"
-- Right "f8f8f8"
--
hexDigit :: Stream s => Parser'S s Char
hexDigit = charParserOf isHexDigit <?> "hex-string"

-- | Parses any single alphabetical character, [a-zA-Z]
-- >>> t' (many alpha) "stop COVID-19"
-- Right "stop"
--
alpha :: Stream s => Parser'S s Char
alpha = charParserOf isAlpha <?> "letter"

-- | The same to @alpha@
-- >>> t' (many letter) "COVID-19"
-- Right "COVID"
--
letter :: Stream s => Parser'S s Char
letter = alpha

-- | Parses any alphabetical or numeric character. [0-9a-zA-Z]
-- >>> t' (many alphaNum) "year2022"
-- Right "year2022"
--
alphaNum :: Stream s => Parser'S s Char
alphaNum = charParserOf isAlphaNum <?> "letter-or-digit"

-- | Parses any single lowercase letter, [a-z]
-- >>> t' (many lower) "covID-19"
-- Right "cov"
--
lower :: Stream s => Parser'S s Char
lower = charParserOf isLower <?> "lowercase-letter"

-- | Parses any single uppercase letter, [A-Z]
-- >>> t' (many upper) "COVID-19"
-- Right "COVID"
--
upper :: Stream s => Parser'S s Char
upper = charParserOf isUpper <?> "uppercase-letter"

-- | Parses a single special character, anychar = alphaNum <|> special
-- >>> t' special "# stop COVID-19 -->"
-- Right '#'
--
special :: Stream s => Parser'S s Char
special = charParserOf isSpecial <?> "special-character"
  where isSpecial c = or $ ($ c) <$> [isPunctuation, isSymbol]

tab :: Stream s => Parser'S s Char
tab = char '\t' <?> "tab"

lf :: Stream s => Parser'S s Char
lf = char '\n' <?> "linefeed"

crlf :: Stream s => Parser'S s Char
crlf = (char '\r' >> char '\n') <?> "carrige-return + linefeed"

eol :: Stream s => Parser'S s Char
eol = (lf <|> crlf) <?> "end-of-line"

space :: Stream s => Parser'S s Char
space = charParserOf isSpace <?> "space"

oneOf :: Stream s => [Char] -> Parser'S s Char
oneOf cs = charParserOf (`elem` cs) <?> label'oneof
  where label'oneof = unwords ["one of", show ((: []) <$> cs)]

noneOf :: Stream s => [Char] -> Parser'S s Char
noneOf cs = charParserOf (`notElem` cs) <?> label'noneof
  where label'noneof = unwords ["none of", show ((: []) <$> cs)]



-------------------------
-- token parsers: lexer
-------------------------
-- | generate a lexical parser for the given parser @p@
token
  :: Stream s
  => Parser'S s String  -- parser for line comment mark
  -> Parser'S s String  -- parser for block comment opener
  -> Parser'S s String  -- parser for block comment closure
  -> Parser'S s a       -- parser for target lexeme
  -> Parser'S s a
token s bra ket p = p <* jump (lineComment s) (blockComment bra ket)

-- |
-- symbol :: Stream s => String -> Parser'S s String
-- symbol name = token (string name)


-- | skipping unnecesary part including whitespaces and comments
-- >>> :{
--   unwrap' $ t
--     (jump (lineComment $ string "#")
--           (blockComment (string "/*")
--           (string "*/")))
--     "  ## line-comment starts\n \r\n  /*inner block*/  end-of-jump"
-- :}
-- "end-of-jump"
--
jump :: Stream s => Parser'S s a -> Parser'S s a -> Parser'S s ()
jump lc bc = skipMany $ skipSome space <|> skipSome lc <|> skipSome bc

lineComment :: Stream s => Parser'S s String -> Parser'S s String
lineComment p = p *> manyTill anychar eol

blockComment
  :: Stream s => Parser'S s String -> Parser'S s String -> Parser'S s String
blockComment bra ket = bra *> manyTill anychar ket

caseGuard :: Stream s => Bool -> String -> Parser'S s String
caseGuard sensitive s | sensitive = string s
                      | otherwise = mapM char' s
 where
  char' c | isAlpha c = char (toLower c) <|> char (toUpper c)
          | otherwise = char c

identifier
  :: Stream s => Parser'S s Char -> Parser'S s String -> Parser'S s String
identifier start body = (:) <$> start <*> body
