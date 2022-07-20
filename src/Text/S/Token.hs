module Text.S.Token
  ( module Text.S.Token
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
-- | Parses a single character
--
-- >> dot = char '.'
--
char :: Stream s => Char -> Parser'S s Char
char c = charParserOf (== c) <?> show [c]

anychar :: Stream s => Parser'S s Char
anychar = charParserOf (const True) <?> "all kinds of character"

anycharBut :: Stream s => Char -> Parser'S s Char
anycharBut c =
  charParserOf (/= c) <?> unwords ["any character except for", show c]

string :: Stream s => String -> Parser'S s String
string = mapM char

anystring :: Stream s => Parser'S s String
anystring = many anychar

digit :: Stream s => Parser'S s Char
digit = charParserOf isDigit <?> "digit"

hexDigit :: Stream s => Parser'S s Char
hexDigit = charParserOf isHexDigit <?> "hex-string"

alpha :: Stream s => Parser'S s Char
alpha = charParserOf isAlpha <?> "letter"

letter :: Stream s => Parser'S s Char
letter = alpha

alphaNum :: Stream s => Parser'S s Char
alphaNum = charParserOf isAlphaNum <?> "letter-or-digit"

lower :: Stream s => Parser'S s Char
lower = charParserOf isLower <?> "lowercase-letter"

upper :: Stream s => Parser'S s Char
upper = charParserOf isUpper <?> "uppercase-letter"

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
token :: Stream s => Parser'S s a -> Parser'S s a
token p = p <* many space

-- reserved :: Stream s => String -> Parser'S s String
-- reserved s = token (string s)

symbol :: Stream s => String -> Parser'S s String
symbol name = token (string name)

-- | skipping unnecesary part including whitespaces and comments
jump :: Stream s => Parser'S s a -> Parser'S s a -> Parser'S s ()
jump lc bc = skipMany $ skipSome space <|> skipSome lc <|> skipSome bc

lineComment :: Stream s => Parser'S s String -> Parser'S s String
lineComment p = p *> manyTill anychar eol

blockComment
  :: Stream s => Parser'S s String -> Parser'S s String -> Parser'S s String
blockComment bra ket = bra *> manyTill anychar ket

-- caseGuard :: Stream s => Bool -> Parser'S s String -> Parser'S s String
-- caseGuard sensitive p | sensitive = p
                      -- | otherwise = many caseChar
 -- where
  -- caseChar c | isAlpha c = char (toLower c) <|> char (toUpper c)
             -- | otherwise = char c

identifier
  :: Stream s => Parser'S s Char -> Parser'S s String -> Parser'S s String
identifier start body = (:) <$> start <*> body



jump' :: Parser'S String ()
jump' =
  jump (lineComment $ string "#") (blockComment (string "/*") (string "*/"))

s =
  "    ## line coment start blah \n    /*\n\n\n\n Here is inner block*/francis"
