module Text.S.Combinator where

import           Control.Applicative            ( Alternative(..) )
import           Control.Monad                  ( MonadPlus(..) )
import           Data.Char                      ( isAlpha
                                                , isAlphaNum
                                                , isDigit
                                                , isHexDigit
                                                , isLower
                                                , isSpace
                                                , isUpper
                                                )
import           Text.S.Internal


-------------------------
-- primitive parser
-------------------------

char :: Stream s => Char -> Parser'S s Char
char c = charParserOf (== c) <?> show [c]

anyChar :: Stream s => Parser'S s Char
anyChar = charParserOf (const True)

anyCharBut :: Stream s => Char -> Parser'S s Char
anyCharBut c = charParserOf (/= c)

-- string :: Stream s => String -> Parser'S s a
-- string []       = undefined

digit :: Stream s => Parser'S s Char
digit = charParserOf isDigit <?> "digit"

letter :: Stream s => Parser'S s Char
letter = charParserOf isAlpha <?> "letter"

alphaNum :: Stream s => Parser'S s Char
alphaNum = charParserOf isAlphaNum <?> "letter-or-digit"

hex :: Stream s => Parser'S s Char
hex = charParserOf isHexDigit <?> "hex-string"

lower :: Stream s => Parser'S s Char
lower = charParserOf isLower <?> "lowercase-letter"

upper :: Stream s => Parser'S s Char
upper = charParserOf isUpper <?> "uppercase-letter"

tab :: Stream s => Parser'S s Char
tab = char '\t' <?> "tab"

lf :: Stream s => Parser'S s Char
lf = char '\n' <?> "linefeed"

crlf :: Stream s => Parser'S s Char
crlf = char '\r' >> char '\n' <?> "carrige-return + linefeed"

eol :: Stream s => Parser'S s Char
eol = lf <|> crlf <?> "end-of-line"

space :: Stream s => Parser'S s Char
space = charParserOf isSpace <?> "space"

-- spaces

oneOf :: Stream s => [Char] -> Parser'S s Char
oneOf cs = charParserOf (`elem` cs) <?> label'oneof
  where label'oneof = unwords ["one of", show ((: []) <$> cs)]

noneOf :: Stream s => [Char] -> Parser'S s Char
noneOf cs = charParserOf (`notElem` cs) <?> label'noneof
  where label'noneof = unwords ["none of", show ((: []) <$> cs)]

-- many :: Parser'S s a -> Parser'S s [a]
-- many parser = undefined

-------------------------
-- parser combinators
-------------------------

choice :: Stream s => [Parser'S s a] -> Parser'S s a
choice = foldl (<|>) mzero

fallback :: Stream s => a -> Parser'S s a -> Parser'S s a
fallback x parser = parser <|> return x

optional :: Stream s => Parser'S s a -> Parser'S s ()
optional parser =
  do
    _ <- parser
    return ()
  <|> return ()
