module Text.S.Combinator
  ( module Text.S.Combinator
  , many
  , some
  , (<|>)
  , ($>)
  ) where

import           Control.Applicative            ( Alternative(..)
                                                , liftA2
                                                )
import           Control.Monad                  ( MonadPlus(..)
                                                , replicateM
                                                , replicateM_
                                                )
import           Data.Char                      ( isAlpha
                                                , isAlphaNum
                                                , isDigit
                                                , isHexDigit
                                                , isLower
                                                , isSpace
                                                , isUpper
                                                )
import           Data.Functor                   ( ($>) )
import           Text.S.Internal


-------------------------
-- primitive parsers
-------------------------
char :: Stream s => Char -> Parser'S s Char
char c = charParserOf (== c) <?> show [c]

anyChar :: Stream s => Parser'S s Char
anyChar = charParserOf (const True)

anyCharBut :: Stream s => Char -> Parser'S s Char
anyCharBut c =
  charParserOf (/= c) <?> unwords ["any character except for", show c]

string :: Stream s => String -> Parser'S s String
string = mapM char

anyString :: Stream s => Parser'S s String
anyString = many anyChar

letter :: Stream s => Parser'S s Char
letter = charParserOf isAlpha <?> "letter"

digit :: Stream s => Parser'S s Char
digit = charParserOf isDigit <?> "digit"

hexDigit :: Stream s => Parser'S s Char
hexDigit = charParserOf isHexDigit <?> "hex-string"

alphaNum :: Stream s => Parser'S s Char
alphaNum = charParserOf isAlphaNum <?> "letter-or-digit"

lower :: Stream s => Parser'S s Char
lower = charParserOf isLower <?> "lowercase-letter"

upper :: Stream s => Parser'S s Char
upper = charParserOf isUpper <?> "uppercase-letter"

tab :: Stream s => Parser'S s Char
tab = char '\t' <?> "tab"

linefeed :: Stream s => Parser'S s Char
linefeed = char '\n' <?> "linefeed"

crlf :: Stream s => Parser'S s Char
crlf = (char '\r' >> char '\n') <?> "carrige-return + linefeed"

eol :: Stream s => Parser'S s Char
eol = (linefeed <|> crlf) <?> "end-of-line"

space :: Stream s => Parser'S s Char
space = charParserOf isSpace <?> "space"

spaces :: Stream s => Parser'S s String
spaces = many space <?> "whitespaces"

skipSpaces :: Stream s => Parser'S s ()
skipSpaces = skipMany space <?> "skip whitespaces"

oneOf :: Stream s => [Char] -> Parser'S s Char
oneOf cs = charParserOf (`elem` cs) <?> label'oneof
  where label'oneof = unwords ["one of", show ((: []) <$> cs)]

noneOf :: Stream s => [Char] -> Parser'S s Char
noneOf cs = charParserOf (`notElem` cs) <?> label'noneof
  where label'noneof = unwords ["none of", show ((: []) <$> cs)]


-------------------------
-- parser combinators
-------------------------

-- | attempt a parse without comsuming any input (looking ahead)
try :: Stream s => Parser'S s a -> Parser'S s a
try = id

choice :: Stream s => [Parser'S s a] -> Parser'S s a
choice = foldl (<|>) mzero

option :: Stream s => a -> Parser'S s a -> Parser'S s a
option x p = p <|> return x

count :: Stream s => Int -> Parser'S s a -> Parser'S s [a]
count = replicateM

optionMaybe :: Stream s => Parser'S s a -> Parser'S s (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

between
  :: Stream s
  => Parser'S s bra
  -> Parser'S s ket
  -> Parser'S s a
  -> Parser'S s a
between bra ket p = bra *> p <* ket

sepBy :: Stream s => Parser'S s a -> Parser'S s sep -> Parser'S s [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Stream s => Parser'S s a -> Parser'S s sep -> Parser'S s [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

-- | parses 0+ occurrences of parser, ended by sep
--
-- >> statements = statements `endBy` semi
endBy :: Stream s => Parser'S s a -> Parser'S s sep -> Parser'S s [a]
endBy p sep = many (p <* sep)

-- | apply parser 0+ times until parser 'end' succeeds
--
-- >> comment = (string "<!--") >> manyTill anyChar (string "-->")
manyTill :: Stream s => Parser'S s a -> Parser'S s end -> Parser'S s [a]
manyTill p end = go where go = (end $> []) <|> liftA2 (:) p go

someTill :: Stream s => Parser'S s a -> Parser'S s end -> Parser'S s [a]
someTill p end = liftA2 (:) p (manyTill p end)

skipOptional :: Stream s => Parser'S s a -> Parser'S s ()
skipOptional p = (() <$ p) <|> pure ()

skipMany :: Stream s => Parser'S s a -> Parser'S s ()
skipMany p = () <$ many p

skipSome :: Stream s => Parser'S s a -> Parser'S s ()
skipSome p = p *> skipMany p

skipCount :: Stream s => Int -> Parser'S s a -> Parser'S s ()
skipCount = replicateM_

skipManyTill :: Stream s => Parser'S s a -> Parser'S s end -> Parser'S s end
skipManyTill p end = go where go = end <|> (p *> go)

skipSomeTill :: Stream s => Parser'S s a -> Parser'S s end -> Parser'S s end
skipSomeTill p end = p *> skipManyTill p end
