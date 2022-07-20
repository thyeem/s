module Text.S.Combinator
  ( module Text.S.Combinator
  , many
  , some
  , ($>)
  ) where

import           Control.Applicative            ( Alternative(..)
                                                , liftA2
                                                )
import           Control.Monad                  ( MonadPlus(..)
                                                , replicateM
                                                , replicateM_
                                                )
import           Data.Functor                   ( ($>) )
import           Text.S.Internal



-------------------------
-- parser combinators
-------------------------
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
-- >>> parseTest ( endBy (space <|> digit) (char ',' ) ) "[1,2,3,4]"
-- 111 1,2,3,4
--
endBy :: Stream s => Parser'S s a -> Parser'S s sep -> Parser'S s [a]
endBy p sep = many (p <* sep)

endBy1 :: Stream s => Parser'S s a -> Parser'S s sep -> Parser'S s [a]
endBy1 p sep = some (p <* sep)

-- | apply parser 0+ times until parser 'end' succeeds
--
-- >>> comment = (string "<!--") >> manyTill anychar (string "-->")
manyTill :: Stream s => Parser'S s a -> Parser'S s end -> Parser'S s [a]
manyTill p end = go where go = (end $> []) <|> liftA2 (:) p go

someTill :: Stream s => Parser'S s a -> Parser'S s end -> Parser'S s [a]
someTill p end = liftA2 (:) p (manyTill p end)

skipOptional :: Stream s => Parser'S s a -> Parser'S s ()
skipOptional p = () <$ p <|> pure ()

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
