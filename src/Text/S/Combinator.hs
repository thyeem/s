-----------------------------------------------------------------------------
-- |
-- Module      : Text.S.Combinator
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module defines some useful monadic parser combinators.
--
-- Put simply, combining "sets of parsers" creates a single new large parser
-- for more complex structures.
--
-----------------------------------------------------------------------------

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
import           Data.Foldable                  ( foldl' )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Text.S.Internal



-------------------------
-- parser combinators
-------------------------
-- | Tries to parse with parsers in the list untill one of them succeeds
--
-- >>> t' (choice [letter, special, digit]) "$parser"
-- Right '$'
--
choice :: MonadPlus m => [m a] -> m a
choice = foldl' (<|>) mzero

-- | Firstly tries to parse with parser @__p__@. If failed, it returns @__x__@.
--
-- This is useful to set default value of parser @__p__@.
--
-- >>> t' (option "Mars" spaces) "nuclear-bomb-explosion -> Earth"
-- Right "Mars"
--
option :: MonadPlus m => a -> m a -> m a
option x p = p <|> return x

-- | Tries to parse @__n__@-times with the given parser. The same as `replicateM`
--
-- >>> t' (count 6 letter) "parser-combinator"
-- Right "parser"
--
count :: MonadPlus m => Int -> m a -> m [a]
count = replicateM

-- | Tries to parse with parser @__p__@. If failed, it returns `Nothing`
-- otherwise, it returns `Just`-wrapped parser @__p__@
--
-- >>> t' (optionMaybe digits) "COVID-19"
-- Right Nothing
--
-- >>> t' (optionMaybe letters) "COVID-19"
-- Right (Just "COVID")
--
optionMaybe :: MonadPlus m => m a -> m (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

-- | Tries to parse with parser @__p__@, which is between the given two parsers,
-- /opener/ @__bra__@ and /closer/ @__ket__@.
--
-- >>> p = some $ digit <|> char ','
-- >>> t' (between (symbol "[") (symbol "]") p) "[1,2,3,4]"
-- Right "1,2,3,4"
--
between :: MonadPlus m => m bra -> m ket -> m a -> m a
between bra ket p = bra *> p <* ket

-- | Parses 0+ occurrences of parser @__p__@, separated by separator @__sep__@.
--
-- See also `sepBy1`.
--
-- >>> t' (sepBy decimals (symbol ",")) "1,2,3,4,5"
-- Right [1,2,3,4,5]
--
-- >>> t' (sepBy decimals (symbol ".")) "1,2,3,4,5"
-- Right []
--
sepBy :: MonadPlus m => m a -> m b -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parses 1+ occurrences of parser @__p__@, separated by separator @__sep__@
--
-- See the difference with `endBy1`
--
-- >>> t' (sepBy1 (anystringBut "a") (symbol "a")) "parser combinator"
-- Right ["p","rser combin","tor"]
--
sepBy1 :: MonadPlus m => m a -> m b -> m [a]
sepBy1 p sep = liftA2 (:) p (some (sep *> p))

-- | Parses 0+ occurrences of parser @__p__@, ended by separator @__sep__@.
--
-- See also `endBy1`.
--
-- >>> t' (endBy alphaNums (char ';')) "statement1;statement2;statement3;"
-- Right ["statement1","statement2","statement3"]
--
-- >>> t' (endBy alphaNums (char ':')) "statement1;statement2;statement3;"
-- Right []
--
endBy :: MonadPlus m => m a -> m b -> m [a]
endBy p end = many (p <* end)

-- | Parses 1+ occurrences of parser @__p__@, ended by separator @__sep__@
--
-- See the difference with `sepBy1`
--
-- >>> t' (endBy1 (anystringBut "a") (symbol "a")) "parser combinator"
-- Right ["p","rser combin"]
--
endBy1 :: MonadPlus m => m a -> m b -> m [a]
endBy1 p end = some (p <* end)


-- manyTill' :: MonadPlus m => m a -> m b -> m [a]
-- manyTill' p end = someTill' p end <|> (assert end $> [])

-- someTill' :: MonadPlus m => m a -> m b -> m [a]
-- someTill' p end = liftA2 (:) p (manyTill' p end)


-- | Applies parser @__p__@ 0+ times until parser @__end__@ succeeds
--
-- See also `someTill`.
--
-- >>> p = string "{-" *> manyTill anychar (string "-}")
-- >>> t' p "{- haskell block comment here -}"
-- Right " haskell block comment here "
--
-- >>> q = string "{-" *> manyTill special (string "-}")
-- >>> t' q "{--}"
-- Right ""
--
manyTill :: MonadPlus m => m a -> m b -> m [a]
manyTill p end = someTill p end <|> (end $> [])

-- | Applies parser @__p__@ 1+ times until parser @__end__@ succeeds
--
-- >>> p = someTill (letter <|> space) (string ":")
-- >>> t' p "for x in xs: f(x)"
-- Right "for x in xs"
--
someTill :: MonadPlus m => m a -> m b -> m [a]
someTill p end = liftA2 (:) p (manyTill p end)

-- | Tries to parse with parser @__p__@. If exists, consume it. Otherwise ignore it.
--
skipOptional :: MonadPlus m => m a -> m ()
skipOptional p = void p <|> pure ()

-- | Applies parser @__p__@ 0+ times, then skips the results.
--
skipMany :: MonadPlus m => m a -> m ()
skipMany = void . many

-- | Applies parser @__p__@ 1+ times, then skips the results.
--
skipSome :: MonadPlus m => m a -> m ()
skipSome p = p *> skipMany p

-- | Tries to parse @__n__@-times with the given parser.
--
-- The same as `count`, but this skips the results.
--
skipCount :: MonadPlus m => Int -> m a -> m ()
skipCount = replicateM_

-- |
skipManyTill :: MonadPlus m => m a -> m b -> m b
skipManyTill p end = go where go = end <|> (p *> go)

-- |
skipSomeTill :: MonadPlus m => m a -> m b -> m b
skipSomeTill p end = p *> skipManyTill p end

-- | Tries to parse @__p__@ with left-associative operator @__op__@ repeatedly.
--
-- This also tries to fold (evaluate) them by the given operator @__op__@.
--
-- See also `foldro`.
--
-- >>> op = symbol "^" *> pure (^)
-- >>> t' (foldlo op (strip integer)) "2 ^ 3 ^ 4"
-- Right 4096
--
-- >>> op = (symbol "+" *> pure (+)) <|> (symbol "-" *> pure (-))
-- >>> t' (foldlo op (strip integer)) "7 - 4 + 2"
-- Right 5
--
foldlo :: MonadPlus m => m (a -> a -> a) -> m a -> m a
foldlo op p = p >>= rest
  where rest x = (op >>= \f -> p >>= rest . f x) <|> pure x

-- | Tries to parse @__p__@ with right-associative operator @__op__@ repeatedly.
--
-- This also tries to fold (evaluate) them by the given operator @__op__@.
--
-- See also `foldlo`.
--
-- >>> op = symbol "^" *> pure (^)
-- >>> t' (foldro op (strip integer)) "2 ^ 3 ^ 4"
-- Right 2417851639229258349412352
--
-- >>> op = (symbol "+" *> pure (+)) <|> (symbol "-" *> pure (-))
-- >>> t' (foldro op (strip integer)) "7 - 4 + 2"
-- Right 1
--
foldro :: MonadPlus m => m (a -> a -> a) -> m a -> m a
foldro op p = p >>= rest
  where rest x = (op >>= (\f -> f x <$> (p >>= rest))) <|> pure x
