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
import           Data.Functor                   ( ($>) )
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

-- | Firstly tries to parse with parser @p@. If failed, it returns @x@.
--
-- This is useful to set default value of parser @p@.
--
-- >>> t' (option "Mars" spaces) "nuclear-bomb-explosion -> Earth"
-- Right "Mars"
--
option :: MonadPlus m => a -> m a -> m a
option x p = p <|> return x

-- | Tries to parse @n@-times with the given parser. The same as `replicateM`
--
-- >>> t' (count 6 letter) "parser-combinator"
-- Right "parser"
--
count :: MonadPlus m => Int -> m a -> m [a]
count = replicateM

-- | Tries to parse with parser @p@. If failed, it returns `Nothing`
-- otherwise, it returns `Just`-wrapped parser @p@
--
-- >>> t' (optionMaybe digits) "COVID-19"
-- Right Nothing
--
-- >>> t' (optionMaybe letters) "COVID-19"
-- Right (Just "COVID")
--
optionMaybe :: MonadPlus m => m a -> m (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

-- | Tries to parse with parser @p@, which is between the given two parsers,
-- /opener/ @bra@ and /closer/ @ket@.
--
-- >>> p = some $ digit <|> char ','
-- >>> t' (between (token "[") (token "]") p) "[1,2,3,4]"
-- Right "1,2,3,4"
--
between :: MonadPlus m => m bra -> m ket -> m a -> m a
between bra ket p = bra *> p <* ket

-- | Parses 0+ occurrences of parser @p@, separated by separator @sep@.
--
-- See also `sepBy1`.
--
-- >>> t' (sepBy decimals (token ",")) "1,2,3,4,5"
-- Right [1,2,3,4,5]
--
-- >>> t' (sepBy decimals (token ".")) "1,2,3,4,5"
-- Right []
--
sepBy :: MonadPlus m => m a -> m b -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parses 1+ occurrences of parser @p@, separated by separator @sep@
--
-- See the difference with `endBy1`
--
-- >>> t' (sepBy1 (some $ anycharBut 'a') (token "a")) "parser combinator"
-- Right ["p","rser combin","tor"]
--
sepBy1 :: MonadPlus m => m a -> m b -> m [a]
sepBy1 p sep = liftA2 (:) p (some (sep *> p))

-- | Parses 0+ occurrences of parser @p@, ended by separator @sep@.
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
endBy p sep = many (p <* sep)

-- | Parses 1+ occurrences of parser @p@, ended by separator @sep@
--
-- See the difference with `sepBy1`
--
-- >>> t' (endBy1 (some $ anycharBut 'a') (token "a")) "parser combinator"
-- Right ["p","rser combin"]
--
endBy1 :: MonadPlus m => m a -> m b -> m [a]
endBy1 p sep = some (p <* sep)

-- | Apply parser @p@ 0+ times until parser @end@ succeeds
--
-- >>> :{
--   t' (string "{-" >> manyTill anychar (string "-}"))
--      "{- haskell block comment here -}"
-- :}
-- Right " haskell block comment here "
--
manyTill :: MonadPlus m => m a -> m b -> m [a]
manyTill p end = go where go = (end $> []) <|> liftA2 (:) p go

-- |
someTill :: MonadPlus m => m a -> m b -> m [a]
someTill p end = liftA2 (:) p (manyTill p end)

-- |
skipOptional :: MonadPlus m => m a -> m ()
skipOptional p = () <$ p <|> pure ()

-- |
skipMany :: MonadPlus m => m a -> m ()
skipMany p = () <$ many p

-- |
skipSome :: MonadPlus m => m a -> m ()
skipSome p = p *> skipMany p

-- |
skipCount :: MonadPlus m => Int -> m a -> m ()
skipCount = replicateM_

-- |
skipManyTill :: MonadPlus m => m a -> m b -> m b
skipManyTill p end = go where go = end <|> (p *> go)

-- |
skipSomeTill :: MonadPlus m => m a -> m b -> m b
skipSomeTill p end = p *> skipManyTill p end

-- | Parses 0+ occurrences of @p@
-- chainl p op x = chainl1 p op <|> return x
chainl :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainl p op x = option x (chainl1 p op)

-- |
chainl1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainl1 p op = p >>= go
  where go x = (op >>= (\f -> p >>= go . f x)) <|> pure x

-- | Parses 0+ occurrences of @p@
-- chainr p op x = chainlr p op <|> return x
chainr :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainr p op x = option x (chainr1 p op)

-- |
chainr1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainr1 p op = g
 where
  g = p >>= go
  go x = op >>= (\f -> f x <$> g)
