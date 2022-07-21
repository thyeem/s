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
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                )
import           Text.S.Internal



-------------------------
-- parser combinators
-------------------------
-- | Try to parse with parsers in the list untill one of them succeeds
--
-- >>> t' (choice [letter, special, digit]) "$parser"
-- Right '$'
--
choice :: MonadPlus m => [m a] -> m a
choice = foldl (<|>) mzero

option :: MonadPlus m => a -> m a -> m a
option x p = p <|> return x

count :: MonadPlus m => Int -> m a -> m [a]
count = replicateM

optionMaybe :: MonadPlus m => m a -> m (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

between :: MonadPlus m => m b -> m b -> m a -> m a
between bra ket p = bra *> p <* ket

sepBy :: MonadPlus m => m a -> m b -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: MonadPlus m => m a -> m b -> m [a]
sepBy1 p sep = liftA2 (:) p (some (sep *> p))

-- | Parses 0+ occurrences of parser @p@, ended by separator @sep@
--
-- >>> :{
--   t' (endBy (many alphaNum) (char ';'))
--      "statement1;statement2;statement3;"
-- :}
-- Right ["statement1","statement2","statement3"]
--
endBy :: MonadPlus m => m a -> m b -> m [a]
endBy p sep = many (p <* sep)

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

someTill :: MonadPlus m => m a -> m b -> m [a]
someTill p end = liftA2 (:) p (manyTill p end)

skipOptional :: MonadPlus m => m a -> m ()
skipOptional p = () <$ p <|> pure ()

skipMany :: MonadPlus m => m a -> m ()
skipMany p = () <$ many p

skipSome :: MonadPlus m => m a -> m ()
skipSome p = p *> skipMany p

skipCount :: MonadPlus m => Int -> m a -> m ()
skipCount = replicateM_

skipManyTill :: MonadPlus m => m a -> m b -> m b
skipManyTill p end = go where go = end <|> (p *> go)

skipSomeTill :: MonadPlus m => m a -> m b -> m b
skipSomeTill p end = p *> skipManyTill p end

-- | Parses 0+ occurrences of @p@
chainl :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
-- chainl p op x = chainl1 p op <|> return x
chainl p op x = option x (chainl1 p op)

chainl1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainl1 p op = p >>= go
  where go x = (op >>= (\f -> p >>= go . f x)) <|> pure x

-- | Parses 0+ occurrences of @p@
chainr :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
-- chainr p op x = chainlr p op <|> return x
chainr p op x = option x (chainr1 p op)

chainr1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainr1 p op = g
 where
  g = p >>= go
  go x = op >>= (\f -> g <&> f x)
