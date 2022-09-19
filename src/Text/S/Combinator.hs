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
  ) where

import           Control.Applicative            ( (<**>)
                                                , liftA2
                                                )
import           Control.Monad                  ( MonadPlus(..)
                                                , replicateM
                                                , replicateM_
                                                )
import           Data.Bifunctor                 ( first )
import           Data.List                      ( foldl' )
import           Text.S.Internal



-------------------------
-- parser combinators
-------------------------
-- |
some :: MonadPlus m => m a -> m [a]
some p = liftA2 (:) p (many p)
{-# INLINE some #-}

-- |
many :: MonadPlus m => m a -> m [a]
many p = some p <|> pure []
{-# INLINE many #-}

-- | Tries to parse with parsers in the list untill one of them succeeds.
--
-- >>> t' (choice [letter, special, digit]) "$parser"
-- '$'
--
choice :: MonadPlus m => [m a] -> m a
choice = foldl' (<|>) mzero
{-# INLINE choice #-}

-- | Firstly tries to parse with parser @__p__@.
--
-- If failed, it returns @__x__@. This is useful to set default value of parser @__p__@.
--
-- >>> t' (option "Mars" spaces) "nuclear-bomb-explosion -> Earth"
-- "Mars"
--
option :: MonadPlus m => a -> m a -> m a
option x p = p <|> return x
{-# INLINE option #-}

-- | Tries to parse @__n-times__@ with the given parser. The same as 'replicateM'.
--
-- See also 'skipCount'
--
-- >>> t' (count 6 letter) "Parser-Combinator"
-- "Parser"
--
count :: MonadPlus m => Int -> m a -> m [a]
count = replicateM
{-# INLINE count #-}

-- | Tries to parse with parser @__p__@. If failed, it returns 'Nothing'.
-- Otherwise, it returns the result of parser @__p__@ wrapped by 'Just'.
--
-- >>> t' (optionMaybe digits) "COVID-19"
-- Nothing
--
-- >>> t' (optionMaybe letters) "COVID-19"
-- Just "COVID"
--
optionMaybe :: MonadPlus m => m a -> m (Maybe a)
optionMaybe p = option Nothing (Just <$> p)
{-# INLINE optionMaybe #-}

-- | Tries to parse with parser @__p__@, which is between the given two parsers,
-- an opener @__bra__@ and a closer @__ket__@.
--
-- This consumes the result of parser @__bra__@ and @__ket__@ from input stream.
--
-- >>> p = some $ digit <|> char ','
-- >>> t' (between (symbol "[") (symbol "]") p) "[1,2,3,4]"
-- "1,2,3,4"
--
between :: MonadPlus m => m bra -> m ket -> m a -> m a
between bra ket p = bra *> p <* ket
{-# INLINE between #-}

-- | Parses @__0+(zero or more)__@ occurrences of parser @__p__@,
-- which is separated by separator @__sep__@.
--
-- This consumes the result of separator parser @__sep__@ from input stream.
--
-- See also 'endBy'.
--
-- >>> t' (sepBy (symbol ",") decimals) "1,2,3,4,5"
-- [1,2,3,4,5]
--
-- >>> t' (sepBy (symbol ".") decimals) "1,2,3,4,5"
-- [1]
--
sepBy :: MonadPlus m => m sep -> m a -> m [a]
sepBy sep p = sepBy1 sep p <|> pure []
{-# INLINE sepBy #-}

-- | Parses @__1+(one or more)__@ occurrences of parser @__p__@,
-- which is separated by separator @__sep__@.
--
-- This consumes the result of separator parser @__sep__@ from input stream.
--
-- See also 'endBy1'
--
-- >>> t' (sepBy1 (symbol "a") (anystringBut "a")) "parser combinator"
-- ["p","rser combin","tor"]
--
sepBy1 :: MonadPlus m => m sep -> m a -> m [a]
sepBy1 sep p = liftA2 (:) p (many (sep *> p))
{-# INLINE sepBy1 #-}

-- | Parses @__0+(zero or more)__@ occurrences of parser @__p__@,
-- which is ended by parser @__end__@.
--
-- This consumes the result of parser @__end__@ from input stream.
--
-- See also 'sepBy'.
--
-- >>> p = some $ alphaNum <|> char '=' <|> space
-- >>> t' (endBy (char ';') p) "int a=1;int b=2;"
-- ["int a=1","int b=2"]
--
-- >>> t' (endBy (char ';') digits) "10:20:30:"
-- []
--
endBy :: MonadPlus m => m end -> m a -> m [a]
endBy end p = many (p <* end)
{-# INLINE endBy #-}

-- | Parses @__1+(one or more)__@ occurrences of parser @__p__@,
-- which is ended by parser @__end__@.
--
-- This consumes the result of parser @__end__@ from input stream.
--
-- See also 'sepBy1'
--
-- >>> t' (endBy1 (symbol "a") (anystringBut "a")) "parser combinator"
-- ["p","rser combin"]
--
endBy1 :: MonadPlus m => m end -> m a -> m [a]
endBy1 end p = some (p <* end)
{-# INLINE endBy1 #-}

-- | Tries to parse @__0+(zero or more)-times__@ with parser @__p__@
-- until parser @__end__@ succeeds.
--
-- This consumes the result of parser @__end__@ from input stream.
--
-- See also 'manyTill''. It keeps the result of parser @__end__@.
--
-- >>> p = string "{-" *> manyTill (string "-}") anychar
-- >>> t' p "{- haskell block comment here -}"
-- " haskell block comment here "
--
-- >>> q = string "{-" *> manyTill (string "-}") special
-- >>> t' q "{--}"
-- ""
--
manyTill :: MonadPlus m => m end -> m a -> m [a]
manyTill end p = someTill end p <|> (end $> [])
{-# INLINE manyTill #-}

-- | Tries to parse @__1+(one or more)-times__@ with parser @__p__@
-- until parser @__end__@ succeeds.
--
-- See also 'someTill''. It keeps the result of parser @__end__@.
--
-- >>> p = someTill (string ":") (letter <|> space)
-- >>> t' p "for x in xs: f(x)"
-- "for x in xs"
--
someTill :: MonadPlus m => m end -> m a -> m [a]
someTill end p = liftA2 (:) p (manyTill end p)
{-# INLINE someTill #-}

-- | Tries to parse @__0+(zero or more)-times__@ with parser @__p__@
-- until parser @__end__@ succeeds.
--
-- This looks alike a lot 'manyTill',
-- but keeps the result of @__end__@ as tuple-element.
--
-- Use this when you need the result of @__end__@ as well.
--
-- See also 'manyTill'
--
-- >>> p = alphaNum <|> space
-- >>> t' (manyTill' special p) "stop COVID-19"
-- ("stop COVID",'-')
--
-- >>> t' (manyTill' letters digit) "stop COVID-19"
-- ("","stop")
--
manyTill' :: MonadPlus m => m end -> m a -> m ([a], end)
manyTill' end p = someTill' end p <|> (([], ) <$> end)
{-# INLINE manyTill' #-}

-- | Tries to parse @__1+(one or more)-times__@ with parser @__p__@
-- until parser @__end__@ succeeds.
--
-- This looks alike a lot 'manyTill',
-- but keeps the result of @__end__@ as tuple-element.
--
-- Use this when you need the result of @__end__@ as well.
--
-- See also 'someTill'
--
-- >>> stopCodon = symbol "UAA"
-- >>> geneticSequence = "AUCUCGUCAUCUCGUUAACUCGUA"
-- >>> t' (someTill' stopCodon upper) geneticSequence
-- ("AUCUCGUCAUCUCGU","UAA")
--
someTill' :: MonadPlus m => m end -> m a -> m ([a], end)
someTill' end p = liftA2 f p (manyTill' end p) where f a b = first (a :) b
{-# INLINE someTill' #-}

-- | Tries to parse with parser @__p__@.
-- If succeeds, then consume the result and throws it away. Otherwise ignore it.
--
-- >>> s' (skipOptional special) "$PARSER_COMBINATOR"
-- "PARSER_COMBINATOR"
--
-- >>> s' (skipOptional letter) "$PARSER_COMBINATOR"
-- "$PARSER_COMBINATOR"
--
skipOptional :: MonadPlus m => m a -> m ()
skipOptional p = void p <|> pure ()
{-# INLINE skipOptional #-}

-- | Tries to parse @__0+(zero or more)-times__@ with parser @__p__@,
-- then discards the result.
--
-- >>> s' (skipMany (anycharBut '#')) "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
-- "#-G-Ab-A-Bb-B"
--
-- >>> s' (skipMany digit) "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
-- "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
--
skipMany :: MonadPlus m => m a -> m ()
skipMany = void . many
{-# INLINE skipMany #-}

-- | Tries to parse @__1+(one or more)-times__@ with parser @__p__@,
-- then discards the result.
--
-- >>> s' (skipSome (letter <|> char '-')) "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
-- "#-G-Ab-A-Bb-B"
--
skipSome :: MonadPlus m => m a -> m ()
skipSome p = p *> skipMany p
{-# INLINE skipSome #-}

-- | Tries to parse @__n-times__@ with the given parser.
--
-- The same as 'count', but this discards the result.
-- This is equivalent to 'replicateM_'.
--
-- See also 'count'
--
-- >>> s' (skipCount 5 (digit *> char ':')) "1:2:3:4:5:6:7:8"
-- "6:7:8"
--
skipCount :: MonadPlus m => Int -> m a -> m ()
skipCount = replicateM_
{-# INLINE skipCount #-}

-- | Tries to parse @__0+(zero or more)-times__@ with parser @__p__@,
-- until parser @__end__@ succeeds.
--
-- That is, it okay for parser @__p__@ to fail as long as @__end__@ succeeds.
--
-- The result of parser @__end__@ is returned
-- while the result of parser @__p__@ is discarded.
--
-- See also 'skipSomeTill'.
--
-- >>> stopCodon = symbol "UAA"
-- >>> geneticSequence = "AUCUCGUCAUCUCGUUAACUCGUA"
-- >>> t' (skipManyTill stopCodon upper) geneticSequence
-- "UAA"
--
-- >>> s' (skipManyTill stopCodon upper) geneticSequence
-- "CUCGUA"
--
-- >>> s' (skipManyTill stopCodon upper) "UAACUCGUA"
-- "CUCGUA"
--
skipManyTill :: MonadPlus m => m end -> m a -> m end
skipManyTill end p = go where go = end <|> (p *> go)
{-# INLINE skipManyTill #-}

-- | Tries to parse @__1+(one or more)-times__@ with parser @__p__@,
-- until parser @__end__@ succeeds.
--
-- The result of parser @__end__@ is returned
-- while the result of parser @__p__@ is discarded.
--
-- See also 'skipManyTill'.
--
-- >>> t' (skipSomeTill (char '#') anychar) "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
-- '#'
--
-- >>> s' (skipSomeTill (char '#') anychar) "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
-- "-G-Ab-A-Bb-B"
--
skipSomeTill :: MonadPlus m => m end -> m a -> m end
skipSomeTill end p = p *> skipManyTill end p
{-# INLINE skipSomeTill #-}

-- | Tries to repeatedly parse two @__p__@ operands
-- with @__infix left-associative__@ binary operator @__op__@.
--
-- In other words, this can parse any expression consisting of
-- binary operator that is between its operands and is left-associative.
-- The result will be folded if the operation is evaluable.
--
-- See also 'chainl' and 'chainr1'.
--
-- >>> op = symbol "^" $> (^)
-- >>> t' (chainl1 op (strip integer)) "2 ^ 3 ^ 4"
-- 4096
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- 'Text.S.Expr.powOp''.
--
-- >>> op = (symbol "+" $> (+)) <|> (symbol "-" $> (-))
-- >>> t' (chainl1 op (strip integer)) "7 - 4 + 2"
-- 5
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- @'Text.S.Expr.addOp' '<|>' 'Text.S.Expr.subOp'@.
--
chainl1 :: MonadPlus m => m (a -> a -> a) -> m a -> m a
chainl1 op p = p >>= chainl op p
{-# INLINE chainl1 #-}

-- |
chainl :: MonadPlus m => m (a -> a -> a) -> m a -> a -> m a
chainl op p = rest
 where
  rest x = bind x <|> pure x
  bind x = op >>= \f -> p >>= rest . f x
{-# INLINE chainl #-}

-- | Tries to repeatedly parse two @__p__@ operands
-- with @__infix right-associative__@ binary operator @__op__@.
--
-- In other words, this can parse any expression consisting of
-- binary operator that is between its operands and is right-associative.
-- The result will be folded if the operation is evaluable.
--
-- See also 'chainr' and 'chainl1'.
--
-- >>> op = symbol "^" $> (^)
-- >>> t' (chainr1 op (strip integer)) "2 ^ 3 ^ 4"
-- 2417851639229258349412352
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- 'Text.S.Expr.powOp''.
--
-- >>> op = (symbol "+" $> (+)) <|> (symbol "-" $> (-))
-- >>> t' (chainr1 op (strip integer)) "7 - 4 + 2"
-- 1
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- @'Text.S.Expr.addOp' '<|>' 'Text.S.Expr.subOp'@.
--
chainr1 :: MonadPlus m => m (a -> a -> a) -> m a -> m a
chainr1 op p = p >>= chainr op p
{-# INLINE chainr1 #-}

-- |
chainr :: MonadPlus m => m (a -> a -> a) -> m a -> a -> m a
chainr op p = rest
 where
  rest x = bind x <|> pure x
  bind x = op >>= (\f -> f x <$> (p >>= rest))
{-# INLINE chainr #-}

-- | Tries to repeatedly parse two @__p__@ operands
-- with @__prefix or polish prefix__@ binary operator @__op__@.
--
-- In other words, this can parse any expression consisting of
-- binary operator that precedes its operands.
-- The result will be folded if the operation is evaluable.
--
-- See also 'chainq1'.
--
-- >>> op = strip (symbol "^") $> (^)
-- >>> t' (chainp1 op (strip integer)) "^ ^ 2 3 4"
-- 4096
--
-- >>> t' (chainp1 op (strip integer)) "^ 2 ^ 3 4"
-- 2417851639229258349412352
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- 'Text.S.Expr.powOp''.
--
-- >>> op = binop "+" (+) <|> binop "-" (-) <|> binop "*" (*)
-- >>> t' (chainp1 op (strip integer)) "- 20 * + 2 3 4"
-- 0
--
-- For more information about the @__op__@ in the example above,
-- See @'Text.S.Expr.addOp', 'Text.S.Expr.subOp', and 'Text.S.Expr.mulOp'@.
--
chainp1 :: MonadPlus m => m (a -> a -> a) -> m a -> m a
chainp1 op p = op <*> o <*> o where o = chainp1 op p <|> p
{-# INLINE chainp1 #-}

-- |
chainp :: MonadPlus m => m (a -> a -> a) -> m a -> a -> m a
chainp op p x = op <*> pure x <*> o where o = chainp1 op p <|> p
{-# INLINE chainp #-}

-- | Tries to repeatedly parse two @__p__@ operands
-- with @__postfix or reverse-polish prefix__@ binary operator @__op__@.
--
-- In other words, this can parse any expression consisting of
-- binary operator that follows its operands.
-- The result will be folded if the operation is evaluable.
--
-- See also 'chainp1'.
--
-- >>> op = strip (symbol "^") $> (^)
-- >>> t' (chainq1 op (strip integer)) "2 3 ^ 4 ^"
-- 4096
--
-- >>> t' (chainq1 op (strip integer)) "2 3 4 ^ ^"
-- 2417851639229258349412352
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- 'Text.S.Expr.powOp''.
--
-- >>> op = binop "+" (+) <|> binop "-" (-) <|> binop "*" (*)
-- >>> t' (chainq1 op (strip integer)) "2 3 + 4 * 20 -"
-- 0
--
-- For more information about the @__op__@ in the example above,
-- See @'Text.S.Expr.addOp', 'Text.S.Expr.subOp', and 'Text.S.Expr.mulOp'@.
--
chainq1 :: MonadPlus m => m (a -> a -> a) -> m a -> m a
chainq1 op p = p >>= chainq op p
{-# INLINE chainq1 #-}

-- |
chainq :: MonadPlus m => m (a -> a -> a) -> m a -> a -> m a
chainq op p = rest
 where
  rest x = find x <|> pure x
  find x = (p >>= rest) >>= bind x
  bind x y = op >>= rest . flip uncurry (x, y)
{-# INLINE chainq #-}

-- |
betweenOp :: MonadPlus m => m (a -> a) -> m (a -> a) -> m a -> m a
betweenOp pre post p = (option id pre <*> p) <**> option id post
{-# INLINE betweenOp #-}
