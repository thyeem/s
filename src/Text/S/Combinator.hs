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
  , void
  ) where

import           Control.Applicative            ( Alternative(..)
                                                , liftA2
                                                )
import           Control.Monad                  ( MonadPlus(..)
                                                , replicateM
                                                , replicateM_
                                                )
import           Data.Bifunctor                 ( first )
import           Data.Foldable                  ( foldl' )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Text.S.Internal



-------------------------
-- parser combinators
-------------------------
-- | Tries to parse with parsers in the list untill one of them succeeds.
--
-- >>> t' (choice [letter, special, digit]) "$parser"
-- '$'
--
choice :: MonadPlus m => [m a] -> m a
choice = foldl' (<|>) mzero

-- | Firstly tries to parse with parser @__p__@.
--
-- If failed, it returns @__x__@. This is useful to set default value of parser @__p__@.
--
-- >>> t' (option "Mars" spaces) "nuclear-bomb-explosion -> Earth"
-- "Mars"
--
option :: MonadPlus m => a -> m a -> m a
option x p = p <|> return x

-- | Tries to parse @__n-times__@ with the given parser. The same as 'replicateM'.
--
-- See also 'skipCount'
--
-- >>> t' (count 6 letter) "Parser-Combinator"
-- "Parser"
--
count :: MonadPlus m => Int -> m a -> m [a]
count = replicateM

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

-- | Parses @__0+(zero or more)__@ occurrences of parser @__p__@,
-- which is separated by separator @__sep__@.
--
-- This consumes the result of separator parser @__sep__@ from input stream.
--
-- See also 'endBy'.
--
-- >>> t' (sepBy decimals (symbol ",")) "1,2,3,4,5"
-- [1,2,3,4,5]
--
-- >>> t' (sepBy decimals (symbol ".")) "1,2,3,4,5"
-- []
--
sepBy :: MonadPlus m => m a -> m b -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parses @__1+(one or more)__@ occurrences of parser @__p__@,
-- which is separated by separator @__sep__@.
--
-- This consumes the result of separator parser @__sep__@ from input stream.
--
-- See also 'endBy1'
--
-- >>> t' (sepBy1 (anystringBut "a") (symbol "a")) "parser combinator"
-- ["p","rser combin","tor"]
--
sepBy1 :: MonadPlus m => m a -> m b -> m [a]
sepBy1 p sep = liftA2 (:) p (some (sep *> p))

-- | Parses @__0+(zero or more)__@ occurrences of parser @__p__@,
-- which is ended by parser @__end__@.
--
-- This consumes the result of parser @__end__@ from input stream.
--
-- See also 'sepBy'.
--
-- >>> p = some $ alphaNum <|> char '=' <|> space
-- >>> t' (endBy p (char ';')) "int a=1;int b=2;"
-- ["int a=1","int b=2"]
--
-- >>> t' (endBy digits (char ';')) "10:20:30:"
-- []
--
endBy :: MonadPlus m => m a -> m b -> m [a]
endBy p end = many (p <* end)

-- | Parses @__1+(one or more)__@ occurrences of parser @__p__@,
-- which is ended by parser @__end__@.
--
-- This consumes the result of parser @__end__@ from input stream.
--
-- See also 'sepBy1'
--
-- >>> t' (endBy1 (anystringBut "a") (symbol "a")) "parser combinator"
-- ["p","rser combin"]
--
endBy1 :: MonadPlus m => m a -> m b -> m [a]
endBy1 p end = some (p <* end)

-- | Tries to parse @__0+(zero or more)-times__@ with parser @__p__@
-- until parser @__end__@ succeeds.
--
-- This consumes the result of parser @__end__@ from input stream.
--
-- See also 'manyTill''. It keeps the result of parser @__end__@.
--
-- >>> p = string "{-" *> manyTill anychar (string "-}")
-- >>> t' p "{- haskell block comment here -}"
-- " haskell block comment here "
--
-- >>> q = string "{-" *> manyTill special (string "-}")
-- >>> t' q "{--}"
-- ""
--
manyTill :: MonadPlus m => m a -> m b -> m [a]
manyTill p end = someTill p end <|> (end $> [])

-- | Tries to parse @__1+(one or more)-times__@ with parser @__p__@
-- until parser @__end__@ succeeds.
--
-- See also 'someTill''. It keeps the result of parser @__end__@.
--
-- >>> p = someTill (letter <|> space) (string ":")
-- >>> t' p "for x in xs: f(x)"
-- "for x in xs"
--
someTill :: MonadPlus m => m a -> m b -> m [a]
someTill p end = liftA2 (:) p (manyTill p end)

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
-- >>> t' (manyTill' p special) "stop COVID-19"
-- ("stop COVID",'-')
--
-- >>> t' (manyTill' digit letters) "stop COVID-19"
-- ("","stop")
--
manyTill' :: MonadPlus m => m a -> m b -> m ([a], b)
manyTill' p end = someTill' p end <|> (([], ) <$> end)

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
-- >>> t' (someTill' upper stopCodon) geneticSequence
-- ("AUCUCGUCAUCUCGU","UAA")
--
someTill' :: MonadPlus m => m a -> m b -> m ([a], b)
someTill' p end = liftA2 f p (manyTill' p end) where f a b = first (a :) b

-- | Tries to parse with parser @__p__@.
-- If succeeds, then consume the result and throws it away. Otherwise ignore it.
--
-- >>> ts' (skipOptional special) "$PARSER_COMBINATOR"
-- "PARSER_COMBINATOR"
--
-- >>> ts' (skipOptional letter) "$PARSER_COMBINATOR"
-- "$PARSER_COMBINATOR"
--
skipOptional :: MonadPlus m => m a -> m ()
skipOptional p = void p <|> pure ()

-- | Tries to parse @__0+(zero or more)-times__@ with parser @__p__@,
-- then discards the result.
--
-- >>> ts' (skipMany (anycharBut '#')) "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
-- "#-G-Ab-A-Bb-B"
--
-- >>> ts' (skipMany digit) "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
-- "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
--
skipMany :: MonadPlus m => m a -> m ()
skipMany = void . many

-- | Tries to parse @__1+(one or more)-times__@ with parser @__p__@,
-- then discards the result.
--
-- >>> ts' (skipSome (letter <|> char '-')) "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
-- "#-G-Ab-A-Bb-B"
--
skipSome :: MonadPlus m => m a -> m ()
skipSome p = p *> skipMany p

-- | Tries to parse @__n-times__@ with the given parser.
--
-- The same as 'count', but this discards the result.
-- This is equivalent to 'replicateM_'.
--
-- See also 'count'
--
-- >>> ts' (skipCount 5 (digit *> char ':')) "1:2:3:4:5:6:7:8"
-- "6:7:8"
--
skipCount :: MonadPlus m => Int -> m a -> m ()
skipCount = replicateM_

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
-- >>> t' (skipManyTill upper stopCodon) geneticSequence
-- "UAA"
--
-- >>> ts' (skipManyTill upper stopCodon) geneticSequence
-- "CUCGUA"
--
-- >>> ts' (skipManyTill upper stopCodon) "UAACUCGUA"
-- "CUCGUA"
--
skipManyTill :: MonadPlus m => m a -> m b -> m b
skipManyTill p end = go where go = end <|> (p *> go)

-- | Tries to parse @__1+(one or more)-times__@ with parser @__p__@,
-- until parser @__end__@ succeeds.
--
-- The result of parser @__end__@ is returned
-- while the result of parser @__p__@ is discarded.
--
-- See also 'skipManyTill'.
--
-- >>> t' (skipSomeTill anychar (char '#')) "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
-- '#'
--
-- >>> ts' (skipSomeTill anychar (char '#')) "C-Db-D-Eb-E-F-F#-G-Ab-A-Bb-B"
-- "-G-Ab-A-Bb-B"
--
skipSomeTill :: MonadPlus m => m a -> m b -> m b
skipSomeTill p end = p *> skipManyTill p end

-- | Tries to repeatedly parse two @__p__@ operands
-- with @__infix left-associative__@ binary operator @__op__@.
--
-- In other words, this can parse any expression consisting of
-- binary operator that is between its operands and is left-associative.
-- The result will be folded if the operation is evaluable.
--
-- See also 'bindr'.
--
-- >>> op = symbol "^" $> (^)
-- >>> t' (bindl op (strip integer)) "2 ^ 3 ^ 4"
-- 4096
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- 'Text.S.Expr.powOp''.
--
-- >>> op = (symbol "+" $> (+)) <|> (symbol "-" $> (-))
-- >>> t' (bindl op (strip integer)) "7 - 4 + 2"
-- 5
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- @'Text.S.Expr.addOp' '<|>' 'Text.S.Expr.subOp'@.
--
bindl :: MonadPlus m => m (a -> a -> a) -> m a -> m a
bindl op p = p >>= rest
 where
  rest x = bind x <|> pure x
  bind x = op >>= \f -> p >>= rest . f x

-- | Tries to repeatedly parse two @__p__@ operands
-- with @__infix right-associative__@ binary operator @__op__@.
--
-- In other words, this can parse any expression consisting of
-- binary operator that is between its operands and is right-associative.
-- The result will be folded if the operation is evaluable.
--
-- See also 'bindl'.
--
-- >>> op = symbol "^" $> (^)
-- >>> t' (bindr op (strip integer)) "2 ^ 3 ^ 4"
-- 2417851639229258349412352
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- 'Text.S.Expr.powOp''.
--
-- >>> op = (symbol "+" $> (+)) <|> (symbol "-" $> (-))
-- >>> t' (bindr op (strip integer)) "7 - 4 + 2"
-- 1
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- @'Text.S.Expr.addOp' '<|>' 'Text.S.Expr.subOp'@.
--
bindr :: MonadPlus m => m (a -> a -> a) -> m a -> m a
bindr op p = p >>= rest
 where
  rest x = bind x <|> pure x
  bind x = op >>= (\f -> f x <$> (p >>= rest))

-- | Tries to repeatedly parse two @__p__@ operands
-- with @__prefix or polish prefix__@ binary operator @__op__@.
--
-- In other words, this can parse any expression consisting of
-- binary operator that precedes its operands.
-- The result will be folded if the operation is evaluable.
--
-- See also 'bindq'.
--
-- >>> op = strip (symbol "^") $> (^)
-- >>> t' (bindp op (strip integer)) "^ ^ 2 3 4"
-- 4096
--
-- >>> t' (bindp op (strip integer)) "^ 2 ^ 3 4"
-- 2417851639229258349412352
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- 'Text.S.Expr.powOp''.
--
-- >>> op = binop "+" (+) <|> binop "-" (-) <|> binop "*" (*)
-- >>> t' (bindp op (strip integer)) "- 20 * + 2 3 4"
-- 0
--
-- For more information about the @__op__@ in the example above,
-- See @'Text.S.Expr.addOp', 'Text.S.Expr.subOp', and 'Text.S.Expr.mulOp'@.
--
bindp :: MonadPlus m => m (a -> a -> a) -> m a -> m a
bindp op p = op >>= (\f -> liftA2 f x x) where x = bindp op p <|> p

-- | Tries to repeatedly parse two @__p__@ operands
-- with @__postfix or reverse-polish prefix__@ binary operator @__op__@.
--
-- In other words, this can parse any expression consisting of
-- binary operator that follows its operands.
-- The result will be folded if the operation is evaluable.
--
-- See also 'bindp'.
--
-- >>> op = strip (symbol "^") $> (^)
-- >>> t' (bindq op (strip integer)) "2 3 ^ 4 ^"
-- 4096
--
-- >>> t' (bindq op (strip integer)) "2 3 4 ^ ^"
-- 2417851639229258349412352
--
-- the @__op__@ in the example above is equivalent to \(\to\)
-- 'Text.S.Expr.powOp''.
--
-- >>> op = binop "+" (+) <|> binop "-" (-) <|> binop "*" (*)
-- >>> t' (bindq op (strip integer)) "2 3 + 4 * 20 -"
-- 0
--
-- For more information about the @__op__@ in the example above,
-- See @'Text.S.Expr.addOp', 'Text.S.Expr.subOp', and 'Text.S.Expr.mulOp'@.
--
bindq :: MonadPlus m => m (a -> a -> a) -> m a -> m a
bindq op p = p >>= rest
 where
  rest x = find x <|> pure x
  find x = bindq op p >>= bind x
  bind x y = op >>= rest . flip uncurry (x, y)

-- | Tries to repeatedly parse two @__p__@ operands
bindu :: MonadPlus m => m (a -> a) -> m (a -> a) -> m a -> m a
bindu pre post p = do
  pre  <- option id pre
  x    <- p
  post <- option id post
  return . post . pre $ x
