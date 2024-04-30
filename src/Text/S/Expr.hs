-- |
-- Module      : Text.S.Expr
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module defines a generalized expression builder.
-- One can easily write an expression parser by simply defining
-- the expression unit (atom) and operator precedence (priority).
module Text.S.Expr where

import Control.Applicative ((<**>))
import Control.Monad (MonadPlus (mzero))
import Data.Functor (($>))
import Data.List (foldl')
import Text.S.Base
import Text.S.Combinator
import Text.S.Internal
import Text.S.Lexeme

data Operator s a
  = PrefixU (S s (a -> a))
  | PostfixU (S s (a -> a))
  | InfixL (S s (a -> a -> a))
  | InfixR (S s (a -> a -> a))
  | PrefixB (S s (a -> a -> a))
  | PostfixB (S s (a -> a -> a))

type LevelPriority s a = [Operator s a]

type OperatorTable s a = [LevelPriority s a]

type OperatorRecord s a =
  ( [S s (a -> a)]
  , [S s (a -> a)]
  , [S s (a -> a -> a)]
  , [S s (a -> a -> a)]
  , [S s (a -> a -> a)]
  , [S s (a -> a -> a)]
  )

applyPriority :: S s a -> LevelPriority s a -> S s a
applyPriority unit level = choice [expr'l, expr'r, expr'p, expr'q, term]
 where
  (a, b, c, d, e, f) = foldl' groupOp ([], [], [], [], [], []) level
  term = (option id (choice a) <*> unit) <**> option id (choice b)
  expr'l
    | null c = mzero
    | otherwise = term >>= chainl (choice c) term
  expr'r
    | null d = mzero
    | otherwise = term >>= chainr (choice d) term
  expr'q
    | null f = mzero
    | otherwise = term >>= chainq (choice f) term
  expr'p
    | null e = mzero
    | otherwise = chainp1 (choice e) term
{-# INLINEABLE applyPriority #-}

groupOp :: OperatorRecord s a -> Operator s a -> OperatorRecord s a
groupOp (a, b, c, d, e, f) op = case op of
  PrefixU o -> (o : a, b, c, d, e, f)
  PostfixU o -> (a, o : b, c, d, e, f)
  InfixL o -> (a, b, o : c, d, e, f)
  InfixR o -> (a, b, c, o : d, e, f)
  PrefixB o -> (a, b, c, d, o : e, f)
  PostfixB o -> (a, b, c, d, e, o : f)
{-# INLINEABLE groupOp #-}

-- | expression parser builder
expr :: Stream s => S s a -> OperatorTable s a -> S s a
expr = foldl' applyPriority
{-# INLINE expr #-}

prefixU :: (Stream s) => String -> (a -> a) -> Operator s a
prefixU sym = PrefixU . unop sym
{-# INLINE prefixU #-}

postfixU :: (Stream s) => String -> (a -> a) -> Operator s a
postfixU sym = PostfixU . unop sym
{-# INLINE postfixU #-}

infixL :: (Stream s) => String -> (a -> a -> a) -> Operator s a
infixL sym = InfixL . binop sym
{-# INLINE infixL #-}

infixR :: (Stream s) => String -> (a -> a -> a) -> Operator s a
infixR sym = InfixR . binop sym
{-# INLINE infixR #-}

prefixB :: (Stream s) => String -> (a -> a -> a) -> Operator s a
prefixB sym = PrefixB . binop sym
{-# INLINE prefixB #-}

postfixB :: (Stream s) => String -> (a -> a -> a) -> Operator s a
postfixB sym = PostfixB . binop sym
{-# INLINE postfixB #-}

binop :: Stream s => String -> (a -> a -> b) -> S s (a -> a -> b)
binop sym f = strip (string sym) $> f
{-# INLINE binop #-}

unop :: Stream s => String -> (a -> b) -> S s (a -> b)
unop sym f = strip (string sym) $> f
{-# INLINE unop #-}

addOp :: (Stream s, Num a) => S s (a -> a -> a)
addOp = binop "+" (+)
{-# INLINE addOp #-}

subOp :: (Stream s, Num a) => S s (a -> a -> a)
subOp = binop "-" (-)
{-# INLINE subOp #-}

mulOp :: (Stream s, Num a) => S s (a -> a -> a)
mulOp = binop "*" (*)
{-# INLINE mulOp #-}

divOp :: (Stream s, Num a, Fractional a) => S s (a -> a -> a)
divOp = binop "/" (/)
{-# INLINE divOp #-}

divOp' :: (Stream s, Num a, Integral a) => S s (a -> a -> a)
divOp' = binop "/" div
{-# INLINE divOp' #-}

powOp :: (Stream s, Num a, Floating a) => S s (a -> a -> a)
powOp = binop "**" (**)
{-# INLINE powOp #-}

powOp' :: (Stream s, Num a, Integral a) => S s (a -> a -> a)
powOp' = binop "^" (^)
{-# INLINE powOp' #-}

negOp :: (Stream s, Num a) => S s (a -> a)
negOp = unop "-" negate
{-# INLINE negOp #-}

posOp :: (Stream s, Num a) => S s (a -> a)
posOp = unop "+" id
{-# INLINE posOp #-}

eqOp :: (Stream s, Eq a) => S s (a -> a -> Bool)
eqOp = binop "==" (==)
{-# INLINE eqOp #-}

ltOp :: (Stream s, Ord a) => S s (a -> a -> Bool)
ltOp = binop "<" (<)
{-# INLINE ltOp #-}

gtOp :: (Stream s, Ord a) => S s (a -> a -> Bool)
gtOp = binop ">" (>)
{-# INLINE gtOp #-}

leOp :: (Stream s, Ord a) => S s (a -> a -> Bool)
leOp = binop "<=" (<=)
{-# INLINE leOp #-}

geOp :: (Stream s, Ord a) => S s (a -> a -> Bool)
geOp = binop ">=" (>=)
{-# INLINE geOp #-}
