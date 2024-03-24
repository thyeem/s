-- |
-- Module      : Text.S.Expr
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
module Text.S.Expr
  ( module Text.S.Expr
  )
where

import Control.Monad (MonadPlus (mzero))
import Data.Functor (($>))
import Data.List (foldl')
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
  (a, b, c, d, e, f) = foldl' (flip sortOp) ([], [], [], [], [], []) level
  term = betweenOp (choice a) (choice b) unit
  expr'l = null c ? mzero ::: term >>= chainl (choice c) term
  expr'r = null d ? mzero ::: term >>= chainr (choice d) term
  expr'q = null f ? mzero ::: term >>= chainq (choice f) term
  expr'p = null e ? mzero ::: chainp1 (choice e) term
{-# INLINEABLE applyPriority #-}

sortOp :: Operator s a -> OperatorRecord s a -> OperatorRecord s a
sortOp (PrefixU op) (a, b, c, d, e, f) = (op : a, b, c, d, e, f)
sortOp (PostfixU op) (a, b, c, d, e, f) = (a, op : b, c, d, e, f)
sortOp (InfixL op) (a, b, c, d, e, f) = (a, b, op : c, d, e, f)
sortOp (InfixR op) (a, b, c, d, e, f) = (a, b, c, op : d, e, f)
sortOp (PrefixB op) (a, b, c, d, e, f) = (a, b, c, d, op : e, f)
sortOp (PostfixB op) (a, b, c, d, e, f) = (a, b, c, d, e, op : f)
{-# INLINEABLE sortOp #-}

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
binop sym f = strip (symbol sym) $> f
{-# INLINE binop #-}

unop :: Stream s => String -> (a -> b) -> S s (a -> b)
unop sym f = strip (symbol sym) $> f
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
