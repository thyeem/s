-----------------------------------------------------------------------------
-- |
-- Module      : Text.S.Language
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
--
-----------------------------------------------------------------------------

module Text.S.Expr
  ( module Text.S.Expr
  ) where


import           Control.DeepSeq                ( NFData )
import           Control.Monad                  ( MonadPlus(mzero) )
import           Data.List                      ( foldl' )
import           Text.S.Combinator
import           Text.S.Internal
import           Text.S.Lexeme



-- |
data Operator s a  = PrefixU  (ParserS s (a -> a))
                   | PostfixU (ParserS s (a -> a))
                   | InfixL   (ParserS s (a -> a -> a))
                   | InfixR   (ParserS s (a -> a -> a))
                   | PrefixB  (ParserS s (a -> a -> a))
                   | PostfixB (ParserS s (a -> a -> a))


-- |
type LevelPriority s a = [Operator s a]


-- |
type OperatorTable s a = [LevelPriority s a]


-- |
type OperatorRecord s a
  = ( [ParserS s (a -> a)]
    , [ParserS s (a -> a)]
    , [ParserS s (a -> a -> a)]
    , [ParserS s (a -> a -> a)]
    , [ParserS s (a -> a -> a)]
    , [ParserS s (a -> a -> a)]
    )


-- |
applyPriority :: ParserS s a -> LevelPriority s a -> ParserS s a
applyPriority atom opTable = choice [expr'l, expr'r, expr'p, expr'q, term]
 where
  (a, b, c, d, e, f) = foldl' (flip sortOp) ([], [], [], [], [], []) opTable
  term               = betweenOp (choice a) (choice b) atom
  expr'l             = null c ? mzero ::: term >>= chainl (choice c) term
  expr'r             = null d ? mzero ::: term >>= chainr (choice d) term
  expr'q             = null f ? mzero ::: term >>= chainq (choice f) term
  expr'p             = null e ? mzero ::: chainp1 (choice e) term
{-# INLINABLE applyPriority #-}

-- |
sortOp :: Operator s a -> OperatorRecord s a -> OperatorRecord s a
sortOp (PrefixU  op) (a, b, c, d, e, f) = (op : a, b, c, d, e, f)
sortOp (PostfixU op) (a, b, c, d, e, f) = (a, op : b, c, d, e, f)
sortOp (InfixL   op) (a, b, c, d, e, f) = (a, b, op : c, d, e, f)
sortOp (InfixR   op) (a, b, c, d, e, f) = (a, b, c, op : d, e, f)
sortOp (PrefixB  op) (a, b, c, d, e, f) = (a, b, c, d, op : e, f)
sortOp (PostfixB op) (a, b, c, d, e, f) = (a, b, c, d, e, op : f)
{-# INLINE sortOp #-}


-- | expression parser builder
expr :: (Stream s, NFData s) => ParserS s a -> OperatorTable s a -> ParserS s a
expr = foldl' applyPriority
{-# INLINABLE expr #-}

-- |
prefixU :: (Stream s, NFData s) => String -> (a -> a) -> Operator s a
prefixU sym = PrefixU . unop sym
{-# INLINE prefixU #-}

-- |
postfixU :: (Stream s, NFData s) => String -> (a -> a) -> Operator s a
postfixU sym = PostfixU . unop sym
{-# INLINE postfixU #-}

-- |
infixL :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator s a
infixL sym = InfixL . binop sym
{-# INLINE infixL #-}

-- |
infixR :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator s a
infixR sym = InfixR . binop sym
{-# INLINE infixR #-}

-- |
prefixB :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator s a
prefixB sym = PrefixB . binop sym
{-# INLINE prefixB #-}

-- |
postfixB :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator s a
postfixB sym = PostfixB . binop sym
{-# INLINE postfixB #-}

-- |
binop
  :: (Stream s, NFData s) => String -> (a -> a -> a) -> ParserS s (a -> a -> a)
binop sym f = strip (symbol sym) $> f
{-# INLINE binop #-}

-- |
unop :: (Stream s, NFData s) => String -> (a -> a) -> ParserS s (a -> a)
unop sym f = strip (symbol sym) $> f
{-# INLINE unop #-}

-- |
addOp :: (Stream s, NFData s, Num a) => ParserS s (a -> a -> a)
addOp = binop "+" (+)
{-# INLINE addOp #-}

-- |
--
subOp :: (Stream s, NFData s, Num a) => ParserS s (a -> a -> a)
subOp = binop "-" (-)
{-# INLINE subOp #-}

-- |
--
mulOp :: (Stream s, NFData s, Num a) => ParserS s (a -> a -> a)
mulOp = binop "*" (*)
{-# INLINE mulOp #-}

-- |
--
divOp :: (Stream s, NFData s, Num a, Fractional a) => ParserS s (a -> a -> a)
divOp = binop "/" (/)
{-# INLINE divOp #-}

-- |
--
divOp' :: (Stream s, NFData s, Num a, Integral a) => ParserS s (a -> a -> a)
divOp' = binop "/" div
{-# INLINE divOp' #-}

-- |
--
powOp :: (Stream s, NFData s, Num a, Floating a) => ParserS s (a -> a -> a)
powOp = binop "**" (**)
{-# INLINE powOp #-}

-- |
--
powOp' :: (Stream s, NFData s, Num a, Integral a) => ParserS s (a -> a -> a)
powOp' = binop "^" (^)
{-# INLINE powOp' #-}

-- |
negOp :: (Stream s, NFData s, Num a) => ParserS s (a -> a)
negOp = unop "-" negate
{-# INLINE negOp #-}

-- |
posOp :: (Stream s, NFData s, Num a) => ParserS s (a -> a)
posOp = unop "+" id
{-# INLINE posOp #-}
