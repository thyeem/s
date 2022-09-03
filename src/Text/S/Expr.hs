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


import           Control.DeepSeq                ( NFData
                                                , force
                                                )
import           Control.Monad
import           Data.Bool                      ( bool )
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
type OperatorTable s a = [[Operator s a]]


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
processRecord :: ParserS s a -> [Operator s a] -> ParserS s a
processRecord atom opTable =
  (term >>= expr'l)
    <|> (term >>= expr'r)
    <|> (term >>= expr'q)
    <|> expr'p
    <|> term
 where
  (a, b, c, d, e, f) = foldl' (flip sortOp) ([], [], [], [], [], []) opTable
  term               = betweenOp (choice a) (choice b) atom
  expr'l             = bool (chainl (choice c) term) (const mzero) (null c)
  expr'r             = bool (chainr (choice d) term) (const mzero) (null d)
  expr'q             = bool (chainq (choice f) term) (const mzero) (null f)
  expr'p             = bool (chainp1 (choice e) term) mzero (null e)
{-# INLINABLE processRecord #-}

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
expr = foldl' processRecord

-- |
prefix'u :: (Stream s, NFData s) => String -> (a -> a) -> Operator s a
prefix'u sym = PrefixU . unop sym
{-# INLINE prefix'u #-}

-- |
postfix'u :: (Stream s, NFData s) => String -> (a -> a) -> Operator s a
postfix'u sym = PostfixU . unop sym
{-# INLINE postfix'u #-}

-- |
infix'l :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator s a
infix'l sym = InfixL . binop sym
{-# INLINE infix'l #-}

-- |
infix'r :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator s a
infix'r sym = InfixR . binop sym
{-# INLINE infix'r #-}

-- |
prefix'b :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator s a
prefix'b sym = PrefixB . binop sym
{-# INLINE prefix'b #-}

-- |
postfix'b :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator s a
postfix'b sym = PostfixB . binop sym
{-# INLINE postfix'b #-}

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
