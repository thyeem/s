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
import           Control.Monad
import           Text.S.Combinator
import           Text.S.Internal
import           Text.S.Lexeme


-- |
data Operator m a  = PrefixU  (m (a -> a))
                   | PostfixU (m (a -> a))
                   | InfixL   (m (a -> a -> a))
                   | InfixR   (m (a -> a -> a))
                   | PrefixB  (m (a -> a -> a))
                   | PostfixB (m (a -> a -> a))

-- |
type OpPriority m a = [[Operator m a]]


-- |
type OpRecord m a
  = ( [m (a -> a)]
    , [m (a -> a)]
    , [m (a -> a -> a)]
    , [m (a -> a -> a)]
    , [m (a -> a -> a)]
    , [m (a -> a -> a)]
    )


priority :: (Stream s, NFData s) => OpPriority (ParserS s) Integer
priority =
  [ [prefixU "-" negate, prefixU "+" id]
  , [postfixU "++" (+ 1), postfixU "--" (subtract 1)]
  , [infixL "*" (*), infixL "/" div]
  , [infixL "+" (+), infixL "-" (-)]
  ]

expr :: (Stream s, NFData s) => ParserS s Integer
expr = expr' unit priority

unit :: (Stream s, NFData s) => ParserS s Integer
unit = strip integer <|> parens expr

-- |
expr' :: MonadPlus m => m a -> OpPriority m a -> m a
expr' = foldl setLevelPriority

-- |
setLevelPriority :: MonadPlus m => m a -> [Operator m a] -> m a
setLevelPriority unit ops = choice [term]
-- setLevelPriority unit ops = choice [expr'r, expr'l, expr'p, expr'q]
 where
  (up, uq, bl, br, bp, bq) = foldr sortOp ([], [], [], [], [], []) ops
  term                     = bindu (choice up) (choice uq) unit
  -- expr'r                   = bindr (choice br) term
  -- expr'l                   = bindl (choice bl) term
  -- expr'p                   = bindp (choice bp) term
  -- expr'q                   = bindq (choice bq) term

-- |
sortOp :: Operator m a -> OpRecord m a -> OpRecord m a
sortOp (PrefixU  op) (up, uq, bl, br, bp, bq) = (op : up, uq, bl, br, bp, bq)
sortOp (PostfixU op) (up, uq, bl, br, bp, bq) = (up, op : uq, bl, br, bp, bq)
sortOp (InfixL   op) (up, uq, bl, br, bp, bq) = (up, uq, op : bl, br, bp, bq)
sortOp (InfixR   op) (up, uq, bl, br, bp, bq) = (up, uq, bl, op : br, bp, bq)
sortOp (PrefixB  op) (up, uq, bl, br, bp, bq) = (up, uq, bl, br, op : bp, bq)
sortOp (PostfixB op) (up, uq, bl, br, bp, bq) = (up, uq, bl, br, bp, op : bq)

-- |
prefixU :: (Stream s, NFData s) => String -> (a -> a) -> Operator (ParserS s) a
prefixU sym = PrefixU . unop sym

-- |
postfixU
  :: (Stream s, NFData s) => String -> (a -> a) -> Operator (ParserS s) a
postfixU sym = PostfixU . unop sym

-- |
infixL
  :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator (ParserS s) a
infixL sym = InfixL . binop sym

-- |
infixR
  :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator (ParserS s) a
infixR sym = InfixR . binop sym

-- |
prefixB
  :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator (ParserS s) a
prefixB sym = PrefixB . binop sym

-- |
postfixB
  :: (Stream s, NFData s) => String -> (a -> a -> a) -> Operator (ParserS s) a
postfixB sym = PostfixB . binop sym

-- |
binop
  :: (Stream s, NFData s) => String -> (a -> a -> a) -> ParserS s (a -> a -> a)
binop sym f = strip (symbol sym) $> f

-- |
unop :: (Stream s, NFData s) => String -> (a -> a) -> ParserS s (a -> a)
unop sym f = strip (symbol sym) $> f

-- |
addOp :: (Stream s, NFData s, Num a) => ParserS s (a -> a -> a)
addOp = binop "+" (+)

-- |
--
subOp :: (Stream s, NFData s, Num a) => ParserS s (a -> a -> a)
subOp = binop "-" (-)

-- |
--
mulOp :: (Stream s, NFData s, Num a) => ParserS s (a -> a -> a)
mulOp = binop "*" (*)

-- |
--
divOp :: (Stream s, NFData s, Num a, Fractional a) => ParserS s (a -> a -> a)
divOp = binop "/" (/)

-- |
--
powOp' :: (Stream s, NFData s, Num a, Integral a) => ParserS s (a -> a -> a)
powOp' = binop "^" (^)

-- |
--
powOp :: (Stream s, NFData s, Num a, Floating a) => ParserS s (a -> a -> a)
powOp = binop "**" (**)

-- |
negOp :: (Stream s, NFData s, Num a, Floating a) => ParserS s (a -> a)
negOp = unop "-" negate

-- |
posOp :: (Stream s, NFData s, Num a, Floating a) => ParserS s (a -> a)
posOp = unop "+" id
