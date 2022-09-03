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
setLevelPriority :: ParserS s a -> [Operator s a] -> ParserS s a
setLevelPriority atom ops =
  (term >>= expr'l)
    <|> (term >>= expr'r)
    -- <|> (term >>= expr'q)
    -- <|> expr'p
    <|> term
 where
  (a, b, c, d, e, f) = foldl' (flip sortOp) ([], [], [], [], [], []) ops
  term               = betweenOp (choice a) (choice b) atom
  expr'l             = if null c then const mzero else chainl (choice c) term
  expr'r             = if null d then const mzero else chainr (choice d) term
  expr'p             = if null e then mzero else chainp1 (choice e) term
  expr'q             = if null f then const mzero else chainq (choice f) term
{-# INLINABLE setLevelPriority #-}

-- |
sortOp :: Operator s a -> OperatorRecord s a -> OperatorRecord s a
sortOp (PrefixU  op) (a, b, c, d, e, f) = (op : a, b, c, d, e, f)
sortOp (PostfixU op) (a, b, c, d, e, f) = (a, op : b, c, d, e, f)
sortOp (InfixL   op) (a, b, c, d, e, f) = (a, b, op : c, d, e, f)
sortOp (InfixR   op) (a, b, c, d, e, f) = (a, b, c, op : d, e, f)
sortOp (PrefixB  op) (a, b, c, d, e, f) = (a, b, c, d, op : e, f)
sortOp (PostfixB op) (a, b, c, d, e, f) = (a, b, c, d, e, op : f)
{-# INLINE sortOp #-}


expr :: (Stream s, NFData s) => ParserS s a -> OperatorTable s a -> ParserS s a
expr = foldl' setLevelPriority

----------------------------------------------------------------------
expr' :: (Stream s, NFData s) => ParserS s Double
expr' = expr atom table
 where
  atom  = parens expr' <|> strip floating
  label = "infixL"
  table = case label of
    "infixL"   -> table'infixL
    "infixR"   -> table'infixR
    "prefixB"  -> table'prefixB
    "postfixB" -> table'postfixB
    "unary"    -> table'unary
    _          -> error "no such op table"

  table'infixL =
    [ [ prefixU "-" negate
      , prefixU "+" id
      ]
    -- , [postfixU "++" (+ 1), postfixU "--" (subtract 1)]
    , [infixL "^" (**)]
    , [infixL "*" (*), infixL "/" (/)]
    , [infixL "+" (+), infixL "-" (-)]
    ]

  table'postfixB =
    [ [ postfixB "^" (**)
      , postfixB "*" (*)
      , postfixB "/" (/)
      , postfixB "+" (+)
      , postfixB "-" (-)
      ]
    ]

  table'prefixB =
    [ [ prefixB "^" (**)
      , prefixB "*" (*)
      , prefixB "/" (/)
      , prefixB "+" (+)
      , prefixB "-" (-)
      ]
    ]

  table'infixR =
    [ [prefixU "-" negate]
    , [postfixU "++" (+ 1), postfixU "--" (subtract 1)]
    , [infixR "^" (**)]
    , [infixR "*" (*), infixR "/" (/)]
    , [infixR "+" (+), infixR "-" (-)]
    ]

  table'unary =
    [[prefixU "-" negate], [postfixU "++" (+ 1), postfixU "--" (subtract 1)]]


op' :: (Stream s, NFData s, Num a, Integral a) => ParserS s (a -> a -> a)
op' = addOp <|> subOp <|> mulOp <|> divOp' <|> powOp'
-- |
-- expr' :: MonadPlus m => m a -> OperatorTable m a -> m a
-- expr' = foldl' setLevelPriority

----------------------------------------------------------------------

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
divOp' :: (Stream s, NFData s, Num a, Integral a) => ParserS s (a -> a -> a)
divOp' = binop "/" div

-- |
--
powOp :: (Stream s, NFData s, Num a, Floating a) => ParserS s (a -> a -> a)
powOp = binop "**" (**)

-- |
--
powOp' :: (Stream s, NFData s, Num a, Integral a) => ParserS s (a -> a -> a)
powOp' = binop "^" (^)

-- |
negOp :: (Stream s, NFData s, Num a) => ParserS s (a -> a)
negOp = unop "-" negate

-- |
posOp :: (Stream s, NFData s, Num a) => ParserS s (a -> a)
posOp = unop "+" id
