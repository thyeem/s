{-# Language DeriveAnyClass #-}
{-# Language StandaloneDeriving #-}

module Text.S.Example.Lisp where

import           System.IO
import           Text.S


data Sexp = A Atom
          | L List
          deriving (Eq, Show)

type List = [Sexp]

data Atom = NIL
          |  S String
          |  K String
          |  B Bool
          |  I Integer
          |  F Rational
          |  E String
          deriving (Eq, Show)

-- Core

int :: Parser Atom
int = I <$> integer

flt :: Parser Atom
flt = F . toRational <$> float

nil :: Parser Atom
nil = NIL <$ (symbol "nil" <|> symbol "'nil")

sym :: Parser Atom
sym = S <$> identifier lispdef

-- Reader


-- Printer

-- |
-- TODO: add history
repl :: IO ()
repl = do
  input <- read'
  if input == "q" then pure () else eval' input >> repl
 where
  read' = putStrLn mempty >> putStr "SLISP> " >> hFlush stdout >> getLine
  eval' = putStrLn

-- vector
-- hash-map
-- fn/macro


deriving instance Pretty Atom
deriving instance Pretty Sexp
