module Text.S.Example.Sexp where

data Sexp = Atom
          | List [Sexp]
            deriving (Eq, Show)
