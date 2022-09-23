{-# Language DeriveAnyClass #-}
{-# Language LambdaCase #-}
{-# Language StandaloneDeriving #-}
{-# Language OverloadedStrings #-}

module Text.S.Example.Lisp where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.List                      ( intercalate )
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           System.Console.Haskeline
import           Text.S


data Sexp = A Atom
          | L List
          deriving (Eq, Show)


data Atom = NIL           -- nil
          | S String      -- symbol
          | K String      -- keyword
          | B Bool        -- boolean
          | I Integer     -- integer
          | F Rational    -- float
          | Q String      -- literal string
          deriving (Eq, Show)


type List = [Sexp]


deriving instance Pretty Atom
deriving instance Pretty Sexp


----------
-- Read
----------

sexp :: Parser Sexp
sexp = choice [nil, str, bool, flt, int, key, sym, form, quote]

nil :: Parser Sexp
nil = A NIL <$ lexeme (symbol "nil" <|> symbol "'nil")

key :: Parser Sexp
key = A . K <$> lexeme (symbol ":" *> identifier lispdef)

sym :: Parser Sexp
sym = A . S <$> lexeme (identifier lispdef)

str :: Parser Sexp
str = A . Q <$> lexeme stringLit

bool :: Parser Sexp
bool = A . B <$> lexeme (symbol "t" $> True)

flt :: Parser Sexp
flt = A . F . toRational <$> lexeme float

int :: Parser Sexp
int = A . I <$> lexeme integer

form :: Parser Sexp
form =
  L <$> lexeme (between (symbol "(") (symbol ")") (sepBy (some space) sexp))

quote :: Parser Sexp
quote = symbol "'" *> sexp


read' :: Text -> Either String Sexp
read' s = case parse' sexp s of
  Ok ok _ -> Right ok
  Error st ->
    Left
      . unlines
      $ ["error occurred while parsing s-expression:", TL.unpack (pretty st)]



----------
-- Eval
----------

eval :: Sexp -> Either String Sexp
eval = pure

----------
-- Print
----------

print' :: MonadIO m => Either String Sexp -> InputT m ()
print' = outputStrLn . show'

show' :: Either String Sexp -> String
show' = \case
  Left  err  -> err
  Right sexp -> show sexp

----------
-- Loop
----------
-- |
repl :: IO ()
repl = runInputT defaultSettings loop
 where
  loop = do
    input <- getInputLine "SLISP> "
    case input of
      Nothing    -> pure ()
      Just ""    -> loop
      Just input -> do
        print' $ read' (fromString input) >>= eval
        loop
