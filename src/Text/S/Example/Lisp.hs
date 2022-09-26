{-# Language DeriveAnyClass #-}
{-# Language LambdaCase #-}
{-# Language StandaloneDeriving #-}
{-# Language OverloadedStrings #-}

module Text.S.Example.Lisp where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           System.Console.Haskeline
import           Text.S


data Sexp = Atom Atom
          | List List
          deriving (Eq, Ord)


data Atom = NIL
          | S String
          | K String
          | B Bool
          | I Integer
          | R Double
          | Q String
          deriving (Eq, Ord)


type List = [Sexp]


deriving instance Pretty Atom
deriving instance Pretty Sexp

----------
-- Env
----------

type Env = M.Map String Sexp



----------
-- Read
----------

sexp :: Parser Sexp
sexp = choice [nil, str, bool, real, int, key, sym, form, quote]

nil :: Parser Sexp
nil = Atom NIL <$ (symbol "nil" <|> symbol "'nil")

key :: Parser Sexp
key = Atom . K <$> (symbol ":" *> identifier lispdef)

sym :: Parser Sexp
sym = Atom . S <$> identifier lispdef

str :: Parser Sexp
str = Atom . Q <$> stringLit

bool :: Parser Sexp
bool = Atom . B <$> (symbol "t" $> True)

real :: Parser Sexp
real = Atom . R <$> float

int :: Parser Sexp
int = Atom . I <$> integer

form :: Parser Sexp
form =
  List <$> between (symbol "(") (symbol ")") (skip *> endBy (many space) sexp)

-- vector :: Parser Sexp
-- vector =
  -- List <$> between (symbol "[") (symbol "]") (skip *> endBy (many space) sexp)

quote :: Parser Sexp
quote = symbol "'" *> sexp

read' :: Text -> Either String Sexp
read' s = case parse' sexp s of
  Ok ok (State stream _ _)
    | isEmpty stream -> Right ok
    | otherwise -> err ["*** More than one sexp in input ***", T.unpack stream]
  Error state -> err ["*** Parsing error ***", TL.unpack (pretty state)]



----------
-- Eval
----------

eval :: Env -> Sexp -> Either String (Sexp, Env)
eval env sexp = case sexp of
  a@(Atom _)                 -> pure (a, env)
  List (Atom (S sym) : rest) -> apply env sym rest
  List (Atom v : _) -> err ["*** Eval error *** Invalid function:", show v]
  _                          -> err ["nothing"]


apply :: Env -> String -> [Sexp] -> Either String (Sexp, Env)
apply env sym rest = err ["Not implemented yet"]

----------
-- Print
----------

print' :: MonadIO m => Sexp -> InputT m ()
print' = outputStrLn . TL.unpack . pretty

err :: [String] -> Either String a
err = Left . unwords


instance Show Atom where
  show = \case
    NIL       -> "NIL"
    S symbol  -> symbol
    K keyword -> keyword
    B bool    -> show bool
    I int     -> show int
    R real    -> show real
    Q string  -> string


instance Show Sexp where
  show = \case
    Atom atom -> show atom
    List list -> "(" <> unwords (show <$> list) <> ")"


----------
-- Loop
----------
-- |
repl :: IO ()
repl = runInputT defaultSettings (loop M.empty)
 where
  loop env = do
    input <- getInputLine "SLISP> "
    case input of
      Nothing    -> pure ()
      Just "q"   -> pure ()
      Just []    -> loop env
      Just input -> case read' (fromString input) >>= eval env of
        Left  err         -> outputStrLn err >> loop env
        Right (expr, env) -> print' expr >> loop env
