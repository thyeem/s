{-# Language DeriveAnyClass #-}
{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language StandaloneDeriving #-}
{-# Language OverloadedStrings #-}

module Text.S.Example.Lisp where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Dynamic
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
          | B Bool
          | I Integer
          | R Double
          | S String
          | K String
          | L String
          | Q String
          deriving (Eq, Ord)


type List = [Sexp]


deriving instance Pretty Atom
deriving instance Pretty Sexp


----------
-- Env
----------

type Env = M.Map String Sexp

(??) :: Ord k => k -> M.Map k a -> Bool
(??) = M.member


----------
-- Read
----------
-- |
read' :: Text -> Either String Sexp
read' s = case parse' sexp s of
  Ok ok (State stream _ _)
    | isEmpty stream -> Right ok
    | otherwise -> err ["*** More than one sexp in input ***", T.unpack stream]
  Error state -> err ["*** Parsing error ***", TL.unpack (pretty state)]

sexp :: Parser Sexp
sexp = choice [nil, str, bool, real, int, quote, key, sym, form]

nil :: Parser Sexp
nil = Atom NIL <$ (symbol "nil" <|> symbol "'nil")

bool :: Parser Sexp
bool = Atom . B <$> (symbol "t" $> True)

int :: Parser Sexp
int = Atom . I <$> (skip *> integer)

real :: Parser Sexp
real = Atom . R <$> float

sym :: Parser Sexp
sym = Atom . S <$> identifier lispdef

key :: Parser Sexp
key = Atom . K . (":" ++) <$> (symbol ":" *> identifier lispdef)

str :: Parser Sexp
str = Atom . L <$> stringLit

quote :: Parser Sexp
quote = symbol "'" *> (Atom . Q . ("'" ++) . show <$> (skip *> sexp))

form :: Parser Sexp
form =
  List <$> between (symbol "(") (symbol ")") (skip *> endBy (many space) sexp)

-- vector :: Parser Sexp
-- vector =
  -- List <$> between (symbol "[") (symbol "]") (skip *> endBy (many space) sexp)

----------
-- Eval
----------
-- |
eval :: Env -> Sexp -> Either String (Sexp, Env)
eval env sexp = case sexp of
  Atom (Q v)                 -> pure (Atom . Q . tail $ v, env)
  v@Atom{}                   -> pure (v, env)
  List (Atom (S sym) : args) -> apply env sym args
  List (v : _) -> err ["*** Eval error *** Invalid function:", show v]
  v                          -> Right (v, env)


apply :: Env -> String -> [Sexp] -> Either String (Sexp, Env)
apply env sym args = case sym of
  "+"     -> f'calc env sym args
  "list"  -> f'list env args
  "quote" -> f'quote env args
  _ -> err ["*** Eval error *** Symbol's function definition is void:", sym]


f'calc :: Env -> String -> [Sexp] -> Either String (Sexp, Env)
f'calc env sym args = case args of
  [Atom (I a), Atom (I b)] -> Right (Atom . I $ a + b, env)
  [Atom (I a), Atom (R b)] -> Right (Atom . R $ fromIntegral a + b, env)
  [Atom (R a), Atom (I b)] -> Right (Atom . R $ a + fromIntegral b, env)
  [Atom (R a), Atom (R b)] -> Right (Atom . R $ a + b, env)
  _                        -> err ["None"]

f'list :: Env -> [Sexp] -> Either String (Sexp, Env)
f'list env args = Right (List args, env)

f'quote :: Env -> [Sexp] -> Either String (Sexp, Env)
f'quote env args
  | nargs /= 1 = err ["*** Wrong number of arguments ***", "quote,", show nargs]
  | otherwise  = Right (Atom . Q . show . head $ args, env)
  where nargs = length args

----------
-- Print
----------
-- |
print' :: MonadIO m => Sexp -> InputT m ()
print' = outputStrLn . show
-- print' = outputStrLn . TL.unpack . pretty

err :: [String] -> Either String a
err = Left . unwords


instance Show Atom where
  show = \case
    NIL       -> "NIL"
    S symbol  -> symbol
    K keyword -> keyword
    B bool    -> show bool
    I intger  -> show intger
    R real    -> show real
    L string  -> string
    Q string  -> string


instance Show Sexp where
  show = \case
    Atom atom -> show atom
    List list -> "(" <> unwords (show <$> list) <> ")"


----------
-- REPL
----------
-- | repl for SLISP
sl :: IO ()
sl = runInputT defaultSettings (loop M.empty)
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
