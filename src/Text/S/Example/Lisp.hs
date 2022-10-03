{-# Language DeriveAnyClass #-}
{-# Language LambdaCase #-}
{-# Language NamedFieldPuns #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language RecordWildCards #-}
{-# Language StandaloneDeriving #-}
{-# Language TupleSections #-}

module Text.S.Example.Lisp where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( foldM )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Fixed                     ( mod' )
import qualified Data.Map                      as M
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           System.Console.Haskeline
import           Text.S


-- | S-exp AST
data Sexp = NIL
          | Boolean     Bool
          | Int         Integer
          | Real        Double
          | Symbol      String
          | Keyword     String
          | StringLit   String
          | Quote       Sexp
          | List        [Sexp]
          deriving (Eq, Ord)


-- | Context of Result or Error
type RE = Either String


----------
-- Read
----------
-- | READ
read' :: Text -> RE Sexp
read' s = case parse' sexp s of
  Ok ok (State stream _ _) | isEmpty stream -> pure ok
                           | otherwise      -> err [errRepl, errManySexp]
  Error _ -> err [errRead, errParsing]

-- | S-expression
sexp :: Parser Sexp
sexp = between
  jump
  jump
  (choice [nil, str, bool, real, int, quote, key, sym, vec, form])

-- | skips whitespaces and line/block comments
jump :: Parser ()
jump = skips lispdef

-- | end of identifiers
end :: Parser ()
end = gap <|> void (try (symbol ")"))

-- | nil
nil :: Parser Sexp
nil = NIL <$ symbol "nil" <* end

-- | boolean
bool :: Parser Sexp
bool = Boolean <$> (symbol "t" <* end $> True)

-- | integer
int :: Parser Sexp
int = Int <$> integer <* end

-- | real number
real :: Parser Sexp
real = Real <$> float <* end

-- | symbol
sym :: Parser Sexp
sym = Symbol <$> identifier lispdef

-- | keyword
key :: Parser Sexp
key = Keyword . (":" ++) <$> (symbol ":" *> identifier lispdef)

-- | string literal
str :: Parser Sexp
str = StringLit <$> stringLit

-- | quote
quote :: Parser Sexp
quote = symbol "'" *> (Quote <$> sexp)

-- | vector
vec :: Parser Sexp
vec = List <$> between (symbol "#(") (symbol ")") (many sexp)

-- | form
form :: Parser Sexp
form = List <$> between (symbol "(") (symbol ")") (many sexp)


----------
-- Eval
----------
-- |
eval :: Env -> Sexp -> RE (Env, Sexp)
eval env e = case e of
  Quote  q  -> pure (env, q)
  List   [] -> pure (env, NIL)
  List (s@(Symbol "defparameter") : args) -> apply env s args
  List (s@(Symbol "defvar") : args) -> apply env s args
  List (s@(Symbol "quote") : args) -> apply env s args
  List (s@(Symbol _) : args) -> evalList env args >>= apply env s
  List (l@List{} : args) -> evalList env args >>= apply env l
  List (a : _) -> err [errEval, errInvalidFn, show' a]
  Symbol k  -> (env, ) <$> get k env
  a         -> pure (env, a)

-- |
apply :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
apply env e args = case e of
  Symbol "symbolp"      -> f'symbolp env e args
  Symbol "numberp"      -> f'numberp env e args
  Symbol "stringp"      -> f'stringp env e args
  Symbol "listp"        -> f'listp env e args
  Symbol "defvar"       -> f'defvar env e args
  Symbol "defparameter" -> f'defparameter env e args
  Symbol "list"         -> f'list env e args
  Symbol "quote"        -> f'quote env e args
  Symbol "+"            -> f'add env e args
  Symbol "-"            -> f'sub env e args
  Symbol "*"            -> f'mul env e args
  Symbol "/"            -> f'div env e args
  Symbol "mod"          -> f'mod env e args
  Symbol "expt"         -> f'expt env e args
  Symbol "sqrt"         -> f'sqrt env e args
  Symbol "1+"           -> f'1p env e args
  Symbol "1-"           -> f'1m env e args
  Symbol k              -> err [errEval, errVoidSymbolFn, k]
  _                     -> err [errEval, errNotAllowed]


-- |
evalList :: Env -> [Sexp] -> RE [Sexp]
evalList env = mapM ((snd <$>) . eval env)

-- |
symbolp :: Sexp -> Sexp
symbolp = \case
  Symbol{} -> Boolean True
  _        -> NIL

-- |
numberp :: Sexp -> Sexp
numberp = \case
  Int{}  -> Boolean True
  Real{} -> Boolean True
  _      -> NIL

-- |
stringp :: Sexp -> Sexp
stringp = \case
  StringLit{} -> Boolean True
  _           -> NIL

-- |
listp :: Sexp -> Sexp
listp = \case
  List{} -> Boolean True
  _      -> NIL

-- | symbolp
f'symbolp :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'symbolp env e args = (env, ) . symbolp <$> unary e args

-- | numberp
f'numberp :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'numberp env e args = (env, ) . numberp <$> unary e args

-- | stringp
f'stringp :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'stringp env e args = (env, ) . stringp <$> unary e args

-- | stringp
f'listp :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'listp env e args = (env, ) . listp <$> unary e args

-- | defparameter
f'defparameter :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'defparameter env e args = binary e args >>= \(a, b) -> do
  case (a, b) of
    (s@(Symbol v), a) -> (, s) <$> set'g (v, a) env
    _                 -> err ["Not a symbol"]

-- | defvar
f'defvar :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'defvar env e args = binary e args >>= \(a, b) -> do
  case (a, b) of
    (s@(Symbol v), a) -> (, s) <$> defvar v a
    _                 -> err ["Not a symbol"]
 where
  defvar k a = case M.lookup k (env'g env) of
    Just _  -> pure env
    Nothing -> set'g (k, a) env

-- | let
f'let :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'let env e args = case args of
  (bind@List{} : rest) -> do
    env' <- let'bind bind (local env)
    eval env' (List rest)
  _ -> err ["Malformed let"]

let'bind :: Sexp -> Env -> RE Env
let'bind bind env = case bind of
  List (List [Symbol s, a] : rest) -> set'l (s, a) env >>= let'bind (List rest)
  List [] -> pure env
  _ -> err ["Malformed let-binding"]


-- | list
f'list :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'list env e args = pure (env, List args)

-- | quote
f'quote :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'quote env e args = (env, ) <$> unary e args

-- | (+)
f'add :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'add env e args = (env, ) <$> fold (curry (f'calb (+))) e args

-- | (-)
f'sub :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'sub env e args = (env, ) <$> fold (curry (f'calb (-))) e args

-- | (*)
f'mul :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'mul env e args = (env, ) <$> fold (curry (f'calb (*))) e args

-- | (/)
f'div :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'div env e args = (env, ) <$> fold (curry (f'calb (/))) e args

-- | (%) or mod
f'mod :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'mod env e args = (env, ) <$> (binary e args >>= f'calb mod')

-- | expt
f'expt :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'expt env e args = (env, ) <$> (binary e args >>= f'calb (**))

-- | sqrt
f'sqrt :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'sqrt env e args =
  (env, ) <$> (unary e args >>= (f'calb (*) . (, Real 1)) >>= f'calu sqrt)

-- | (1+)
f'1p :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'1p env e args = (env, ) <$> (unary e args >>= f'calu (+ 1))

-- | (1-)
f'1m :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
f'1m env e args = (env, ) <$> (unary e args >>= f'calu (subtract 1))

-- |
f'calu
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a) -> Sexp -> RE Sexp
f'calu f = \case
  Int  a -> pure . Int . floor . f $ fromIntegral a
  Real a -> pure . Real . f $ a
  _      -> err [errEval, errNotAllowed]

-- |
f'calb
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a -> a)
  -> (Sexp, Sexp)
  -> RE Sexp
f'calb op = \case
  (Int  a, Int b ) -> pure . Int . floor $ fromIntegral a `op` fromIntegral b
  (Int  a, Real b) -> pure . Real $ fromIntegral a `op` b
  (Real a, Int b ) -> pure . Real $ a `op` fromIntegral b
  (Real a, Real b) -> pure . Real $ a `op` b
  _                -> err [errEval, errNotAllowed]

-- | fold list args using the given binary function
fold :: (Sexp -> Sexp -> RE Sexp) -> Sexp -> [Sexp] -> RE Sexp
fold f e args = case args of
  (x : xs) -> foldM f x xs
  []       -> case e of
    Symbol "+" -> pure (Int 0)
    Symbol "*" -> pure (Int 1)
    _          -> err [errEval, errWrongNargs, show' e, show 0]


-- | creates functions to control a function's number of arguments
arity :: (Int -> Bool) -> Sexp -> [Sexp] -> RE [Sexp]
arity pred e args
  | pred nargs = err [errEval, errWrongNargs, show' e ++ ",", show nargs]
  | otherwise  = pure args
  where nargs = length args

-- | guard for arguments of unary functions
unary :: Sexp -> [Sexp] -> RE Sexp
unary e args = head <$> arity (/= 1) e args

-- | guard for arguments of binary functions
binary :: Sexp -> [Sexp] -> RE (Sexp, Sexp)
binary e args = x >>= \a -> y >>= \b -> pure (a, b)
 where
  g = arity (/= 2) e args
  x = head <$> g
  y = head . tail <$> g

-- | guard for arguments of even-ary(pairwise) functions
evenary :: Sexp -> [Sexp] -> RE [Sexp]
evenary = arity even


----------
-- Env
----------
-- |
data Env = Env
  { env'g :: M.Map String Sexp
  , env'l :: M.Map String Sexp
  }
  deriving Show

-- |
init'env :: Env
init'env = Env M.empty M.empty

-- |
get :: String -> Env -> RE Sexp
get k env = case match of
  Just v  -> pure v
  Nothing -> err [errEval, errVoidSymbolVar, k]
  where match = M.lookup k (env'l env) <|> M.lookup k (env'g env)

-- |
set :: (String, Sexp) -> Env -> RE Env
set (k, e) env@Env {..} | M.member k env'l = set'l (k, e) env
                        | otherwise        = set'g (k, e) env

-- |
set'g :: (String, Sexp) -> Env -> RE Env
set'g (k, e) env@Env {..} = pure (env { env'g = M.insert k e env'g })

-- |
set'l :: (String, Sexp) -> Env -> RE Env
set'l (k, e) env@Env {..} = pure (env { env'l = M.insert k e env'l })

-- | when going into local scope
local :: Env -> Env
local env@Env {..} = env { env'g = env'l <> env'g, env'l = M.empty }

-- | when getting out from local scope
global :: Env -> Env -> Env
global l@Env{} g@Env{} = g { env'g = env'g l }


----------
-- Print
----------
-- |
print' :: MonadIO m => Sexp -> InputT m ()
print' = outputStrLn . show'

show' :: Sexp -> String
show' = \case
  NIL               -> "nil"
  Int       intger  -> show intger
  Real      real    -> show real
  Symbol    symbol  -> symbol
  Keyword   keyword -> keyword
  StringLit string  -> "\"" ++ string ++ "\""
  Quote     sexp    -> "'" ++ show' sexp
  Boolean bool | bool      -> "t"
               | otherwise -> "nil"
  List list -> case list of
    [] -> "nil"
    _  -> "(" <> unwords (show' <$> list) <> ")"


deriving instance Pretty Sexp

deriving instance Show Sexp


err :: [String] -> RE a
err = Left . unwords

errEval :: String
errEval = "*** Eval error ***"

errRepl :: String
errRepl = "*** REPL error ***"

errRead :: String
errRead = "*** Read error ***"

errInvalidFn :: String
errInvalidFn = "Invalid function:"

errVoidSymbolFn :: String
errVoidSymbolFn = "Symbol's function definition is void:"

errVoidSymbolVar :: String
errVoidSymbolVar = "Symbol's value as variable is void:"

errWrongNargs :: String
errWrongNargs = "Wrong number of arguments:"

errWrongTargs :: String
errWrongTargs = "Wrong type arguments:"

errManySexp :: String
errManySexp = "More than one sexp in input"

errParsing :: String
errParsing = "Occurred error during parsing"

errNotAllowed :: String
errNotAllowed = "Operation not allowed"


----------
-- REPL
----------
-- | repl for SLISP
sl :: IO ()
sl = runInputT (defaultSettings { historyFile }) (loop init'env normal)
 where
  normal      = (read', print')
  debug       = (read'd, print'd)
  historyFile = Just "/tmp/slisp.hist"
  loop env mode@(reader, printer) = do
    input <- getInputLine "SLISP> "
    case input of
      Nothing    -> pure ()
      Just []    -> loop env normal
      Just ";;;" -> loop env debug
      Just input -> case reader (fromString input) >>= eval env of
        Left  err          -> outputStrLn err >> loop env mode
        Right (env', expr) -> printer expr >> loop env' mode


----------
-- Debug
----------
-- | debug-mode printer
print'd :: MonadIO m => Sexp -> InputT m ()
print'd = outputStrLn . TL.unpack . pretty

-- | debug-mode reader
read'd :: Text -> RE Sexp
read'd s = case parse' sexp s of
  Ok ok (State stream _ _) | isEmpty stream -> pure ok
                           | otherwise      -> err [errRepl, errManySexp]
  Error state -> err [errRead, errParsing, "\n", TL.unpack (pretty state)]


-- Deferred:
-- symbol and function don't share namespaces
-- |symbol name with space| = symbol\ name \with \space
