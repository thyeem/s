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
eval :: ST Sexp -> RE (ST Sexp)
eval (env, e) = case e of
  Quote  q  -> pure (env, q)
  List   [] -> pure (env, NIL)
  List es@(Symbol "defparameter" : _) -> apply env es
  List es@(Symbol "defvar" : _) -> apply env es
  List es@(Symbol "quote" : _) -> apply env es
  List (e@Symbol{} : args) -> evalList env args >>= apply env . (e :)
  List (List{} : args) -> evalList env args >>= apply env
  List (a : _) -> err [errEval, errInvalidFn, show' a]
  Symbol k  -> (env, ) <$> get k env
  a         -> pure (env, a)

-- |
apply :: Env -> [Sexp] -> RE (ST Sexp)
apply env es@(e : _) = case e of
  Symbol "symbolp"      -> f'symbolp env es
  Symbol "numberp"      -> f'numberp env es
  Symbol "stringp"      -> f'stringp env es
  Symbol "listp"        -> f'listp env es
  Symbol "defvar"       -> f'defvar env es
  Symbol "defparameter" -> f'defparameter env es
  Symbol "list"         -> f'list env es
  Symbol "quote"        -> f'quote env es
  Symbol "+"            -> f'add env es
  Symbol "-"            -> f'sub env es
  Symbol "*"            -> f'mul env es
  Symbol "/"            -> f'div env es
  Symbol "mod"          -> f'mod env es
  Symbol "expt"         -> f'expt env es
  Symbol "sqrt"         -> f'sqrt env es
  Symbol "1+"           -> f'1p env es
  Symbol "1-"           -> f'1m env es
  Symbol k              -> err [errEval, errVoidSymbolFn, k]
  _                     -> err [errEval, errNotAllowed]
apply _ _ = err [errEval, errNotAllowed]


-- |
evalList :: Env -> [Sexp] -> RE [Sexp]
evalList env = mapM ((snd <$>) . curry eval env)

-- | predicate for symbol
symbolp :: Sexp -> Sexp
symbolp = \case
  Symbol{} -> Boolean True
  _        -> NIL

-- | predicate for number
numberp :: Sexp -> Sexp
numberp = \case
  Int{}  -> Boolean True
  Real{} -> Boolean True
  _      -> NIL

-- | predicate for string literal
stringp :: Sexp -> Sexp
stringp = \case
  StringLit{} -> Boolean True
  _           -> NIL

-- | predicate for string list
listp :: Sexp -> Sexp
listp = \case
  List{} -> Boolean True
  _      -> NIL

-- | symbolp
f'symbolp :: Env -> [Sexp] -> RE (ST Sexp)
f'symbolp env es = (env, ) . symbolp <$> unary es

-- | numberp
f'numberp :: Env -> [Sexp] -> RE (ST Sexp)
f'numberp env es = (env, ) . numberp <$> unary es

-- | stringp
f'stringp :: Env -> [Sexp] -> RE (ST Sexp)
f'stringp env es = (env, ) . stringp <$> unary es

-- | stringp
f'listp :: Env -> [Sexp] -> RE (ST Sexp)
f'listp env es = (env, ) . listp <$> unary es

-- | defparameter
f'defparameter :: Env -> [Sexp] -> RE (ST Sexp)
f'defparameter env es = binary es >>= \(a, b) -> do
  case (a, b) of
    (s@(Symbol v), a) -> (, s) <$> set'g (v, a) env
    _                 -> err ["Not a symbol"]

-- | defvar
f'defvar :: Env -> [Sexp] -> RE (ST Sexp)
f'defvar env es = binary es >>= \(a, b) -> do
  case (a, b) of
    (s@(Symbol v), a) -> (, s) <$> defvar v a
    _                 -> err ["Not a symbol"]
 where
  defvar k a = case M.lookup k (env'g env) of
    Just _  -> pure env
    Nothing -> set'g (k, a) env

-- | let
f'let :: Env -> [Sexp] -> RE (ST Sexp)
f'let env (e : args) = case args of
  (bind@List{} : rest) -> let'bind (local env) bind >>= eval . (, List rest)
  _                    -> err ["Malformed let"]
f'let _ _ = err [errEval, errNotAllowed]

let'bind :: Env -> Sexp -> RE Env
let'bind env bind = case bind of
  List (List [Symbol s, a] : rest) ->
    set'l (s, a) env >>= flip let'bind (List rest)
  List [] -> pure env
  _       -> err ["Malformed let-binding"]


-- | list
f'list :: Env -> [Sexp] -> RE (ST Sexp)
f'list env (_ : args) = pure (env, List args)
f'list _   _          = err [errEval, errNotAllowed]

-- | quote
f'quote :: Env -> [Sexp] -> RE (ST Sexp)
f'quote env es = (env, ) <$> unary es

-- | (+)
f'add :: Env -> [Sexp] -> RE (ST Sexp)
f'add env es = (env, ) <$> fold (curry (f'calb (+))) es

-- | (-)
f'sub :: Env -> [Sexp] -> RE (ST Sexp)
f'sub env es = (env, ) <$> fold (curry (f'calb (-))) es

-- | (*)
f'mul :: Env -> [Sexp] -> RE (ST Sexp)
f'mul env es = (env, ) <$> fold (curry (f'calb (*))) es

-- | (/)
f'div :: Env -> [Sexp] -> RE (ST Sexp)
f'div env es = (env, ) <$> fold (curry (f'calb (/))) es

-- | (%) or mod
f'mod :: Env -> [Sexp] -> RE (ST Sexp)
f'mod env es = (env, ) <$> (binary es >>= f'calb mod')

-- | expt
f'expt :: Env -> [Sexp] -> RE (ST Sexp)
f'expt env es = (env, ) <$> (binary es >>= f'calb (**))

-- | sqrt
f'sqrt :: Env -> [Sexp] -> RE (ST Sexp)
f'sqrt env es =
  (env, ) <$> (unary es >>= (f'calb (*) . (, Real 1)) >>= f'calu sqrt)

-- | (1+)
f'1p :: Env -> [Sexp] -> RE (ST Sexp)
f'1p env es = (env, ) <$> (unary es >>= f'calu (+ 1))

-- | (1-)
f'1m :: Env -> [Sexp] -> RE (ST Sexp)
f'1m env es = (env, ) <$> (unary es >>= f'calu (subtract 1))

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
fold :: (Sexp -> Sexp -> RE Sexp) -> [Sexp] -> RE Sexp
fold f (e : args) = case args of
  (x : xs) -> foldM f x xs
  []       -> case e of
    Symbol "+" -> pure (Int 0)
    Symbol "*" -> pure (Int 1)
    _          -> err [errEval, errWrongNargs, show' e, show 0]
fold _ _ = err [errEval, errNotAllowed]


-- | creates functions to control a function's number of arguments
arity :: (Int -> Bool) -> [Sexp] -> RE [Sexp]
arity pred (e : args)
  | pred nargs = err [errEval, errWrongNargs, show' e ++ ",", show nargs]
  | otherwise  = pure args
  where nargs = length args
arity _ _ = err [errEval, errNotAllowed]

-- | guard for arguments of unary functions
unary :: [Sexp] -> RE Sexp
unary es = head <$> arity (/= 1) es

-- | guard for arguments of binary functions
binary :: [Sexp] -> RE (Sexp, Sexp)
binary es@(e : args) = x >>= \a -> y >>= \b -> pure (a, b)
 where
  g = arity (/= 2) es
  x = head <$> g
  y = head . tail <$> g
binary _ = err [errEval, errNotAllowed]

-- | guard for arguments of even-ary(pairwise) functions
evenary :: [Sexp] -> RE [Sexp]
evenary = arity even


----------
-- State
----------
-- |
type ST a = (Env, a)

get's :: ST a -> a
get's = snd

get'e :: ST a -> Env
get'e = fst

put's :: a -> ST a -> ST a
put's e' (env, e) = (env, e')

put'e :: Env -> ST a -> ST a
put'e env' (env, e) = (env', e)

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
      Just input -> case reader (fromString input) >>= curry eval env of
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
