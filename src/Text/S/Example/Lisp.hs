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
import           Control.Monad                  ( (>=>)
                                                , foldM
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Fixed                     ( mod' )
import           Data.Functor                   ( (<&>) )
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
          | Seq         [Sexp]
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
-- | EVAL
eval :: ST Sexp -> RE (ST Sexp)
eval s@(env, e) = case e of
  Quote  a                 -> put a s
  Symbol k                 -> find k s
  Seq    es                -> put es s >>= evalSeq
  List   []                -> put NIL s
  List   es@(Symbol{} : _) -> apply env es
  List   (   a        : _) -> err [errEval, errInvalidFn, show' a]
  a                        -> put a s

-- |
apply :: Env -> [Sexp] -> RE (ST Sexp)
apply env es@(e : _) = case e of
  Symbol "let*"         -> f'letq (env, es)
  Symbol "symbolp"      -> f'symbolp (env, es)
  Symbol "numberp"      -> f'numberp (env, es)
  Symbol "stringp"      -> f'stringp (env, es)
  Symbol "listp"        -> f'listp (env, es)
  Symbol "defvar"       -> f'defvar (env, es)
  Symbol "defparameter" -> f'defparameter (env, es)
  Symbol "list"         -> f'list (env, es)
  Symbol "quote"        -> f'quote (env, es)
  Symbol "+"            -> f'add (env, es)
  Symbol "-"            -> f'sub (env, es)
  Symbol "*"            -> f'mul (env, es)
  Symbol "/"            -> f'div (env, es)
  Symbol "mod"          -> f'mod (env, es)
  Symbol "expt"         -> f'expt (env, es)
  Symbol "sqrt"         -> f'sqrt (env, es)
  Symbol "1+"           -> f'1p (env, es)
  Symbol "1-"           -> f'1m (env, es)
  Symbol k              -> err [errEval, errVoidSymbolFn, k]
  _                     -> err [errEval, errNotAllowed]
apply _ _ = err [errEval, errNotAllowed]


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
  NIL    -> Boolean True
  List{} -> Boolean True
  _      -> NIL

-- | symbolp
f'symbolp :: ST [Sexp] -> RE (ST Sexp)
f'symbolp s = unary s >>= modify (pure . symbolp)

-- | numberp
f'numberp :: ST [Sexp] -> RE (ST Sexp)
f'numberp s = unary s >>= eval >>= modify (pure . numberp)

-- | stringp
f'stringp :: ST [Sexp] -> RE (ST Sexp)
f'stringp s = unary s >>= eval >>= modify (pure . stringp)

-- | stringp
f'listp :: ST [Sexp] -> RE (ST Sexp)
f'listp s = unary s >>= eval >>= modify (pure . listp)

-- | defparameter
f'defparameter :: ST [Sexp] -> RE (ST Sexp)
f'defparameter s = binary s >>= get >>= \case
  (e@(Symbol k), a) ->
    put a s >>= eval >>= \(env, a') -> set'env (k, a') (env, e)
  (x, _) -> err [errEval, errNotSymbol, show' x]

-- | defvar
f'defvar :: ST [Sexp] -> RE (ST Sexp)
f'defvar s = binary s >>= get >>= \case
  (e@(Symbol k), a) ->
    put a s >>= eval >>= \(env, a') -> set'env' (k, a') (env, e)
  (x, _) -> err [errEval, errNotSymbol, show' x]

-- | let
f'letq :: ST [Sexp] -> RE (ST Sexp)
f'letq s@(_, _ : args) = case args of
  (l@List{} : rest)
    | null rest
    -> put NIL s
    | otherwise
    -> put l s >>= local >>= bindSeq >>= put (Seq rest) >>= eval >>= global s
  _ -> err [errEval, errMalformed, "let*"]
f'letq _ = err [errEval, errNotAllowed]

-- |
evalSeq :: ST [Sexp] -> RE (ST Sexp)
evalSeq s@(_, es) = case es of
  [e       ] -> put e s >>= eval
  (e : rest) -> put e s >>= eval >>= put rest >>= evalSeq
  _          -> err [errEval, errMalformed, "sequence"]

-- |
evalList :: ST [Sexp] -> RE (ST [Sexp])
evalList = go []
 where
  go r s@(env, es) = case es of
    (e : rest) -> put e s >>= eval >>= \(env', e') -> go (e' : r) (env', rest)
    []         -> put (reverse r) s

-- |
bindSeq :: ST Sexp -> RE (ST Sexp)
bindSeq s@(env, e) = case e of
  List (List [Symbol k, a] : rest) ->
    set'lenv (k, a) s >>= put (List rest) >>= bindSeq
  List [] -> pure s
  _       -> err [errEval, errMalformed, "bindings"]

-- | list
f'list :: ST [Sexp] -> RE (ST Sexp)
f'list s@(_, _ : args) = put args s >>= evalList >>= modify (pure . List)
f'list _               = err [errEval, errNotAllowed]

-- | quote
f'quote :: ST [Sexp] -> RE (ST Sexp)
f'quote = unary

-- | (+)
f'add :: ST [Sexp] -> RE (ST Sexp)
f'add = fold (f'calb (+))

-- | (-)
f'sub :: ST [Sexp] -> RE (ST Sexp)
f'sub = fold (f'calb (-))

-- | (*)
f'mul :: ST [Sexp] -> RE (ST Sexp)
f'mul = fold (f'calb (*))

-- | (/)
f'div :: ST [Sexp] -> RE (ST Sexp)
f'div = fold (f'calb (/))

-- | (%) or mod
f'mod :: ST [Sexp] -> RE (ST Sexp)
f'mod s = binary s >>= modify (uncurry (f'calb mod'))

-- | expt
f'expt :: ST [Sexp] -> RE (ST Sexp)
f'expt s = binary s >>= modify (uncurry (f'calb (**)))

-- | sqrt
f'sqrt :: ST [Sexp] -> RE (ST Sexp)
f'sqrt s = unary s >>= modify (f'calb (*) (Real 1) >=> f'calu sqrt)

-- | (1+)
f'1p :: ST [Sexp] -> RE (ST Sexp)
f'1p s = unary s >>= eval >>= modify (f'calu (+ 1))

-- | (1-)
f'1m :: ST [Sexp] -> RE (ST Sexp)
f'1m s = unary s >>= eval >>= modify (f'calu (subtract 1))

-- | unary arithmetic operator builder
f'calu
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a) -> Sexp -> RE Sexp
f'calu f = \case
  Int  a -> pure . Int . floor . f $ fromIntegral a
  Real a -> pure . Real . f $ a
  _      -> err [errEval, errNotAllowed]

-- | binary arithmetic operator builder
f'calb
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a -> a)
  -> Sexp
  -> Sexp
  -> RE Sexp
f'calb op x y = case (x, y) of
  (Int  a, Int b ) -> pure . Int . floor $ fromIntegral a `op` fromIntegral b
  (Int  a, Real b) -> pure . Real $ fromIntegral a `op` b
  (Real a, Int b ) -> pure . Real $ a `op` fromIntegral b
  (Real a, Real b) -> pure . Real $ a `op` b
  _                -> err [errEval, errNotAllowed]

-- | fold list args using the given binary function
fold :: (Sexp -> Sexp -> RE Sexp) -> ST [Sexp] -> RE (ST Sexp)
fold f s@(_, e : args) = case args of
  (_ : _) -> put args s >>= evalList >>= \q@(_, es) ->
    foldM f (head es) (tail es) >>= flip put q
  [] -> case e of
    Symbol "+" -> put (Int 0) s
    Symbol "*" -> put (Int 1) s
    _          -> err [errEval, errWrongNargs, show' e, show 0]
fold _ _ = err [errEval, errNotAllowed]


-- | creates functions to control function's number of arguments
arity :: (Int -> Bool) -> ST [Sexp] -> RE (ST [Sexp])
arity p (env, e : args)
  | not (p nargs) = err [errEval, errWrongNargs, show' e ++ ",", show nargs]
  | otherwise     = pure (env, args)
  where nargs = length args
arity _ _ = err [errEval, errNotAllowed]

-- | guard for arguments of unary functions
unary :: ST [Sexp] -> RE (ST Sexp)
unary s = arity (== 1) s >>= \(env, e) -> pure (env, head e)

-- | guard for arguments of binary functions
binary :: ST [Sexp] -> RE (ST (Sexp, Sexp))
binary s = g >>= \(env, es) -> pure (env, (head es, head . tail $ es))
  where g = arity (== 2) s

-- | guard for arguments of even-ary(pairwise) functions
evenary :: ST [Sexp] -> RE (ST [Sexp])
evenary = arity even


----------
-- State
----------
-- | definition of EVAL-state
-- during the evaluation process, each evaluation step has one of
-- these states and eventually collapsed into a S-exp value
type ST a = (Env, a)

-- | get the result value from the state
get :: ST a -> RE a
get = pure . snd

-- | set the given reuslt value to the state
put :: a -> ST b -> RE (ST a)
put x (env, _) = pure (env, x)

-- | transforms the old state to a new state with the given functions
modify :: (a -> RE b) -> ST a -> RE (ST b)
modify f (env, e) = f e <&> (env, )

-- | get the env from the state
get' :: ST a -> RE Env
get' = pure . fst

-- | set the given env to the state
put' :: Env -> ST a -> RE (ST a)
put' env (_, x) = pure (env, x)


----------
-- Env
----------
-- | SLISP environment
data Env = Env
  { env'g :: M.Map String Sexp
  , env'l :: M.Map String Sexp
  }
  deriving Show

-- |
init'env :: Env
init'env = Env M.empty M.empty

-- |
set'env :: (String, Sexp) -> ST a -> RE (ST a)
set'env (k, e) s@(env@Env {..}, _) | M.member k env'l = set'lenv (k, e) s
                                   | otherwise        = set'genv (k, e) s

-- | the same as `set'env`, but putting value only when no key
set'env' :: (String, Sexp) -> ST a -> RE (ST a)
set'env' (k, a) s@(env, _) = case M.lookup k (env'g env) of
  Just _  -> pure s
  Nothing -> set'env (k, a) s

-- |
set'genv :: (String, Sexp) -> ST a -> RE (ST a)
set'genv (k, e) s@(env@Env {..}, _) =
  put' (env { env'g = M.insert k e env'g }) s

-- |
set'lenv :: (String, Sexp) -> ST a -> RE (ST a)
set'lenv (k, e) s@(env@Env {..}, _) =
  put' (env { env'l = M.insert k e env'l }) s

-- | when going into local-scope
local :: ST a -> RE (ST a)
local s@(env@Env {..}, _) =
  put' (env { env'g = env'l <> env'g, env'l = M.empty }) s

-- | when getting out from local-scope
global :: ST b -> ST a -> RE (ST a)
global (g@Env{}, _) s@(l@Env{}, a) = put' (g { env'g = env'g l }) s

-- | find stored S-exp values with symbol keys
find :: String -> ST Sexp -> RE (ST Sexp)
find k s@(env, _) = case match of
  Just v  -> put v s
  Nothing -> err [errEval, errVoidSymbolVar, k]
  where match = M.lookup k (env'l env) <|> M.lookup k (env'g env)


----------
-- Print
----------
-- | PRINT
print' :: MonadIO m => Sexp -> InputT m ()
print' = outputStrLn . show'

-- | stringifies S-expression
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
  Seq  seq  -> show' (List seq)
  List list -> case list of
    [] -> "nil"
    _  -> "(" ++ unwords (show' <$> list) ++ ")"


deriving instance Show Sexp

deriving instance Pretty Sexp


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

errNotSymbol :: String
errNotSymbol = "Not a symbol:"

errMalformed :: String
errMalformed = "Malformed:"

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
        Left  err       -> outputStrLn err >> loop env mode
        Right (env', e) -> printer e >> loop env' mode


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
-- multiline REPL input (completion available)
-- case-insensitive
