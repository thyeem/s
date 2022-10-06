{-# Language DeriveAnyClass #-}
{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language RecordWildCards #-}
{-# Language StandaloneDeriving #-}
{-# Language TupleSections #-}

module Text.S.Example.Lisp where

import           Control.Applicative            ( (<|>)
                                                , liftA2
                                                )
import           Control.Monad                  ( foldM )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Fixed                     ( mod' )
import           Data.Functor                   ( (<&>) )
import qualified Data.Map                      as M
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           System.Console.Haskeline
import           System.Directory               ( getHomeDirectory )
import           System.FilePath                ( (</>) )
import           Text.S

-- | S-exp AST
data Sexp = NIL
          | Boolean     Bool
          | Int         Integer
          | Float       Double
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
  (choice [nil, str, bool, flt, int, quote, key, sym, vec, form])

-- | Skip whitespaces and line/block comments
jump :: Parser ()
jump = skips lispdef

-- | End of Identifiers
end :: Parser ()
end = gap <|> void (try (symbol ")"))

-- | NIL
nil :: Parser Sexp
nil = NIL <$ symbol "nil" <* end

-- | Boolean
bool :: Parser Sexp
bool = Boolean <$> (symbol "t" <* end $> True)

-- | Integer
int :: Parser Sexp
int = Int <$> integer <* end

-- | Non-integer
flt :: Parser Sexp
flt = Float <$> float <* end

-- | Symbol
sym :: Parser Sexp
sym = Symbol <$> identifier lispdef

-- | Keyword
key :: Parser Sexp
key = Keyword . (":" ++) <$> (symbol ":" *> identifier lispdef)

-- | String Literal
str :: Parser Sexp
str = StringLit <$> stringLit

-- | Quote
quote :: Parser Sexp
quote = symbol "'" *> (Quote <$> sexp)

-- | Vector
vec :: Parser Sexp
vec = List <$> between (symbol "#(") (symbol ")") (many sexp)

-- | Form
form :: Parser Sexp
form = List <$> between (symbol "(") (symbol ")") (many sexp)


----------
-- Eval
----------
-- | EVAL
eval :: ST Sexp -> RE (ST Sexp)
eval s = get s >>= \case
  Quote  a                 -> put a s
  Symbol k                 -> find k s
  Seq    es                -> put es s >>= evalSeq
  List   []                -> put NIL s
  List   es@(Symbol{} : _) -> put es s >>= apply
  List   (   a        : _) -> err [errEval, errInvalidFn, show' a]
  a                        -> put a s

-- |
apply :: ST [Sexp] -> RE (ST Sexp)
apply s =
  get s
    >>= \case
          Symbol "let*"         -> f'let' s
          -- Symbol "let"          -> f'let s
          -- Symbol "setq"         -> f'setq s
          Symbol "symbolp"      -> f'symbolp s
          Symbol "numberp"      -> f'numberp s
          Symbol "stringp"      -> f'stringp s
          Symbol "listp"        -> f'listp s
          Symbol "symbol-value" -> f'symbolValue s
          Symbol "defvar"       -> f'defvar s
          Symbol "defparameter" -> f'defparameter s
          Symbol "list"         -> f'list s
          Symbol "quote"        -> f'quote s
          Symbol "+"            -> f'add s
          Symbol "-"            -> f'sub s
          Symbol "*"            -> f'mul s
          Symbol "/"            -> f'div s
          Symbol "mod"          -> f'mod s
          Symbol "expt"         -> f'expt s
          Symbol "exp"          -> f'exp s
          Symbol "log"          -> f'log s
          Symbol "sqrt"         -> f'sqrt s
          Symbol "abs"          -> f'abs s
          Symbol "sin"          -> f'sin s
          Symbol "cos"          -> f'cos s
          Symbol "tan"          -> f'tan s
          Symbol "sinh"         -> f'sinh s
          Symbol "cosh"         -> f'cosh s
          Symbol "tanh"         -> f'tanh s
          Symbol "1+"           -> f'1p s
          Symbol "1-"           -> f'1m s
          Symbol "float"        -> f'float s
          Symbol k              -> err [errEval, errVoidSymbolFn, k]
          a                     -> err [errEval, errNotAllowed, "apply"]
    .   head

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
    set'lenv k a s >>= put (List rest) >>= bindSeq
  List [] -> pure s
  _       -> err [errEval, errMalformed, "bindings"]

-- | symbolp
f'symbolp :: ST [Sexp] -> RE (ST Sexp)
f'symbolp s = f'pred s $ \case
  Symbol{} -> Boolean True
  _        -> NIL

-- | numberp
f'numberp :: ST [Sexp] -> RE (ST Sexp)
f'numberp s = f'pred s $ \case
  Int{}   -> Boolean True
  Float{} -> Boolean True
  _       -> NIL

-- | stringp
f'stringp :: ST [Sexp] -> RE (ST Sexp)
f'stringp s = f'pred s $ \case
  StringLit{} -> Boolean True
  _           -> NIL

-- | listp
f'listp :: ST [Sexp] -> RE (ST Sexp)
f'listp s = f'pred s $ \case
  NIL    -> Boolean True
  List{} -> Boolean True
  _      -> NIL

-- | predicate builder
f'pred :: ST [Sexp] -> (Sexp -> Sexp) -> RE (ST Sexp)
f'pred s p = g'unary s >>= eval >>= modify (pure . p)

-- | list
f'list :: ST [Sexp] -> RE (ST Sexp)
f'list s = g'args s >>= evalList >>= modify (pure . List)

-- | quote
f'quote :: ST [Sexp] -> RE (ST Sexp)
f'quote = g'unary

-- | float
f'float :: ST [Sexp] -> RE (ST Sexp)
f'float s = g'unary s >>= g'float

-- | (+)
f'add :: ST [Sexp] -> RE (ST Sexp)
f'add s = g'args s >>= evalList >>= mapM' g'number >>= fold (f'calb (+)) "+"

-- | (-)
f'sub :: ST [Sexp] -> RE (ST Sexp)
f'sub s = g'args s >>= evalList >>= mapM' g'number >>= fold (f'calb (-)) "-"

-- | (*)
f'mul :: ST [Sexp] -> RE (ST Sexp)
f'mul s = g'args s >>= evalList >>= mapM' g'number >>= fold (f'calb (*)) "*"

-- | (/)
f'div :: ST [Sexp] -> RE (ST Sexp)
f'div s = g'args s >>= evalList >>= mapM' g'number >>= fold (f'calb (/)) "/"

-- | (%) or mod
f'mod :: ST [Sexp] -> RE (ST Sexp)
f'mod s = g'binary s >>= evalList >>= mapM' g'number >>= g'tuple >>= modify
  (uncurry (f'calb mod'))

-- | expt
f'expt :: ST [Sexp] -> RE (ST Sexp)
f'expt s = g'binary s >>= evalList >>= mapM' g'number >>= g'tuple >>= modify
  (uncurry (f'calb (**)))

-- | exp
f'exp :: ST [Sexp] -> RE (ST Sexp)
f'exp s = g'unary s >>= eval >>= g'float >>= modify (f'calu exp)

-- | log
f'log :: ST [Sexp] -> RE (ST Sexp)
f'log s = g'unary s >>= eval >>= g'float >>= modify (f'calu log)

-- | sqrt
f'sqrt :: ST [Sexp] -> RE (ST Sexp)
f'sqrt s = g'unary s >>= eval >>= g'float >>= modify (f'calu sqrt)

-- | sin
f'sin :: ST [Sexp] -> RE (ST Sexp)
f'sin s = g'unary s >>= eval >>= g'float >>= modify (f'calu sin)

-- | cos
f'cos :: ST [Sexp] -> RE (ST Sexp)
f'cos s = g'unary s >>= eval >>= g'float >>= modify (f'calu cos)

-- | tan
f'tan :: ST [Sexp] -> RE (ST Sexp)
f'tan s = g'unary s >>= eval >>= g'float >>= modify (f'calu tan)

-- | sinh
f'sinh :: ST [Sexp] -> RE (ST Sexp)
f'sinh s = g'unary s >>= eval >>= g'float >>= modify (f'calu sinh)

-- | cosh
f'cosh :: ST [Sexp] -> RE (ST Sexp)
f'cosh s = g'unary s >>= eval >>= g'float >>= modify (f'calu cosh)

-- | tanh
f'tanh :: ST [Sexp] -> RE (ST Sexp)
f'tanh s = g'unary s >>= eval >>= g'float >>= modify (f'calu tanh)

-- | abs
f'abs :: ST [Sexp] -> RE (ST Sexp)
f'abs s = g'unary s >>= eval >>= modify (f'calu abs)

-- | (1+)
f'1p :: ST [Sexp] -> RE (ST Sexp)
f'1p s = g'unary s >>= eval >>= modify (f'calu (+ 1))

-- | (1-)
f'1m :: ST [Sexp] -> RE (ST Sexp)
f'1m s = g'unary s >>= eval >>= modify (f'calu (subtract 1))

-- | Unary arithmetic operator builder
f'calu
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a) -> Sexp -> RE Sexp
f'calu f = \case
  Int   a -> pure . Int . floor . f $ fromIntegral a
  Float a -> pure . Float . f $ a
  a       -> err [errEval, errNotAllowed, "f'calu"]

-- | Binary arithmetic operator builder
f'calb
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a -> a)
  -> Sexp
  -> Sexp
  -> RE Sexp
f'calb op x y = case (x, y) of
  (Int   a, Int b  ) -> pure . Int . floor $ fromIntegral a `op` fromIntegral b
  (Int   a, Float b) -> pure . Float $ fromIntegral a `op` b
  (Float a, Int b  ) -> pure . Float $ a `op` fromIntegral b
  (Float a, Float b) -> pure . Float $ a `op` b
  _                  -> err [errEval, errNotAllowed, "f'calb"]

-- | Fold arguments of a S-exp list using the given binary function
fold :: (Sexp -> Sexp -> RE Sexp) -> String -> ST [Sexp] -> RE (ST Sexp)
fold f o s = get s >>= \case
  [] -> case o of
    "+" -> put (acc o) s
    "*" -> put (acc o) s
    _   -> err [errEval, errNoArgs, o]
  xs -> put xs s >>= modify (foldM f (acc o))
 where
  acc = \case
    "*" -> Int 1
    "/" -> Int 1
    _   -> Int 0

-- | symbol-value
f'symbolValue :: ST [Sexp] -> RE (ST Sexp)
f'symbolValue s = g'unary s >>= eval >>= g'symbol >>= eval

-- | defparameter
f'defparameter :: ST [Sexp] -> RE (ST Sexp)
f'defparameter s = g'binary s >>= get >>= \case
  [e@(Symbol k), a] -> put a s >>= eval >>= \(env, a') -> set'env k a' (env, e)
  x                 -> err [errEval, errNotSymbol, show' . head $ x]

-- | defvar
f'defvar :: ST [Sexp] -> RE (ST Sexp)
f'defvar s = g'binary s >>= get >>= \case
  [e@(Symbol k), a] ->
    put a s >>= eval >>= \(env, a') -> set'env' k a' (env, e)
  x -> err [errEval, errNotSymbol, show' . head $ x]

-- | let*
f'let' :: ST [Sexp] -> RE (ST Sexp)
f'let' s = g'args s >>= get >>= \case
  (l@List{} : rest)
    | null rest
    -> put NIL s
    | otherwise
    -> put l s >>= local >>= bindSeq >>= put (Seq rest) >>= eval >>= global s
  _ -> err [errEval, errMalformed, "let*"]

-- | Build functions to control function's number of arguments
arity :: (Int -> Bool) -> ST [Sexp] -> RE (ST [Sexp])
arity p s@(_, e : args)
  | not (p nargs) = err [errEval, errWrongNargs, show' e ++ ",", show nargs]
  | otherwise     = put args s
  where nargs = length args
arity _ a = err [errEval, errNotAllowed, "arity"]

-- | Get arguments only
g'args :: ST [Sexp] -> RE (ST [Sexp])
g'args = arity (>= 0)

-- | Guard for unary function's arguments
g'unary :: ST [Sexp] -> RE (ST Sexp)
g'unary s = arity (== 1) s >>= modify (pure . head)

-- | Guard for binary function's arguments
g'binary :: ST [Sexp] -> RE (ST [Sexp])
g'binary = arity (== 2)

-- | Guard for even-ary(pairwise) function's arguments
g'evenary :: ST [Sexp] -> RE (ST [Sexp])
g'evenary = arity even

-- | Guard for tuple state: transforms the result value into a tuple
g'tuple :: ST [Sexp] -> RE (ST (Sexp, Sexp))
g'tuple = \case
  s@(_, [x, y]) -> put (x, y) s
  _             -> err [""]

-- | Guard for symbol argument
g'symbol :: ST Sexp -> RE (ST Sexp)
g'symbol s@(_, e) = case e of
  Symbol{} -> pure s
  a        -> err [errEval, errNotSymbol, show' a]

-- | Guard for number argument
g'number :: ST Sexp -> RE (ST Sexp)
g'number s@(_, e) = case e of
  Int{}   -> pure s
  Float{} -> pure s
  a       -> err [errEval, errNotNumber, show' a]

-- |
g'string :: ST Sexp -> RE (ST Sexp)
g'string s@(_, e) = case e of
  StringLit{} -> pure s
  a           -> err [errEval, errNotString, show' a]

-- |
g'list :: ST Sexp -> RE (ST Sexp)
g'list s@(_, e) = case e of
  List{} -> pure s
  a      -> err [errEval, errNotList, show' a]

-- | Ensure that the state is an integer S-exp
g'int :: ST Sexp -> RE (ST Sexp)
g'int s = g'number s >>= get >>= \case
  i@Int{} -> put i s
  a       -> err [errEval, errNotInteger, show' a]

-- | Ensure that the state is a float S-exp
g'float :: ST Sexp -> RE (ST Sexp)
g'float s = g'number s >>= get >>= \case
  Int i     -> put (Float . fromIntegral $ i) s
  r@Float{} -> put r s
  a         -> err [errEval, errNotFloat, show' a]


----------
-- State
----------
-- | Definition of EVAL-state
-- During the evaluation process, each evaluation step has one of
-- these states and eventually collapsed into a S-exp value
type ST a = (Env, a)

-- | Get the result value from the state
get :: ST a -> RE a
get = pure . snd

-- | Set the given reuslt value to the state
put :: a -> ST b -> RE (ST a)
put x (env, _) = pure (env, x)

-- | Transform the old state to a new state with the given functions
modify :: (a -> RE b) -> ST a -> RE (ST b)
modify f (env, e) = f e <&> (env, )

-- | Get the env from the state
get' :: ST a -> RE Env
get' = pure . fst

-- | Set the given env to the state
put' :: Env -> ST a -> RE (ST a)
put' env (_, x) = pure (env, x)

-- | Map the state result to an action.
-- This map is only valid when the result has multiple value, i.e., a list
mapM' :: (ST a -> RE (ST a)) -> ST [a] -> RE (ST [a])
mapM' f s@(env, es) = mapM f (sequence s) >>= put' env . sequence'
 where
  sequence' xs = (init'env, go [] xs)
  go r = \case
    []              -> reverse r
    ((_, e) : rest) -> go (e : r) rest


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
init'env = Env mempty mempty

-- |
set'env :: String -> Sexp -> ST a -> RE (ST a)
set'env k e s@(env@Env {..}, _) | M.member k env'l = set'lenv k e s
                                | otherwise        = set'genv k e s

-- | The same as `set'env`, but putting value only when no key
set'env' :: String -> Sexp -> ST a -> RE (ST a)
set'env' k a s@(env, _) = case M.lookup k (env'g env) of
  Just _  -> pure s
  Nothing -> set'env k a s

-- |
set'genv :: String -> Sexp -> ST a -> RE (ST a)
set'genv k e s@(env@Env {..}, _) = put' (env { env'g = M.insert k e env'g }) s

-- |
set'lenv :: String -> Sexp -> ST a -> RE (ST a)
set'lenv k e s@(env@Env {..}, _) = put' (env { env'l = M.insert k e env'l }) s

-- | When going into local-scope
local :: ST a -> RE (ST a)
local s@(env@Env {..}, _) =
  put' (env { env'g = env'l <> env'g, env'l = mempty }) s

-- | When getting out from local-scope
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

-- | Stringify S-expression
show' :: Sexp -> String
show' = \case
  NIL               -> "nil"
  Int       intger  -> show intger
  Float     float   -> show float
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

errNoArgs :: String
errNoArgs = "No arguments:"

errWrongTargs :: String
errWrongTargs = "Wrong type arguments:"

errNotSymbol :: String
errNotSymbol = "Not a symbol:"

errNotNumber :: String
errNotNumber = "Not a number:"

errNotString :: String
errNotString = "Not a string:"

errNotInteger :: String
errNotInteger = "Not an integer:"

errNotFloat :: String
errNotFloat = "Not a float:"

errNotList :: String
errNotList = "Not a list:"

errMalformed :: String
errMalformed = "Malformed:"

errManySexp :: String
errManySexp = "More than one sexp in input"

errParsing :: String
errParsing = "Occurred error during parsing"

errNotAllowed :: String
errNotAllowed = "Operation not allowed:"


----------
-- REPL
----------
-- | REPL for SLISP
sl :: IO ()
sl = do
  historyFile <- getHomeDirectory <&> (</> ".slisp")
  runInputT (defaultSettings { historyFile = Just historyFile })
            (loop init'env normal)
 where
  normal = (read', print')
  debug  = (read'd, print'd)
  loop env mode@(reader, printer) = do
    s <- getInputLine "SLISP> "
    case s of
      Nothing   -> pure ()
      Just []   -> loop env normal
      Just ";;" -> loop env debug
      -- Just ";" -> loop env multiline
      Just s    -> case reader (fromString s) >>= curry eval env of
        Left  err       -> outputStrLn err >> loop env mode
        Right (env', e) -> printer e >> loop env' mode


----------
-- Debug
----------
-- | Debug-mode printer
print'd :: MonadIO m => Sexp -> InputT m ()
print'd = outputStrLn . TL.unpack . pretty

-- | Debug-mode reader
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
