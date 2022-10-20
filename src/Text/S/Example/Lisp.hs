{-# Language DeriveAnyClass #-}
{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language RecordWildCards #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeApplications #-}
{-# Language TupleSections #-}

module Text.S.Example.Lisp where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( (>=>)
                                                , foldM
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Fixed                     ( mod' )
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                )
import qualified Data.Map                      as M
import qualified Data.Text.Lazy                as TL
import qualified Data.Vector                   as V
import           System.Console.Haskeline       ( InputT
                                                , Settings(historyFile)
                                                , defaultSettings
                                                , getInputLine
                                                , outputStrLn
                                                , runInputT
                                                )
import           System.Directory               ( getHomeDirectory )
import           System.FilePath                ( (</>) )
import           System.Random                  ( mkStdGen
                                                , randomR
                                                )
import           Text.S                         ( ParserS
                                                , Pretty(pretty)
                                                , Result(Error, Ok)
                                                , State(State)
                                                , Stream(isEmpty)
                                                , between
                                                , choice
                                                , floatB
                                                , gap
                                                , identifier
                                                , integer
                                                , lispdef
                                                , many
                                                , option
                                                , parse'
                                                , skips
                                                , spaces
                                                , stringLit
                                                , symbol
                                                , try
                                                , void
                                                )

-- | S-exp AST
data Sexp = NIL
          | Bool        Bool
          | Int         Integer
          | Float       Double
          | Symbol      String
          | Keyword     String
          | String      String
          | Quote       Sexp
          | Backquote   Sexp
          | Comma       Sexp
          | At          Sexp
          | Cons        Sexp Sexp
          | Seq         [Sexp]
          | List        [Sexp]
          | Vector      (V.Vector Sexp)
          | HashTable   (M.Map String Sexp)
          | Function    String Fn
          | Macro       String Fn


----------
-- State
----------
-- | Definition of EVAL-state
-- During the evaluation process, each evaluation step has one of
-- these states and eventually collapsed into a S-exp value
type ST a = (Env, a)


-- | Context of Result or Error
type RE = Either String


-- | Type to decribe a state transform from 'a' to 'b'
type T a b = ST a -> RE (ST b)


-- | Get the result value from the state
get :: ST a -> RE a
get = pure . snd

-- | Set the given result value to the state
put :: a -> T b a
put x (env, _) = pure (env, x)

-- | Transform the old state to a new state with the given functions
modify :: (a -> RE b) -> T a b
modify f (env, x) = f x <&> (env, )

-- | Get the env from the state
get' :: ST a -> RE Env
get' = pure . fst

-- | Set the given env to the state
put' :: Env -> T a a
put' env (_, x) = pure (env, x)


----------
-- Env
----------
-- | SLISP environment
data Env = Env
  { env'g :: M.Map String Sexp
  , env'l :: M.Map String Sexp
  , env'f :: M.Map String Fn
  }
  deriving Show

instance Semigroup Env where
  (<>) = undefined

instance Monoid Env where
  mempty = Env mempty mempty mempty

-- |
null'env :: ST Sexp
null'env = (mempty, NIL)

-- |
init'env :: ST Sexp
init'env = (Env mempty mempty (M.fromList built'in), NIL)

-- |
set'venv :: String -> T Sexp Sexp
set'venv k s@(Env {..}, _) | M.member k env'l = set'lenv k s
                           | otherwise        = set'genv k s
-- |
set'genv :: String -> T Sexp Sexp
set'genv k s@(env@Env {..}, x) = put' (env { env'g = M.insert k x env'g }) s

-- |
set'lenv :: String -> T Sexp Sexp
set'lenv k s@(env@Env {..}, x) = put' (env { env'l = M.insert k x env'l }) s

-- |
set'fenv :: String -> T Fn Fn
set'fenv k s@(env@Env {..}, x) = put' (env { env'f = M.insert k x env'f }) s

-- | The same as `set'venv`, but set only when keys are not defined
set'venv'ifndef :: String -> T Sexp Sexp
set'venv'ifndef k s@(Env {..}, _) = case M.lookup k env'g of
  Just _  -> pure s
  Nothing -> set'venv k s

-- | Get S-exp from the env by a symbol-value key
from'venv :: String -> T a Sexp
from'venv k s = from'lenv k s <|> from'genv k s

-- | Get S-exp from the env by a symbol-value key (nil if fail)
from'venv' :: String -> T a Sexp
from'venv' k s = from'lenv' k s >>= \case
  t@(_, NIL) -> from'genv' k t
  t          -> pure t

-- | Get S-exp from the global-env by a symbol-value key
from'genv :: String -> T a Sexp
from'genv = getter env'g errVoidSymbolVar

-- | Get S-exp from the global-env by a symbol-value key (nil if fail)
from'genv' :: String -> T a Sexp
from'genv' = getter' env'g

-- | Get S-exp from the local-env by a symbol-value key
from'lenv :: String -> T a Sexp
from'lenv = getter env'l errVoidSymbolVar

-- | Get S-exp from the local-env by a symbol-value key (nil if fail)
from'lenv' :: String -> T a Sexp
from'lenv' = getter' env'l

-- | Get functions from the fn-env by a symbol-funtion key
from'fenv :: String -> T a Fn
from'fenv = getter env'f errVoidSymbolFn

-- | Env getters builder
getter :: (Env -> M.Map String b) -> String -> String -> T a b
getter f msg k s@(env, _) = case M.lookup k (f env) of
  Just v  -> put v s
  Nothing -> err [errEval, msg, k]

-- | Env getters builder (ignore-errors)
-- The same as 'getter' but return nil instead of raising errors
getter' :: (Env -> M.Map String Sexp) -> String -> T a Sexp
getter' f k s@(env, _) = case M.lookup k (f env) of
  Just v  -> put v s
  Nothing -> put NIL s

-- | When going into local-scope
local :: T a a
local s@(env@Env {..}, _) =
  put' (env { env'g = env'l <> env'g, env'l = mempty }) s

-- | When getting out from local-scope
global :: ST b -> T a a
global (g@Env{}, _) s@(l@Env{}, _) = put' (g { env'g = env'g l }) s

-- | Deactivate local env and use only global env
xlocal :: T a a
xlocal s@(env@Env{}, _) = put' (env { env'l = mempty }) s


----------
-- Read
----------
-- | Use 'ParserS' as a stream parser
type Parser = ParserS String

-- | READ
read' :: String -> RE Sexp
read' input = case parse' sexp input of
  Ok ok (State stream _ _) | isEmpty stream -> pure ok
                           | otherwise      -> err [errRepl, errManySexp]
  Error _ -> err [errRead, errParsing]

-- | S-expression
sexp :: Parser Sexp
sexp = between
  jump
  jump
  (choice
    [ at
    , comma
    , nil
    , str
    , bool
    , float
    , int
    , quote
    , bquote
    , key
    , sym
    , cons
    , vec
    , form
    ]
  )

-- | Skip whitespaces and line/block comments
jump :: Parser ()
jump = skips lispdef

-- | End of Identifiers
end :: Parser ()
end = gap <|> void (try (symbol ")"))

-- | NIL
nil :: Parser Sexp
nil = NIL <$ symbol "nil" <* end

-- | Bool
bool :: Parser Sexp
bool = Bool <$> (symbol "t" <* end $> True)

-- | Integer
int :: Parser Sexp
int = Int <$> integer <* option "" (symbol ".") <* end

-- | Non-integer
float :: Parser Sexp
float = Float <$> floatB <* end

-- | Symbol
sym :: Parser Sexp
sym = Symbol <$> identifier lispdef

-- | Keyword
key :: Parser Sexp
key = Keyword . (":" ++) <$> (symbol ":" *> identifier lispdef)

-- | String Literal
str :: Parser Sexp
str = String <$> stringLit

-- | Quote
quote :: Parser Sexp
quote = symbol "'" *> (Quote <$> sexp)

-- | Backquote
bquote :: Parser Sexp
bquote = symbol "`" *> (Backquote <$> sexp)

-- | Comma followed by At-sign
at :: Parser Sexp
at = symbol ",@" *> (At <$> sexp)

-- | Comma
comma :: Parser Sexp
comma = symbol "," *> (Comma <$> sexp)

-- | Vector
vec :: Parser Sexp
vec = Vector . V.fromList <$> between (symbol "#(") (symbol ")") (many sexp)

-- | Cons
cons :: Parser Sexp
cons = between (symbol "(") (symbol ")") pair
 where
  pair = sexp >>= \a -> symbol "." *> spaces *> sexp >>= \b -> pure $ Cons a b

-- | Form
form :: Parser Sexp
form = List <$> between (symbol "(") (symbol ")") (many sexp)


----------
-- Eval
----------
-- | Type for functions and macros
type Fn = T [Sexp] Sexp

-- | EVAL
eval :: T Sexp Sexp
eval s = get s >>= \case
  a | nilp a               -> put NIL s
  Symbol k                 -> from'venv k s
  Quote  a                 -> put a s
  List   xs@(Symbol{} : _) -> put xs s >>= apply
  List   (   a        : _) -> err [errEval, errInvalidFn, show' a]
  a@Cons{}                 -> err [errEval, errInvalidFn, show' a]
  a@Comma{}                -> err [errEval, errNotInBquote, show' a]
  a@At{}                   -> err [errEval, errNotInBquote, show' a]
  Seq       xs             -> put xs s >>= eval'seq
  Backquote a              -> put a s >>= eval'bquote 1
  a                        -> put a s

-- | Apply the function of symbol name to the given arguments
apply :: Fn
apply s = head' "apply" s >>= get >>= \case
  Symbol k -> from'fenv k s >>= \(_, fn) -> fn s
  a        -> err [errEval, errNotSymbol, show' a]

-- | set
f'set :: Fn
f'set s = g'binary s >>= bimapM eval >>= \t@(_, x) -> case x of
  (Symbol k, a) -> put a t >>= set'venv k
  a             -> err [errEval, errNotSymbol, show' (fst a)]

-- | setq
f'setq :: Fn
f'setq s = g'evenary s >>= go
 where
  go t = get t >>= \case
    [Symbol k, a]       -> put a t >>= eval >>= set'venv k
    Symbol k : a : rest -> put a t >>= eval >>= set'venv k >>= put rest >>= go
    _                   -> err [errEval, errMalformed, "setq"]

-- | setf
f'setf :: Fn
f'setf _ = undefined

-- | getf
f'getf :: Fn
f'getf _ = undefined

-- | let
f'let :: Fn
f'let = deflet bind'par "let"

-- | let*
f'let' :: Fn
f'let' = deflet bind'seq "let*"

-- | defparameter
f'defparameter :: Fn
f'defparameter = defsym set'venv

-- | defvar
f'defvar :: Fn
f'defvar = defsym set'venv'ifndef

-- | quote
f'quote :: Fn
f'quote = g'unary

-- | not
f'not :: Fn
f'not = pred' $ \case
  NIL -> Bool True
  _   -> NIL

-- | or
f'or :: Fn
f'or = nfold pure or' "or"

-- | and
f'and :: Fn
f'and = nfold pure and' "and"

-- | =
f'EQ :: Fn
f'EQ = nfold' g'number (==) "="

-- | /=
f'NE :: Fn
f'NE = nfold' g'number (/=) "/="

-- | <
f'LT :: Fn
f'LT = nfold' g'number (<) "<"

-- | >
f'GT :: Fn
f'GT = nfold' g'number (>) ">"

-- | <=
f'LE :: Fn
f'LE = nfold' g'number (<=) "<="

-- | >=
f'GE :: Fn
f'GE = nfold' g'number (>=) ">="

-- | min
f'min :: Fn
f'min = nfold g'number (bop min) "min"

-- | max
f'max :: Fn
f'max = nfold g'number (bop max) "max"

-- | (+)
f'add :: Fn
f'add = nfold g'number (bop (+)) "+"

-- | (-)
f'sub :: Fn
f'sub = nfold g'number (bop (-)) "-"

-- | (*)
f'mul :: Fn
f'mul = nfold g'number (bop (*)) "*"

-- | (/)
f'div :: Fn
f'div = nfold g'number (bop (/)) "/"

-- | (%) or mod
f'mod :: Fn
f'mod = binary g'number (modify (uncurry (bop mod')))

-- | expt
f'expt :: Fn
f'expt = binary g'number (modify (uncurry (bop (**))))

-- | sqrt
f'sqrt :: Fn
f'sqrt = unary g'float (modify (uop sqrt))

-- | exp
f'exp :: Fn
f'exp = unary g'float (modify (uop exp))

-- | log
f'log :: Fn
f'log = unary g'float (modify (uop log))

-- | sin
f'sin :: Fn
f'sin = unary g'float (modify (uop sin))

-- | cos
f'cos :: Fn
f'cos = unary g'float (modify (uop cos))

-- | tan
f'tan :: Fn
f'tan = unary g'float (modify (uop tan))

-- | asin
f'asin :: Fn
f'asin = unary g'float (modify (uop asin))

-- | acos
f'acos :: Fn
f'acos = unary g'float (modify (uop acos))

-- | atan
f'atan :: Fn
f'atan = unary g'float (modify (uop atan))

-- | float
f'float :: Fn
f'float s = g'unary s >>= g'float

-- | abs
f'abs :: Fn
f'abs = unary pure (modify (uop abs))

-- | (1+)
f'1p :: Fn
f'1p = unary pure (modify (uop (+ 1)))

-- | (1-)
f'1m :: Fn
f'1m = unary pure (modify (uop (subtract 1)))

-- | random
f'random :: Fn
f'random = unary
  g'number
  (\s@(_, x) -> case x of
    Float a -> put (Float . random $ a) s
    Int   a -> put (Int . random $ a) s
    _       -> err [errEval, errNotAllowed, "random"]
  )
  where random x = fst $ randomR (0, x) (mkStdGen 1)

-- | ash
f'ash :: Fn
f'ash = undefined

-- | atom
f'atom :: Fn
f'atom = pred' $ \case
  NIL       -> Bool True
  Bool{}    -> Bool True
  Int{}     -> Bool True
  Float{}   -> Bool True
  Symbol{}  -> Bool True
  Keyword{} -> Bool True
  String{}  -> Bool True
  _         -> NIL

-- | symbolp
f'symbolp :: Fn
f'symbolp = pred' $ \case
  NIL      -> Bool True
  Symbol{} -> Bool True
  _        -> NIL

-- | numberp
f'numberp :: Fn
f'numberp = pred' $ \case
  Int{}   -> Bool True
  Float{} -> Bool True
  _       -> NIL

-- | stringp
f'stringp :: Fn
f'stringp = pred' $ \case
  String{} -> Bool True
  _        -> NIL

-- | listp
f'listp :: Fn
f'listp = pred' $ \case
  NIL    -> Bool True
  Cons{} -> Bool True
  List{} -> Bool True
  _      -> NIL

-- | characterp
f'characterp :: Fn
f'characterp = undefined

-- | alphacharp
f'alphacharp :: Fn
f'alphacharp = undefined

-- | alphanumericp
f'alphanumericp :: Fn
f'alphanumericp = undefined

-- | digit-char-p
f'digitcharp :: Fn
f'digitcharp = undefined

-- | lower-case-p
f'lowercasep :: Fn
f'lowercasep = undefined

-- | upper-case-p
f'uppercasep :: Fn
f'uppercasep = undefined

-- | boundp
f'boundp :: Fn
f'boundp s = g'unary s >>= eval >>= g'symbol >>= \t@(_, Symbol k) ->
  from'venv' k t >>= get >>= \case
    NIL -> put NIL t
    _   -> put (Bool True) t

-- | vectorp
f'vectorp :: Fn
f'vectorp = pred' $ \case
  Vector{} -> Bool True
  _        -> NIL

-- | hash-table-p
f'hashtablep :: Fn
f'hashtablep = pred' $ \case
  HashTable{} -> Bool True
  _           -> NIL

-- | account-p
f'accoutp :: Fn
f'accoutp = undefined

-- | list
f'list :: Fn
f'list s = g'nary s >>= mapM' eval >>= modify (pure . List)

-- | cons
f'cons :: Fn
f'cons s = g'binary s >>= bimapM eval >>= \t@(_, x) -> case x of
  (a, NIL   ) -> put (List [a]) t
  (a, List l) -> put (List (a : l)) t
  (a, b     ) -> put (Cons a b) t

-- | car
f'car :: Fn
f'car = g'unary >=> eval >=> car

-- | cdr
f'cdr :: Fn
f'cdr = g'unary >=> eval >=> cdr

-- | caar
f'caar :: Fn
f'caar = g'unary >=> eval >=> car >=> car

-- | cadr
f'cadr :: Fn
f'cadr = g'unary >=> eval >=> cdr >=> car

-- | cdar
f'cdar :: Fn
f'cdar = g'unary >=> eval >=> car >=> cdr
-- | cddr
f'cddr :: Fn
f'cddr = g'unary >=> eval >=> cdr >=> cdr

-- | caaar
f'caaar :: Fn
f'caaar = g'unary >=> eval >=> car >=> car >=> car

-- | caadr
f'caadr :: Fn
f'caadr = g'unary >=> eval >=> cdr >=> car >=> car

-- | caadr
f'cadar :: Fn
f'cadar = g'unary >=> eval >=> car >=> cdr >=> car

-- | caadr
f'caddr :: Fn
f'caddr = g'unary >=> eval >=> cdr >=> cdr >=> car

-- | caadr
f'cdaar :: Fn
f'cdaar = g'unary >=> eval >=> car >=> car >=> cdr

-- | caadr
f'cdadr :: Fn
f'cdadr = g'unary >=> eval >=> cdr >=> car >=> cdr

-- | caadr
f'cddar :: Fn
f'cddar = g'unary >=> eval >=> car >=> cdr >=> cdr

-- | caadr
f'cdddr :: Fn
f'cdddr = g'unary >=> eval >=> cdr >=> cdr >=> cdr

-- | caaaar
f'caaaar :: Fn
f'caaaar = g'unary >=> eval >=> car >=> car >=> car >=> car

-- | caaaar
f'caaadr :: Fn
f'caaadr = g'unary >=> eval >=> cdr >=> car >=> car >=> car

-- | caaaar
f'caadar :: Fn
f'caadar = g'unary >=> eval >=> car >=> cdr >=> car >=> car

-- | caaaar
f'caaddr :: Fn
f'caaddr = g'unary >=> eval >=> cdr >=> cdr >=> car >=> car

-- | caaaar
f'cadaar :: Fn
f'cadaar = g'unary >=> eval >=> car >=> car >=> cdr >=> car

-- | caaaar
f'cadadr :: Fn
f'cadadr = g'unary >=> eval >=> cdr >=> car >=> cdr >=> car

-- | caaaar
f'caddar :: Fn
f'caddar = g'unary >=> eval >=> car >=> cdr >=> cdr >=> car

-- | caaaar
f'cadddr :: Fn
f'cadddr = g'unary >=> eval >=> cdr >=> cdr >=> cdr >=> car

-- | caaaar
f'cdaaar :: Fn
f'cdaaar = g'unary >=> eval >=> car >=> car >=> car >=> cdr

-- | caaaar
f'cdaadr :: Fn
f'cdaadr = g'unary >=> eval >=> cdr >=> car >=> car >=> cdr

-- | caaaar
f'cdadar :: Fn
f'cdadar = g'unary >=> eval >=> car >=> cdr >=> car >=> cdr

-- | caaaar
f'cdaddr :: Fn
f'cdaddr = g'unary >=> eval >=> cdr >=> cdr >=> car >=> cdr

-- | caaaar
f'cddaar :: Fn
f'cddaar = g'unary >=> eval >=> car >=> car >=> cdr >=> cdr

-- | caaaar
f'cddadr :: Fn
f'cddadr = g'unary >=> eval >=> cdr >=> car >=> cdr >=> cdr

-- | caaaar
f'cdddar :: Fn
f'cdddar = g'unary >=> eval >=> car >=> cdr >=> cdr >=> cdr

-- | caaaar
f'cddddr :: Fn
f'cddddr = g'unary >=> eval >=> cdr >=> cdr >=> cdr >=> cdr

-- | nth
f'nth :: Fn
f'nth s = g'binary s >>= bimapM eval >>= \t@(_, (n, l)) ->
  put n t >>= g'int >>= get >>= \(Int i) -> put l t >>= nth (fromIntegral i)

-- | first
f'first :: Fn
f'first s = g'unary s >>= eval >>= nth 0

-- | second
f'second :: Fn
f'second s = g'unary s >>= eval >>= nth 1

-- | third
f'third :: Fn
f'third s = g'unary s >>= eval >>= nth 2

-- | fourth
f'fourth :: Fn
f'fourth s = g'unary s >>= eval >>= nth 3

-- | fifth
f'fifth :: Fn
f'fifth s = g'unary s >>= eval >>= nth 4

-- | sixth
f'sixth :: Fn
f'sixth s = g'unary s >>= eval >>= nth 5

-- | seventh
f'seventh :: Fn
f'seventh s = g'unary s >>= eval >>= nth 6

-- | eighth
f'eighth :: Fn
f'eighth s = g'unary s >>= eval >>= nth 7

-- | nineth
f'ninth :: Fn
f'ninth s = g'unary s >>= eval >>= nth 8

-- | tenth
f'tenth :: Fn
f'tenth s = g'unary s >>= eval >>= nth 9

-- | rest
f'rest :: Fn
f'rest = f'cdr

-- | do
f'do :: Fn
f'do = undefined

-- | dolist
f'dolist :: Fn
f'dolist = undefined

-- | eval
f'eval :: Fn
f'eval s = g'unary s >>= eval >>= eval

-- | funcall
f'funcall :: Fn
f'funcall = undefined

-- | apply
f'apply :: Fn
f'apply = undefined
-- f'apply s = g'nary s >>= mapM' eval >>= \t@(_, x) -> case x of
  -- (f : args) -> case put args t >>= last' of {}

-- | symbol-value
f'symbolValue :: Fn
f'symbolValue s = g'unary s >>= eval >>= g'symbol >>= eval


----------
-- Guard
----------
-- | Get the n-ary function's arguments
g'nary :: T [Sexp] [Sexp]
g'nary = arity (const True)

-- | Guard for unary function's arguments
g'unary :: T [Sexp] Sexp
g'unary = arity (== 1) >=> head' "g'unary"

-- | Guard for binary function's arguments
g'binary :: T [Sexp] (Sexp, Sexp)
g'binary = arity (== 2) >=> g'tuple

-- | Guard for even-ary(pairwise) function's arguments
g'evenary :: T [Sexp] [Sexp]
g'evenary = arity (\x -> x /= 0 && even x)

-- | Guard for tuple state: transforms the result value into a tuple
g'tuple :: T [Sexp] (Sexp, Sexp)
g'tuple = \case
  s@(_, [x, y]) -> put (x, y) s
  a             -> err [errEval, errWrongNargs, show . length $ a]

-- | Guard for non-empty S-exp list
g'nempty :: String -> T [a] [a]
g'nempty msg s = get s >>= \case
  [] -> err [errEval, errNoArgs, msg]
  _  -> pure s

-- | Guard for non-empty S-exp list ([nil] if fail)
g'nempty' :: T [Sexp] [Sexp]
g'nempty' s = get s >>= \case
  [] -> put [NIL] s
  _  -> pure s

-- | Guard for bound symbols
g'bound :: T Sexp Sexp
g'bound s = eval s >>= g'symbol >>= get >>= \(Symbol k) -> from'venv k s

-- | Guard for symbols
g'notnil :: T Sexp Sexp
g'notnil s = get s >>= \case
  NIL -> err [errEval, errUnexpected, "NOT-NIL."]
  _   -> pure s

-- | Guard for symbols
g'symbol :: T Sexp Sexp
g'symbol s = get s >>= \case
  Symbol{} -> pure s
  a        -> err [errEval, errNotSymbol, show' a]

-- | Guard for numbers
g'number :: T Sexp Sexp
g'number s = get s >>= \case
  Int{}   -> pure s
  Float{} -> pure s
  a       -> err [errEval, errNotNumber, show' a]

-- | Guard for boolean
g'bool :: T Sexp Sexp
g'bool s = get s >>= \case
  NIL     -> pure s
  Float{} -> pure s
  a       -> err [errEval, errNotNumber, show' a]

-- | Guard for strings
g'string :: T Sexp Sexp
g'string s = get s >>= \case
  String{} -> pure s
  a        -> err [errEval, errNotString, show' a]

-- | Guard for lists
g'list :: T Sexp Sexp
g'list s = get s >>= \case
  NIL    -> pure s
  Cons{} -> pure s
  List{} -> pure s
  a      -> err [errEval, errNotList, show' a]

-- | Ensure that the state is an integer S-exp
g'int :: T Sexp Sexp
g'int s = g'number s >>= get >>= \case
  i@Int{} -> put i s
  a       -> err [errEval, errNotInteger, show' a]

-- | Ensure that the state is a float S-exp
g'float :: T Sexp Sexp
g'float s = g'number s >>= get >>= \case
  Int i     -> put (Float . fromIntegral $ i) s
  r@Float{} -> put r s
  a         -> err [errEval, errNotFloat, show' a]

-- | Ensure that the state is a non-zero S-exp
g'nzero :: T Sexp Sexp
g'nzero s = get s >>= \case
  Int   0 -> err [errEval, errDivByZero]
  Float 0 -> err [errEval, errDivByZero]
  _       -> pure s

-- | Ensure that the given key is not defined in the local env
g'undef'lkey :: String -> T Sexp Sexp
g'undef'lkey k s@(Env {..}, _) = case M.lookup k env'l of
  Just _  -> err [errEval, errManySymbol, k]
  Nothing -> pure s


----------
-- Core
----------

instance Eq Sexp where
  NIL       == NIL       = True
  Bool    _ == Bool    _ = True
  Int     a == Int     b = a == b
  Float   a == Float   b = a == b
  Symbol  a == Symbol  b = a == b
  Keyword a == Keyword b = a == b
  String  a == String  b = a == b
  a         == b         = nilp a && nilp b


instance Ord Sexp where
  Int     a <= Int     b = a <= b
  Float   a <= Float   b = a <= b
  Symbol  a <= Symbol  b = a <= b
  Keyword a <= Keyword b = a <= b
  String  a <= String  b = a <= b
  _         <= _         = False


-- | Check if the given S-exp is identical to 'NIL'
nilp :: Sexp -> Bool
nilp = \case
  NIL                   -> True
  List      []          -> True
  Quote     NIL         -> True
  Backquote NIL         -> True
  (Quote     (List [])) -> True
  (Backquote (List [])) -> True
  _                     -> False

-- |
atom :: Sexp -> Bool
atom = \case
  NIL       -> True
  Bool{}    -> True
  Int{}     -> True
  Float{}   -> True
  Symbol{}  -> True
  Keyword{} -> True
  String{}  -> True
  _         -> False

-- |
or' :: Sexp -> Sexp -> RE Sexp
or' a b = case (a, b) of
  (NIL, b) -> pure b
  (a  , _) -> pure a

-- |
and' :: Sexp -> Sexp -> RE Sexp
and' a b = case (a, b) of
  (NIL, _) -> pure NIL
  (_  , b) -> pure b

-- | Build functions to get a list item by index
nth :: Int -> T Sexp Sexp
nth i s = get s >>= \case
  List l -> case drop i l of
    []    -> put NIL s
    x : _ -> put x s
  a -> err [errEval, errMalformed, show' a]

-- | functional core of cdr
car :: T Sexp Sexp
car s = g'list s >>= get >>= \case
  Cons x _     -> put x s
  List (x : _) -> put x s
  _            -> put NIL s

-- | functional core of cdr
cdr :: T Sexp Sexp
cdr s = g'list s >>= get >>= \case
  Cons _ y      -> put y s
  List (_ : xs) -> put (List xs) s
  _             -> put NIL s

-- | Turn the state result to its head value
head' :: String -> T [a] a
head' msg s = g'nempty msg s >>= modify (pure . head)

-- | Turn the state result to its head value (nil if fail)
head_ :: T [Sexp] Sexp
head_ s = g'nempty' s >>= modify (pure . head)

-- | Turn the state result to its tail values
tail' :: String -> T [a] [a]
tail' msg s = g'nempty msg s >>= modify (pure . tail)

-- | Turn the state result to its tail values ([] if fail)
tail_ :: T [Sexp] [Sexp]
tail_ s = g'nempty' s >>= modify (pure . tail)

-- | Turn the state result to its last value
last' :: String -> T [a] a
last' msg s = g'nempty msg s >>= modify (pure . last)

-- | Turn the state result to its last value (nil if fail)
last_ :: T [Sexp] Sexp
last_ s = g'nempty' s >>= modify (pure . last)

-- | Turn the state result to its init values
init' :: String -> T [a] [a]
init' msg s = g'nempty msg s >>= modify (pure . init)

-- | Turn the state result to its init values ([] if fail)
init_ :: T [Sexp] [Sexp]
init_ s = g'nempty' s >>= modify (pure . init)

-- | Build functions to control function's number of arguments
arity :: (Int -> Bool) -> T [Sexp] [Sexp]
arity p s@(_, x : args)
  | p nargs   = put args s
  | otherwise = err [errEval, errWrongNargs, show' x ++ ",", show nargs]
  where nargs = length args
arity _ _ = err [errEval, errNoArgs, "arity"]

-- | Predicate builder
pred' :: (Sexp -> Sexp) -> T [Sexp] Sexp
pred' p s = g'unary s >>= eval >>= modify (pure . p)

-- | Map the list-form state result 'ST' to an action. The 'mapM' for 'ST'.
mapM' :: T a b -> T [a] [b]
mapM' f = go []
 where
  go r s@(_, xs) = case xs of
    []       -> put (reverse r) s
    x : rest -> put x s >>= f >>= \(env, v) -> go (v : r) (env, rest)

-- | Map the tuple-form state result to an action. The monadic 'bimap' for 'ST'.
bimapM :: T a b -> T (a, a) (b, b)
bimapM f s@(_, (x, y)) =
  put x s >>= f >>= \t@(_, v) -> put y t >>= f >>= \u@(_, w) -> put (v, w) u

-- | Evaluate backquoted S-exp
eval'bquote :: Int -> T Sexp Sexp
eval'bquote d s = get s >>= \case
  a@At{}       -> err [errEval, errMalformed, show' a]
  a@Comma{}    -> put a s >>= replaceable d id
  List      xs -> put xs s >>= splice d
  Backquote a  -> put a s >>= eval'bquote (d + 1) >>= modify (pure . Backquote)
  a            -> put a s

-- | Check if a given backquoted S-exp is replaceable with the evalualed value
replaceable :: Int -> (Sexp -> Sexp) -> T Sexp Sexp
replaceable d f s = get s >>= \x -> case compare' d x of
  LT -> pure s
  EQ -> replace d f s
  GT -> err [errEval, errNotInBquote, show' x]
 where
  compare' d x = compare (count 0 x) d
  count n = \case
    Comma a -> count (n + 1) a
    At    a -> count (n + 1) a
    _       -> n

-- | Substitute comma and at-sign with the evaluated value
replace :: Int -> (Sexp -> Sexp) -> T Sexp Sexp
replace d f s = get s >>= \case
  Comma a@Comma{} -> put a s >>= replace d (f . Comma)
  Comma a@At{}    -> put a s >>= replace d (f . Comma)
  At    a@Comma{} -> put a s >>= replace d (f . At)
  At    a@At{}    -> put a s >>= replace d (f . At)
  Comma a         -> put a s >>= eval >>= modify (pure . f)
  At    a         -> put a s >>= eval >>= \t@(_, x) -> case x of
    List xs -> put xs t >>= mapM' (modify (pure . f)) >>= modify (pure . List)
    _       -> pure t
  _ -> pure s

-- | Splice the given backquoted list
splice :: Int -> T [Sexp] Sexp
splice d = go []
 where
  go r s@(env, xs) = case xs of
    []       -> put (reverse r) s >>= modify (pure . List . concat)
    x : rest -> case x of
      a@Comma{} ->
        put a s >>= replaceable d id >>= \(env', v) -> go ([v] : r) (env', rest)
      a@At{} -> put a s >>= replaceable d id >>= \(env', v) -> case v of
        NIL    -> go r (env', rest)
        List l -> go (l : r) (env', rest)
        a      -> go ([a] : r) (env, rest)
      a -> go ([a] : r) (env, rest)

-- | Evaluate a sequence
eval'seq :: T [Sexp] Sexp
eval'seq = mapM' eval >=> last_

-- | Sequentially bind a sequence (let*-like)
bind'seq :: T [Sexp] [Sexp]
bind'seq s = get s >>= \case
  [] -> pure s
  (List [Symbol k, a] : rest) ->
    put a s >>= eval >>= set'lenv k >>= put rest >>= bind'seq
  x@List{} : _ -> err [errEval, errMalformed, show' x]
  Quote x@(Symbol k) : rest ->
    put x s >>= from'genv k >>= put (List [x, x] : rest) >>= bind'seq
  a : rest -> put a s >>= g'symbol >>= put (List [a, NIL] : rest) >>= bind'seq

-- | Parallelly bind a sequence (let-like)
bind'par :: T [Sexp] [Sexp]
bind'par s = get s >>= \case
  [] -> pure s
  (List [Symbol k, a] : rest) ->
    put a s
      >>= g'undef'lkey k
      >>= xlocal
      >>= eval
      >>= global s
      >>= set'lenv k
      >>= put rest
      >>= bind'par
  x@List{} : _ -> err [errEval, errMalformed, show' x]
  Quote x@(Symbol k) : rest ->
    put x s >>= from'genv k >>= put (List [x, x] : rest) >>= bind'par
  a : rest -> put a s >>= g'symbol >>= put (List [a, NIL] : rest) >>= bind'par

-- | let-function builder
deflet :: T [Sexp] [Sexp] -> String -> Fn
deflet f o s = g'nary s >>= get >>= \case
  Quote x@Symbol{} : rest -> put (List [List [x, NIL]] : rest) s >>= deflet f o
  Quote (List a)   : rest -> put (List [List a] : rest) s >>= deflet f o
  List a : rest ->
    put a s >>= local >>= f >>= put (Seq rest) >>= eval >>= global s
  _ -> err [errEval, errMalformed, o]

-- | function builder of defining symbol values
defsym :: (String -> T Sexp Sexp) -> Fn
defsym f s = g'binary s >>= get >>= \case
  (x@(Symbol k), a) -> put a s >>= eval >>= f k >>= put x
  x                 -> err [errEval, errNotSymbol, show' (fst x)]

-- | Unary arithmetic operator builder
uop
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a) -> Sexp -> RE Sexp
uop f = \case
  Int   a -> pure . Int . floor . f @Double . fromIntegral $ a
  Float a -> pure . Float . f $ a
  _       -> err [errEval, errNotAllowed, "uop"]

-- | Binary arithmetic operator builder
bop
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a -> a)
  -> Sexp
  -> Sexp
  -> RE Sexp
bop op x y = case (x, y) of
  (Int a, Int b) ->
    pure . Int . floor $ (op @Double) (fromIntegral a) (fromIntegral b)
  (Int   a, Float b) -> pure . Float $ fromIntegral a `op` b
  (Float a, Int b  ) -> pure . Float $ a `op` fromIntegral b
  (Float a, Float b) -> pure . Float $ a `op` b
  _                  -> err [errEval, errNotAllowed, "bop"]

-- | Unary function builder
unary :: T Sexp Sexp -> T Sexp Sexp -> Fn
unary g f = g'unary >=> eval >=> g >=> f

-- | Binary function builder
binary :: T Sexp Sexp -> T (Sexp, Sexp) Sexp -> Fn
binary g f = g'binary >=> bimapM (eval >=> g) >=> f

-- | N-fold function builder (arithmetic)
nfold :: T Sexp Sexp -> (Sexp -> Sexp -> RE Sexp) -> String -> Fn
nfold g f msg = g'nary >=> mapM' (eval >=> g) >=> foldNums f msg

-- | N-fold function builder (logical)
nfold' :: T Sexp Sexp -> (Sexp -> Sexp -> Bool) -> String -> Fn
nfold' g op msg =
  g'nary >=> g'nempty msg >=> mapM' (eval >=> g) >=> \s@(_, l) ->
    put (and $ zipWith op l (tail l)) s >>= get >>= \case
      False -> put NIL s
      _     -> put (Bool True) s

-- | Fold S-exp numbers using the given binary arithemetic operator.
foldNums :: (Sexp -> Sexp -> RE Sexp) -> String -> Fn
foldNums f o s = get s >>= \case
  [] -> case o of
    "+" -> put (Int 0) s
    "*" -> put (Int 1) s
    _   -> err [errEval, errNoArgs, o]
  [x] -> case o of
    "-" -> put x s >>= modify (f (Int 0))
    "/" -> put x s >>= g'nzero >>= modify (f (Int 1))
    _   -> put x s
  (x : xs) -> case o of
    "/" -> put xs s >>= mapM' g'nzero >>= modify (foldM f x)
    _   -> put xs s >>= modify (foldM f x)

-- | LISP built-in functions
built'in :: [(String, Fn)]
built'in =
  [ ("set"                  , f'set)
  , ("setq"                 , f'setq)
  , ("setf"                 , f'setf)
  , ("getf"                 , f'getf)
  , ("let"                  , f'let)
  , ("let*"                 , f'let')
  , ("defparameter"         , f'defparameter)
  , ("defvar"               , f'defvar)
  , ("makeunbound"          , undefined)
  , ("quote"                , f'quote)
  , ("not"                  , f'not)
  , ("or"                   , f'or)
  , ("and"                  , f'and)
  , ("="                    , f'EQ)
  , ("/="                   , f'NE)
  , ("<"                    , f'LT)
  , (">"                    , f'GT)
  , ("<="                   , f'LE)
  , (">="                   , f'GE)
  , ("min"                  , f'min)
  , ("max"                  , f'max)
  , ("+"                    , f'add)
  , ("-"                    , f'sub)
  , ("*"                    , f'mul)
  , ("/"                    , f'div)
  , ("mod"                  , f'mod)
  -- numerator
  -- denominator
  , ("rem"                  , undefined)
  , ("expt"                 , f'expt)
  , ("sqrt"                 , f'sqrt)
  , ("exp"                  , f'exp)
  , ("log"                  , f'log)
  , ("sin"                  , f'sin)
  , ("cos"                  , f'cos)
  , ("tan"                  , f'tan)
  , ("asin"                 , f'asin)
  , ("acos"                 , f'acos)
  , ("atan"                 , f'atan)
  , ("truncate"             , undefined)
  , ("round"                , undefined)
  , ("ceiling"              , undefined)
  , ("floor"                , undefined)
  , ("float"                , f'float)
  , ("abs"                  , f'abs)
  , ("signum"               , undefined)
  , ("1+"                   , f'1p)
  , ("1-"                   , f'1m)
  -- COMPLEX-NUMBER
  -- realpart #c
  -- imagpart #c
  -- phase #c
  -- abs #c
  -- conjugate #c
  , ("random"               , f'random)
  -- (setq *random-state* n)
  , ("ash"                  , f'ash)
  -- logand
  -- logior
  -- logxor
  -- lognot
  -- BINARY-OCTAL-HEX LITERAL
  -- #b1010
  -- #o52
  -- #x2a
  , ("intern"               , undefined)
  , ("format"               , undefined)
  -- \\ \" LITERAL ESCAPE
  , ("string="              , undefined)
  , ("string<"              , undefined)
  -- concatenate 'string 'list
  , ("concatenate"          , undefined)
  , ("string-downcase"      , undefined)
  , ("string-upcase"        , undefined)
  , ("string-capitalize"    , undefined)
  , ("string-trim"          , undefined)
  -- cl-ppcre:split
  , ("reduce"               , undefined)
  , ("length"               , undefined)
  , ("search"               , undefined)
  , ("subseq"               , undefined)
  -- CHARACTER LITERAL
  -- #\a #\space #\newline #\tab ..
  , ("code-char"            , undefined)
  , ("char-code"            , undefined)
  , ("char"                 , undefined)
  -- REGEX
  , ("get-decoded-time"     , undefined)
  , ("get-universal-time"   , undefined)
  , ("decode-universal-time", undefined)
  , ("encode-universal-time", undefined)
  -- multiple-value-bind
  , ("list"                 , f'list)
  , ("cons"                 , f'cons)
  , ("car"                  , f'car)
  , ("cdr"                  , f'cdr)
  , ("caar"                 , f'caar)
  , ("cadr"                 , f'cadr)
  , ("cdar"                 , f'cdar)
  , ("cddr"                 , f'cddr)
  , ("caaar"                , f'caaar)
  , ("caadr"                , f'caadr)
  , ("cadar"                , f'cadar)
  , ("caddr"                , f'caddr)
  , ("cdaar"                , f'cdaar)
  , ("cdadr"                , f'cdadr)
  , ("cddar"                , f'cddar)
  , ("cdddr"                , f'cdddr)
  , ("caaaar"               , f'caaaar)
  , ("caaadr"               , f'caaadr)
  , ("caadar"               , f'caadar)
  , ("caaddr"               , f'caaddr)
  , ("cadaar"               , f'cadaar)
  , ("cadadr"               , f'cadadr)
  , ("caddar"               , f'caddar)
  , ("cadddr"               , f'cadddr)
  , ("cdaaar"               , f'cdaaar)
  , ("cdaadr"               , f'cdaadr)
  , ("cdadar"               , f'cdadar)
  , ("cdaddr"               , f'cdaddr)
  , ("cddaar"               , f'cddaar)
  , ("cddadr"               , f'cddadr)
  , ("cdddar"               , f'cdddar)
  , ("cddddr"               , f'cddddr)
  , ("equal"                , undefined)
  , ("nth"                  , f'nth)
  , ("first"                , f'first)
  , ("second"               , f'second)
  , ("third"                , f'third)
  , ("fourth"               , f'fourth)
  , ("fifth"                , f'fifth)
  , ("sixth"                , f'sixth)
  , ("seventh"              , f'seventh)
  , ("eighth"               , f'eighth)
  , ("ninth"                , f'ninth)
  , ("tenth"                , f'tenth)
  , ("rest"                 , f'rest)
  , ("position"             , undefined)
  , ("append"               , undefined)
  , ("nthcdr"               , undefined)
  , ("butlast"              , undefined)
  , ("reverse"              , undefined)
  , ("sort"                 , undefined)
  , ("remove-duplicates"    , undefined)
  , ("member"               , undefined)
  , ("mapcar"               , undefined)
  , ("remove-if-not"        , undefined)
  , ("every"                , undefined)
  , ("some"                 , undefined)
  , ("push"                 , undefined)
  , ("pop"                  , undefined)
  , ("assoc"                , undefined)
  , ("vector"               , undefined)
  , ("elt"                  , undefined)
  , ("aref"                 , undefined)
  , ("coerce"               , undefined)
  , ("map"                  , undefined)
  , ("make-hash-table"      , undefined)
  , ("hash-table-count"     , undefined)
  , ("get-hash"             , undefined)
  , ("nth-value"            , undefined)
  , ("remhash"              , undefined)
  , ("maphash"              , undefined)
  , ("defstruct"            , undefined)
  , ("account-id"           , undefined)
  -- MULTIPLE-VALUES: 'skip
  -- defclass
  -- OBJECTS: 'skip
  , ("defun"                , undefined)
  , ("lambda"               , undefined)
  , ("progn"                , undefined)
  , ("prog1"                , undefined)
  , ("prog2"                , undefined)
  , ("loop"                 , undefined)
  , ("do"                   , f'do)
  , ("dolist"               , f'dolist)
  , ("dotimes"              , undefined)
  , ("if"                   , undefined)
  , ("when"                 , undefined)
  , ("cond"                 , undefined)
  , ("error"                , undefined)
  -- EXCEPTIONS: 'skip
  -- handler-case
  -- define-condition
  -- handler-bind
  -- unwind-protect
  , ("open"                 , undefined)
  , ("close"                , undefined)
  , ("with-open-file"       , undefined)
  , ("read-line"            , undefined)
  , ("load"                 , undefined)
  -- STREAM
  -- param: *standard-input*
  -- param: *standard-output*
  -- param: *error-output*
  -- make-string-input-stream
  -- make-string-output-stream
  , ("eval"                 , f'eval)
  , ("funcall"              , f'funcall)
  , ("apply"                , f'apply)
  , ("defmacro"             , undefined)
  , ("macroexpand"          , undefined)
  , ("type-of"              , undefined)
  , ("describe"             , undefined)
  , ("atom"                 , f'atom)
  , ("symbolp"              , f'symbolp)
  , ("numberp"              , f'numberp)
  , ("integerp"             , undefined)
  , ("rationalp"            , undefined)
  , ("floatp"               , undefined)
  , ("realp"                , undefined)
  , ("complexp"             , undefined)
  , ("zerop"                , undefined)
  , ("stringp"              , f'stringp)
  , ("listp"                , f'listp)
  -- character fn
  , ("characterp"           , f'characterp)
  , ("alpha-char-p"         , f'alphacharp)
  , ("alphanumericp"        , f'alphanumericp)
  , ("digit-char-p"         , f'digitcharp)
  , ("lower-case-p"         , f'lowercasep)
  , ("upper-case-p"         , f'uppercasep)
  --
  , ("boundp"               , f'boundp)
  , ("vectorp"              , f'vectorp)
  , ("hash-table-p"         , f'hashtablep)
  , ("account-p"            , undefined)
  , ("macro-function"       , undefined)
  , ("typep"                , undefined)
  , ("symbol-value"         , f'symbolValue)
  , ("symbol-function"      , undefined)
  , ("symbol-plist"         , undefined)
  , ("function"             , undefined)
  ]


----------
-- Print
----------
-- | PRINT
print' :: MonadIO m => Sexp -> InputT m ()
print' = outputStrLn . show'

-- | Stringify S-expression
show' :: Sexp -> String
show' = \case
  NIL                 -> "nil"
  Bool      _         -> "t"
  Int       intger    -> show intger
  Float     float     -> show float
  Symbol    symbol    -> symbol
  Keyword   keyword   -> keyword
  String    string    -> "\"" ++ string ++ "\""
  Quote     sexp      -> "'" ++ show' sexp
  Backquote sexp      -> "`" ++ show' sexp
  At        sexp      -> ",@" ++ show' sexp
  Comma     sexp      -> "," ++ show' sexp
  Seq       seq       -> show' (List seq)
  Vector    vector    -> "#(" ++ unwords (show' <$> V.toList vector) ++ ")"
  HashTable map       -> show map
  Function name fn    -> unwords [show fn, name]
  Macro    name macro -> unwords [show macro, name]
  List list | null list -> "nil"
            | otherwise -> "(" ++ unwords (show' <$> list) ++ ")"
  Cons a b -> "(" ++ go (a, b) ++ ")"
   where
    go = \case
      (x, NIL     ) -> unwords [show' x]
      (x, List [] ) -> unwords [show' x]
      (x, List y  ) -> unwords [show' x, unwords (show' <$> y)]
      (x, Quote y ) -> unwords [show' x, "quote", show' y]
      (x, Cons y z) -> unwords [show' x, go (y, z)]
      (x, y       ) -> unwords [show' x, ".", show' y]


instance Show (a -> b) where
  showsPrec _ _ = showString "#<function>"

deriving instance Show Sexp

deriving instance Pretty Sexp

deriving instance Pretty Env


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
errNoArgs = "No arguments provided:"

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
errManySexp = "More than one or malformed sexp in input"

errManySymbol :: String
errManySymbol = "Occurred variable more than once:"

errParsing :: String
errParsing = "Occurred error during parsing"

errDivByZero :: String
errDivByZero = "Arithmetic error: DIVISION-BY-ZERO"

errNotInBquote :: String
errNotInBquote = "Not inside backquote:"

errUnexpected :: String
errUnexpected = "Expected:"

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
  debug  = (d'read, pretty')
  loop s@(env, _) mode@(reader, printer) = do
    input <- getInputLine "SLISP> "
    case input of
      Nothing     -> pure ()
      Just []     -> loop s normal
      Just ";"    -> outputStrLn "Set to paste-mode" >> loop s mode
      Just ";;"   -> outputStrLn "Set to debug-mode" >> loop s debug
      Just ";;;"  -> d'symbolv env >> loop s mode
      Just ";;;;" -> d'symbolf env >> loop s mode
      Just str    -> case reader str >>= eval . (env, ) of
        Left  err      -> outputStrLn err >> loop s mode
        Right t@(_, x) -> printer x >> loop t mode

-- Run SLISP externally: READ-EVAL
re :: String -> RE (ST Sexp)
re stream = read' stream >>= flip put init'env >>= eval

-- Run SLISP externally: READ-EVAL-PRINT
rep :: String -> IO ()
rep stream = do
  case re stream of
    Left  err    -> putStrLn err
    Right (_, x) -> putStrLn (show' x)


----------
-- Debug
----------
-- | Debug-mode printer
pretty' :: (MonadIO m, Pretty a) => a -> InputT m ()
pretty' = outputStrLn . TL.unpack . pretty

-- | Debug-mode reader
d'read :: String -> RE Sexp
d'read s = case parse' sexp s of
  Ok ok (State stream _ _) | isEmpty stream -> pure ok
                           | otherwise      -> err [errRepl, errManySexp]
  Error state -> err [errRead, errParsing, "\n", TL.unpack (pretty state)]

-- | blank line
__ :: MonadIO m => InputT m ()
__ = outputStrLn mempty

-- | Display symbol values in Env
d'symbolv :: MonadIO m => Env -> InputT m ()
d'symbolv Env {..} =
  __
    >> outputStrLn "*** global symbol values: [(symbol \"key\", S-exp)] ***"
    >> pretty' (M.toList env'g)
    >> __
    >> outputStrLn "*** local symbol values (must be empty) ***"
    >> pretty' (M.toList env'l)
    >> __

-- | Display symbol functions in Env
d'symbolf :: MonadIO m => Env -> InputT m ()
d'symbolf Env {..} =
  __
    >> outputStrLn "*** SLISP built-in functions ***"
    >> mapM_ outputStrLn
             ((\(a, b) -> unwords [show b, "\t", a]) <$> M.toList env'f)
    >> __
