{-# Language DeriveAnyClass #-}
{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language RecordWildCards #-}
{-# Language StandaloneDeriving #-}
{-# Language TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Text.S.Example.Lisp where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( (>=>)
                                                , foldM
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Complex                   ( Complex(..)
                                                , conjugate
                                                , imagPart
                                                , phase
                                                , realPart
                                                )
import           Data.Fixed                     ( mod' )
import           Data.Function                  ( on )
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                )
import           Data.List                      ( sort
                                                , transpose
                                                )
import qualified Data.Map                      as M
import           Data.Ratio                     ( (%)
                                                , denominator
                                                , numerator
                                                )
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
import           System.Random                  ( StdGen
                                                , mkStdGen
                                                , randomR
                                                , randomRIO
                                                )
import           Text.S                         ( ParserS
                                                , Pretty(pretty)
                                                , Result(Error, Ok)
                                                , State(State)
                                                , Stream(isEmpty)
                                                , anychar
                                                , between
                                                , choice
                                                , count
                                                , floatB
                                                , gap
                                                , identifier
                                                , integer
                                                , lispdef
                                                , many
                                                , manyTill'
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
          | Rational    Rational
          | Float       Double
          | Complex     (Complex Double)
          | Symbol      String
          | Keyword     String
          | Char        String
          | String      String
          | Quote       Sexp
          | Backquote   Sexp
          | Comma       Sexp
          | At          Sexp
          | Cons        Sexp Sexp
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
  { env'r :: StdGen
  , env'g :: M.Map String Sexp
  , env'l :: M.Map String Sexp
  , env'f :: M.Map String Fn
  }
  deriving Show


instance Semigroup Env where
  (<>) = undefined

instance Monoid Env where
  mempty = Env (mkStdGen 0) mempty mempty mempty

-- | Empty environment
null'env :: ST Sexp
null'env = (mempty, NIL)

-- | Initialize SLISP environment
init'env :: Int -> ST Sexp
init'env seed = (Env (mkStdGen seed) mempty mempty (M.fromList built'in), NIL)

-- | Insert a symbol-value key and a S-exp to the env
set'venv :: String -> T Sexp Sexp
set'venv k s@(Env {..}, _) | M.member k env'l = set'lenv k s
                           | otherwise        = set'genv k s
-- | Insert a symbol-value key and a S-exp to the global-env
set'genv :: String -> T Sexp Sexp
set'genv k s@(env@Env {..}, x) = put' (env { env'g = M.insert k x env'g }) s

-- | Insert a symbol-value key and a S-exp to the local-env
set'lenv :: String -> T Sexp Sexp
set'lenv k s@(env@Env {..}, x) = put' (env { env'l = M.insert k x env'l }) s

-- | Insert a symbol-value key and a S-exp to the fn-env
set'fenv :: String -> T Fn Fn
set'fenv k s@(env@Env {..}, x) = put' (env { env'f = M.insert k x env'f }) s

-- | The same as `set'venv`, but set only when keys are not defined
set'venv'ifndef :: String -> T Sexp Sexp
set'venv'ifndef k s@(Env {..}, _) = case M.lookup k env'g of
  Just _  -> pure s
  Nothing -> set'venv k s

-- | Remove S-exp from the env by a symbol-value key
del'venv :: String -> T a a
del'venv k s = del'lenv k s >>= del'genv k

-- | Remove S-exp from the global-env by a symbol-value key
del'genv :: String -> T a a
del'genv k s@(env@Env {..}, _) = put' (env { env'g = M.delete k env'g }) s

-- | Remove S-exp from the local-env by a symbol-value key
del'lenv :: String -> T a a
del'lenv k s@(env@Env {..}, _) = put' (env { env'l = M.delete k env'l }) s

-- | Remove S-exp from the fn-env by a symbol-value key
del'fenv :: String -> T a a
del'fenv k s@(env@Env {..}, _) = put' (env { env'f = M.delete k env'f }) s

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

-- | Env get-function builder
getter :: (Env -> M.Map String b) -> String -> String -> T a b
getter f msg k s@(env, _) = case M.lookup k (f env) of
  Just v  -> put v s
  Nothing -> err [errEval, msg, k]

-- | Env get-function builder (ignore-errors)
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
read' input = case parse' p'sexp input of
  Ok ok (State stream _ _) | isEmpty stream -> pure ok
                           | otherwise      -> err [errRepl, errManySexp]
  Error _ -> err [errRead, errParsing]

-- | S-expression
p'sexp :: Parser Sexp
p'sexp = between
  jump
  jump
  (choice
    [ p'at
    , p'comma
    , p'nil
    , p'char
    , p'str
    , p'bool
    , p'complex
    , p'float
    , p'rat
    , p'int
    , p'quote
    , p'backquote
    , p'keyword
    , p'symbol
    , p'cons
    , p'vector
    , p'form
    ]
  )

-- | Skip whitespaces and line/block comments
jump :: Parser ()
jump = skips lispdef

-- | End of Identifiers
end :: Parser ()
end = gap <|> void (try (symbol ")"))

-- | NIL parser
p'nil :: Parser Sexp
p'nil = NIL <$ symbol "nil" <* end

-- | Bool parser
p'bool :: Parser Sexp
p'bool = Bool <$> (symbol "t" <* end $> True)

-- | Integer parser
p'int :: Parser Sexp
p'int = Int <$> integer <* option "" (symbol ".") <* end

-- | Rational parser
p'rat :: Parser Sexp
p'rat = integer >>= \a -> symbol "/" *> integer >>= \case
  b | b == 0 -> fail . unwords $ [errEval, errDivByZero]
  b          -> pure $ Rational (a % b)

-- | Float parser
p'float :: Parser Sexp
p'float = Float <$> floatB <* end

-- | Complex parser
p'complex :: Parser Sexp
p'complex = (symbol "#c" <|> symbol "#C") *> between
  (jump *> symbol "(")
  (jump *> symbol ")")
  (jump *> real >>= \a -> jump *> real >>= \b -> pure $ complex a b)
  where real = p'int <|> p'float

-- | Symbol parser
p'symbol :: Parser Sexp
p'symbol = Symbol <$> identifier lispdef

-- | Keyword parser
p'keyword :: Parser Sexp
p'keyword = Keyword <$> ((++) <$> symbol ":" <*> identifier lispdef)

-- | Char Literal parser
p'char :: Parser Sexp
p'char = Char <$> ((symbol "#\\" *> (p'char'extra <|> count 1 anychar)) <* end)

-- | String Literal parser
p'str :: Parser Sexp
p'str = String <$> stringLit

-- | Quote parser
p'quote :: Parser Sexp
p'quote = symbol "'" *> (Quote <$> p'sexp)

-- | Backquote parser
p'backquote :: Parser Sexp
p'backquote = symbol "`" *> (Backquote <$> p'sexp)

-- | (Comma + At-sign) parser
p'at :: Parser Sexp
p'at = symbol ",@" *> (At <$> p'sexp)

-- | Comma parser
p'comma :: Parser Sexp
p'comma = symbol "," *> (Comma <$> p'sexp)

-- | Vector parser
p'vector :: Parser Sexp
p'vector =
  Vector . V.fromList <$> between (symbol "#(") (symbol ")") (many p'sexp)

-- | Cons parser
p'cons :: Parser Sexp
p'cons = between (symbol "(") (symbol ")") (manyTill' dot p'sexp)
  >>= \(xs, a) -> pure $ foldr Cons a xs
 where
  dot =
    p'sexp >>= \a -> symbol "." *> spaces *> p'sexp >>= \b -> pure $ Cons a b

-- | Form parser
p'form :: Parser Sexp
p'form = List <$> between (symbol "(") (symbol ")") (many p'sexp)

-- | Lisp standard-char needed to treat specially
p'char'extra :: Parser String
p'char'extra = choice
  [ symbol "space"
  , symbol "newline"
  , symbol "backspace"
  , symbol "tab"
  , symbol "linefeed"
  , symbol "page"
  , symbol "return"
  , symbol "rubout"
  ]


----------
-- Eval
----------
-- | Type for functions and macros
type Fn = T [Sexp] Sexp

-- | EVAL
eval :: T Sexp Sexp
eval s = get s >>= \case
  a | nilp a   -> put NIL s
  Symbol    k  -> from'venv k s
  Quote     a  -> put a s
  Backquote a  -> put a s >>= eval'backquote 1
  List      xs -> put xs s >>= apply
  a@Cons{}     -> err [errEval, errInvalidFn, show' a]
  a@Comma{}    -> err [errEval, errNotInBackquote, show' a]
  a@At{}       -> err [errEval, errNotInBackquote, show' a]
  a            -> put a s

-- | Apply the function of symbol name to the given arguments
apply :: Fn
apply s@(_, f : args) = case f of
  Symbol k -> from'fenv k s >>= \(_, fn) -> fn s
  List l@(Symbol "lambda" : _) ->
    put l s >>= apply >>= modify (pure . (: args)) >>= apply
  Function _ fn -> fn s
  a             -> err [errEval, errInvalidFn, show' a]
apply _ = err [errEval, errNoArgs, "apply"]

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

-- | makunbound
f'makunbound :: Fn
f'makunbound s = g'unary s >>= eval >>= g'symbol >>= \t@(_, a@(Symbol k)) ->
  del'genv k t >>= put a

-- | defvar
f'defvar :: Fn
f'defvar = defsym set'venv'ifndef

-- | quote
f'quote :: Fn
f'quote = g'unary

-- | not
f'not :: Fn
f'not = pred' not'

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
f'LT = nfold' g'real (<) "<"

-- | >
f'GT :: Fn
f'GT = nfold' g'real (>) ">"

-- | <=
f'LE :: Fn
f'LE = nfold' g'real (<=) "<="

-- | >=
f'GE :: Fn
f'GE = nfold' g'real (>=) ">="

-- | min
f'min :: Fn
f'min = nfold g'real (bop'ord min) "min"

-- | max
f'max :: Fn
f'max = nfold g'real (bop'ord max) "max"

-- | (+)
f'add :: Fn
f'add = nfold g'number (bop'num (+)) "+"

-- | (-)
f'sub :: Fn
f'sub = nfold g'number (bop'num (-)) "-"

-- | (*)
f'mul :: Fn
f'mul = nfold g'number (bop'num (*)) "*"

-- | (/)
f'div :: Fn
f'div = nfold g'number (bop'frac (/)) "/"

-- | mod
f'mod :: Fn
f'mod = binary g'real (modify (uncurry (bop'real mod')))

-- | numerator
f'numerator :: Fn
f'numerator = unary g'rational (modify (pure . Int . numerator . unRat))

-- | denominator
f'denominator :: Fn
f'denominator = unary g'rational (modify (pure . Int . denominator . unRat))

-- | rem
f'rem :: Fn
f'rem = binary g'real (modify (uncurry (bop'real mod')))

-- | expt
f'expt :: Fn
f'expt = binary g'number (modify (uncurry (bop'flt (**))))

-- | sqrt
f'sqrt :: Fn
f'sqrt = unary g'number (modify (uop'flt sqrt))

-- | exp
f'exp :: Fn
f'exp = unary g'number (modify (uop'flt exp))

-- | log
f'log :: Fn
f'log = unary g'number (modify (uop'flt log))

-- | sin
f'sin :: Fn
f'sin = unary g'number (modify (uop'flt sin))

-- | cos
f'cos :: Fn
f'cos = unary g'number (modify (uop'flt cos))

-- | tan
f'tan :: Fn
f'tan = unary g'number (modify (uop'flt tan))

-- | asin
f'asin :: Fn
f'asin = unary g'number (modify (uop'flt asin))

-- | acos
f'acos :: Fn
f'acos = unary g'number (modify (uop'flt acos))

-- | atan
f'atan :: Fn
f'atan = unary g'number (modify (uop'flt atan))

-- | gcd
f'gcd :: Fn
f'gcd = binary g'integer (modify (uncurry (bop'int gcd)))

-- | lcm
f'lcm :: Fn
f'lcm = binary g'integer (modify (uncurry (bop'int lcm)))

-- | truncate
f'truncate :: Fn
f'truncate = unary g'real (modify (uop'realInt truncate))

-- | floor
f'floor :: Fn
f'floor = unary g'real (modify (uop'realInt floor))

-- | round
f'round :: Fn
f'round = unary g'real (modify (uop'realInt round))

-- | ceiling
f'ceiling :: Fn
f'ceiling = unary g'real (modify (uop'realInt ceiling))

-- | float
f'float :: Fn
f'float s = g'unary s >>= g'float

-- | abs
f'abs :: Fn
f'abs = unary g'number (modify (uop'num abs))

-- | signum
f'signum :: Fn
f'signum = unary g'number (modify (uop'num signum))

-- | (1+)
f'1p :: Fn
f'1p = unary pure (modify (uop'num (+ 1)))

-- | (1-)
f'1m :: Fn
f'1m = unary pure (modify (uop'num (subtract 1)))

-- | realpart
f'realpart :: Fn
f'realpart = unary g'number (modify (pure . Float . fst . toNum))

-- | imagpart
f'imagpart :: Fn
f'imagpart = unary g'number (modify (pure . Float . snd . toNum))

-- | conjugate
f'conjugate :: Fn
f'conjugate =
  unary g'number (modify (reduce . Complex . conjugate . unComplex))

-- | phase
f'phase :: Fn
f'phase = unary g'number (modify (pure . Float . phase . unComplex))

-- | random
f'random :: Fn
f'random = unary g'real random

-- | ash
f'ash :: Fn
f'ash = undefined

-- | atom
f'atom :: Fn
f'atom = pred' atom

-- | symbolp
f'symbolp :: Fn
f'symbolp = pred' symbolp

-- | numberp
f'numberp :: Fn
f'numberp = pred' numberp

-- | integerp
f'integerp :: Fn
f'integerp = pred' integerp

-- | rationalp
f'rationalp :: Fn
f'rationalp = pred' rationalp

-- | floatp
f'floatp :: Fn
f'floatp = pred' floatp

-- | realp
f'realp :: Fn
f'realp = pred' realp

-- | complexp
f'complexp :: Fn
f'complexp = pred' complexp

-- | zerop
f'zerop :: Fn
f'zerop = pred' zerop

-- | stringp
f'stringp :: Fn
f'stringp = pred' stringp

-- | listp
f'listp :: Fn
f'listp = pred' listp

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
f'vectorp = pred' vectorp

-- | hash-table-p
f'hashtablep :: Fn
f'hashtablep = pred' hashtablep

-- | oddp
f'oddp :: Fn
f'oddp =
  g'unary >=> eval >=> g'integer >=> modify (pure . true't . odd . unInt)

-- | evenp
f'evenp :: Fn
f'evenp =
  g'unary >=> eval >=> g'integer >=> modify (pure . true't . even . unInt)

-- | list
f'list :: Fn
f'list s = g'nary s >>= mapM' eval >>= modify (pure . List)

-- | cons
f'cons :: Fn
f'cons s = g'binary s >>= bimapM eval >>= cons

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
  put n t >>= g'integer >>= get >>= \(Int i) -> put l t >>= nth (fromIntegral i)

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

-- | ninth
f'ninth :: Fn
f'ninth s = g'unary s >>= eval >>= nth 8

-- | tenth
f'tenth :: Fn
f'tenth s = g'unary s >>= eval >>= nth 9

-- | rest
f'rest :: Fn
f'rest = f'cdr

-- | eq
f'eq :: Fn
f'eq s = g'binary s >>= bimapM eval >>= \t@(_, (a, b)) ->
  put (true't $ (typeOf a == typeOf b) && a == b) t

-- | equal
f'equal :: Fn
f'equal = f'eq

-- | append
f'append :: Fn
f'append s = g'nary s >>= mapM' eval >>= \t@(_, x) -> case x of
  []  -> put NIL t
  [a] -> put a t
  a   -> put (init a) t >>= modify (pure . List) >>= concat' >>= get >>= \case
    List xs -> case last a of
      List l -> put (xs <> l <> [NIL]) t >>= foldrM cons
      v      -> put (xs <> [v]) t >>= foldrM cons
    a -> err [errEval, errNotList, show' a]

-- | nthcdr
f'nthcdr :: Fn
f'nthcdr = undefined

-- | butlast
f'butlast :: Fn
f'butlast s = g'unary s >>= eval >>= \t@(_, x) -> case x of
  NIL     -> put NIL t
  List xs -> put xs t >>= modify (pure . List . init)
  a       -> err [errEval, errNotList, show' a]

-- | reverse
f'reverse :: Fn
f'reverse s = g'unary s >>= eval >>= \t@(_, x) -> case x of
  NIL     -> put NIL t
  List xs -> put xs t >>= modify (pure . List . reverse)
  a       -> err [errEval, errNotList, show' a]

-- | sort
f'sort :: Fn
f'sort s = g'unary s >>= eval >>= \t@(_, x) -> case x of
  NIL     -> put NIL t
  List xs -> put xs t >>= mapM' g'real >>= modify (pure . List . sort)
  a       -> err [errEval, errNotList, show' a]

-- | length
f'length :: Fn
f'length s = g'unary s >>= eval >>= \t@(_, x) -> case x of
  NIL     -> put (Int 0) t
  List xs -> put xs t >>= modify (pure . Int . fromIntegral . length)
  a       -> err [errEval, errNotSeq, show' a]

-- | make-hash-table
f'makeHashTable :: Fn
f'makeHashTable = undefined

-- | member
f'member :: Fn
f'member = undefined

-- | mapcar
f'mapcar :: Fn
f'mapcar s = g'nary s >>= mapM' eval >>= \t@(_, f : xs) -> case xs of
  a | not $ all listp' a ->
    err [errEval, errNotList, "NOT EVERY argument is a list"]
  a | all nilp a -> put NIL t
  a ->
    let nargs = length $ filter (not . nilp) a
        largs = filter ((== nargs) . length) (transpose (unList <$> a))
    in  put largs t
          >>= mapM' (modify (pure . (f :) . (Quote <$>)) >=> apply)
          >>= modify (pure . List)

-- | mapc
f'mapc :: Fn
f'mapc = undefined

-- | pop
f'pop :: Fn
f'pop = undefined

-- | push
f'push :: Fn
f'push = undefined

-- | defun
f'defun :: Fn
f'defun = undefined

-- | lambda
f'lambda :: Fn
f'lambda s = g'nary s >>= get >>= \case
  List args : body -> put args s >>= mapM' g'symbol >>= put
    (Function
      "lambda"
      (\t -> g'nary t >>= get >>= \case
        xs
          | length xs /= length args
          -> err [errEval, errWrongNargs, show' (List xs)]
          | otherwise
          -> put ([ List [a, b] | a <- args | b <- xs ]) t
            >>= bind'par
            >>= put body
            >>= eval'body
      )
    )
  _ -> err [errEval, errMalformed, "lambda"]

-- | progn
f'progn :: Fn
f'progn = g'nary >=> eval'body

-- | loop
f'loop :: Fn
f'loop = undefined

-- | do
f'do :: Fn
f'do = undefined

-- | dolist
f'dolist :: Fn
f'dolist = undefined

-- | dotimes
f'dotimes :: Fn
f'dotimes = undefined

-- | if
f'if :: Fn
f'if s = g'nary s >>= get >>= \case
  []              -> err [errEval, errNoArgs, "if"]
  [       a ]     -> err [errEval, errWrongNargs, "if,", show' a]
  a : if' : else' -> put a s >>= eval >>= t'nil >>= \t@(_, v) -> case v of
    NIL -> put else' t >>= eval'body
    _   -> put if' t >>= eval

-- | when
f'when :: Fn
f'when s = g'nary s >>= get >>= \case
  []       -> err [errEval, errNoArgs, "when"]
  [ a ]    -> put a s >>= eval >>= put NIL
  a : rest -> put a s >>= eval >>= t'nil >>= \t@(_, v) -> case v of
    NIL -> put NIL t
    _   -> put rest t >>= eval'body

-- | unless
f'unless :: Fn
f'unless s = g'nary s >>= get >>= \case
  []       -> err [errEval, errNoArgs, "unless"]
  [ a ]    -> put a s >>= eval >>= put NIL
  a : rest -> put a s >>= eval >>= nil't >>= \t@(_, v) -> case v of
    NIL -> put NIL t
    _   -> put rest t >>= eval'body

-- | cond
f'cond :: Fn
f'cond = undefined

-- | error
f'error :: Fn
f'error = undefined

-- | eval
f'eval :: Fn
f'eval s = g'unary s >>= eval >>= eval

-- | funcall
f'funcall :: Fn
f'funcall s = modify (pure . (<> [NIL])) s >>= f'apply

-- | apply
f'apply :: Fn
f'apply s = g'nary s >>= mapM' eval >>= \t@(_, x) -> case x of
  (f : args) ->
    put f t >>= g'symbol >>= put args >>= init' (show' f) >>= get >>= \i ->
      put args t >>= last' (show' f) >>= get >>= \case
        NIL    -> put (f : i) t >>= apply
        List l -> put (f : i <> l) t >>= apply
        e      -> err [errEval, errNotList, show' e]
  _ -> err [errEval, errNotAllowed, "f'apply"]

-- | type-of
f'typeOf :: Fn
f'typeOf s = g'unary s >>= eval >>= modify (pure . Symbol . typeOf)

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
  a             -> err [errEval, errWrongNargs, "tuple,", show (length a)]

-- | Guard for non-empty S-exp list
g'nempty :: String -> T [a] [a]
g'nempty msg s = get s >>= \case
  [] -> err [errEval, errNoArgs, msg]
  _  -> pure s

-- | Guard for non-empty S-exp list ([nil] if empty)
g'nempty' :: T [Sexp] [Sexp]
g'nempty' s = get s >>= \case
  [] -> put [NIL] s
  _  -> pure s

-- | Guard for bound symbols
g'bound :: T Sexp Sexp
g'bound s = eval s >>= g'symbol >>= get >>= \(Symbol k) -> from'venv k s

-- | Guard for symbols
g'symbol :: T Sexp Sexp
g'symbol s = get s >>= \case
  a | symbolp a -> pure s
  a             -> err [errEval, errNotSymbol, show' a]

-- | Guard for numbers
g'number :: T Sexp Sexp
g'number s = get s >>= \case
  a | numberp a -> pure s
  a             -> err [errEval, errNotNumber, show' a]

-- | Guard for boolean
g'bool :: T Sexp Sexp
g'bool s = get s >>= \case
  NIL    -> pure s
  Bool{} -> pure s
  a      -> err [errEval, errNotNumber, show' a]

-- | Guard for strings
g'string :: T Sexp Sexp
g'string s = get s >>= \case
  a | stringp a -> pure s
  a             -> err [errEval, errNotString, show' a]

-- | Guard for lists
g'list :: T Sexp Sexp
g'list s = get s >>= \case
  a | listp a -> pure s
  a           -> err [errEval, errNotList, show' a]

-- | Ensure that the state is an integer S-exp
g'integer :: T Sexp Sexp
g'integer s = g'number s >>= get >>= \case
  a | integerp a -> pure s
  a              -> err [errEval, errNotInteger, show' a]

-- | Ensure that the state is a rational S-exp
g'rational :: T Sexp Sexp
g'rational s = g'number s >>= get >>= \case
  a | rationalp a -> pure s
  a               -> err [errEval, errNotRational, show' a]

-- | Ensure that the state is a float S-exp
g'float :: T Sexp Sexp
g'float s = g'number s >>= get >>= \case
  a | floatp a -> pure s
  a            -> err [errEval, errNotFloat, show' a]

-- | Ensure that the state is a real S-exp
g'real :: T Sexp Sexp
g'real s = g'number s >>= get >>= \case
  a | realp a -> pure s
  a           -> err [errEval, errNotReal, show' a]

-- | Ensure that the state is a complex S-exp
g'complex :: T Sexp Sexp
g'complex s = g'number s >>= get >>= \case
  a | complexp a -> pure s
  a              -> err [errEval, errNotComplex, show' a]

-- | Ensure that the given key is not defined in the local env
g'undef'lkey :: String -> T Sexp Sexp
g'undef'lkey k s@(Env {..}, _) = case M.lookup k env'l of
  Just _  -> err [errEval, errManySymbol, k]
  Nothing -> pure s


----------
-- Core
----------

instance Eq Sexp where
  Bool    _ == Bool    _ = True
  Char    a == Char    b = a == b
  String  a == String  b = a == b
  Symbol  a == Symbol  b = a == b
  Keyword a == Keyword b = a == b
  a == b | nilp a && nilp b       = nilp a == nilp b
         | numberp a && numberp b = toNum a == toNum b
         | otherwise              = False


instance Ord Sexp where
  Bool    a <= Bool    b = a <= b
  Char    a <= Char    b = a <= b
  String  a <= String  b = a <= b
  Symbol  a <= Symbol  b = a <= b
  Keyword a <= Keyword b = a <= b
  a <= b | nilp a && nilp b   = nilp a <= nilp b
         | realp a && realp b = (fst . toNum $ a) <= (fst . toNum $ b)
         | otherwise          = False


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

-- | Check if the given S-exp is an atom
atom :: Sexp -> Bool
atom = \case
  a | nilp a -> True
  Bool{}     -> True
  Int{}      -> True
  Rational{} -> True
  Float{}    -> True
  Complex{}  -> True
  Char{}     -> True
  String{}   -> True
  Symbol{}   -> True
  Keyword{}  -> True
  _          -> False

-- | Check if the given S-exp is a symbol
symbolp :: Sexp -> Bool
symbolp = \case
  a | nilp a -> True
  Symbol{}   -> True
  _          -> False

-- | Check if the given S-exp is a string
stringp :: Sexp -> Bool
stringp = \case
  String{} -> True
  _        -> False

-- | Check if the given S-exp is a list
listp :: Sexp -> Bool
listp = \case
  a | listp' a -> True
  Cons{}       -> True
  _            -> False

-- | Check if the given S-exp is a proper list
listp' :: Sexp -> Bool
listp' = \case
  a | nilp a -> True
  List{}     -> True
  _          -> False

-- | Check if the given S-exp is a vector
vectorp :: Sexp -> Bool
vectorp = \case
  Vector{} -> True
  _        -> False

-- | Check if the given S-exp is a hash-table
hashtablep :: Sexp -> Bool
hashtablep = \case
  HashTable{} -> True
  _           -> False

-- | Check if the given S-exp is a number
numberp :: Sexp -> Bool
numberp x = realp x || complexp x

-- | Check if the given S-exp is a complex number
complexp :: Sexp -> Bool
complexp x = case reduce x of
  Left  _ -> False
  Right a -> case a of
    Complex{} -> True
    _         -> False

-- | Check if the given S-exp is equal to zero
zerop :: Sexp -> Bool
zerop x = numberp x && (fst . toNum $ x) == 0

-- | Complex number builder
complex :: Sexp -> Sexp -> Sexp
complex a b = case reduce c of
  Left  e -> die [errSys, e]
  Right x -> x
  where c = Complex (on (:+) (fst . toNum) a b)

-- | Check if the given S-exp is a real number
realp :: Sexp -> Bool
realp x = case reduce x of
  Left  _ -> False
  Right a -> integerp a || rationalp a || floatp a

-- | Check if the given S-exp is an integer
integerp :: Sexp -> Bool
integerp x = case reduce x of
  Left  _ -> False
  Right a -> case a of
    Int{} -> True
    _     -> False

-- | Check if the given S-exp is a rational number
rationalp :: Sexp -> Bool
rationalp = \case
  Rational{} -> True
  _          -> False

-- | Check if the given S-exp is a floating number
floatp :: Sexp -> Bool
floatp x = case reduce x of
  Left  _ -> False
  Right a -> case a of
    Float{} -> True
    _       -> False

-- | Convert true/false to LISP boolean notations t/nil
true't :: Bool -> Sexp
true't = \case
  True -> Bool True
  _    -> NIL

-- | Convert any S-exp to boolean expression t or nil
t'nil :: T Sexp Sexp
t'nil s = get s >>= \case
  a | nilp a -> put NIL s
  _          -> put (Bool True) s

-- | The same as 't`nil', but nil when S-exp is evaluated as true
nil't :: T Sexp Sexp
nil't s = get s >>= \case
  a | nilp a -> put (Bool True) s
  _          -> put NIL s

-- | Logical 'not'
not' :: Sexp -> Bool
not' = \case
  a | nilp a -> True
  _          -> False

-- | Logical 'or'
or' :: Sexp -> Sexp -> RE Sexp
or' a b = case (a, b) of
  (NIL, b) -> pure b
  (a  , _) -> pure a

-- | Logical 'and'
and' :: Sexp -> Sexp -> RE Sexp
and' a b = case (a, b) of
  (NIL, _) -> pure NIL
  (_  , b) -> pure b

-- | Unbox an S-exp number object and convet it into complex-form (partial)
toNum :: Sexp -> (Double, Double)
toNum = \case
  Int      x -> (fromIntegral x, 0)
  Rational x -> (fromRational x, 0)
  Float    x -> (x, 0)
  Complex  x -> (realPart x, imagPart x)
  a          -> die [errSys, show' a]

-- | Unbox an S-exp number object (partial)
unInt :: Sexp -> Integer
unInt = \case
  Int x -> x
  a     -> die [errSys, show' a]

-- | Unbox an S-exp number object (partial)
unRat :: Sexp -> Rational
unRat = \case
  Rational x -> x
  a          -> die [errSys, show' a]

-- | Unbox an S-exp number object (partial)
unFloat :: Sexp -> Double
unFloat = fst . toNum

-- | Unbox an S-exp number object (partial)
unComplex :: Sexp -> Complex Double
unComplex = uncurry (:+) . toNum

-- | Unary arithmetic (num) operator builder
uop'num :: (forall a . Num a => a -> a) -> Sexp -> RE Sexp
uop'num op = \case
  Int      a -> pure . Int $ op a
  Rational a -> reduce . Rational $ op a
  a          -> uop'flt op a

-- | Unary arithmetic (integral) operator builder
uop'int :: (forall a . Integral a => a -> a) -> Sexp -> RE Sexp
uop'int op x = pure . Int $ op . unInt $ x

-- | Unary arithmetic (real -> integral) operator builder
uop'realInt :: (forall a . RealFrac a => a -> Integer) -> Sexp -> RE Sexp
uop'realInt op = \case
  a -> pure . Int . op . fst . toNum $ a

-- | Unary arithmetic (floating) operator builder
uop'flt :: (forall a . (Num a, Floating a) => a -> a) -> Sexp -> RE Sexp
uop'flt op = \case
  Complex a -> reduce . Complex $ op a
  a         -> reduce . Float . op . fst . toNum $ a

-- | Binary arithmetic (num) operator builder
bop'num :: (forall a . Num a => a -> a -> a) -> Sexp -> Sexp -> RE Sexp
bop'num op x y = case (x, y) of
  (Int a, Int b) -> pure . Int $ a `op` b
  (a    , b    ) -> bop'frac op a b

-- | Binary arithmetic (ord) operator builder
bop'ord :: (forall a . Ord a => a -> a -> a) -> Sexp -> Sexp -> RE Sexp
bop'ord op x y = pure $ x `op` y

-- | Binary arithmetic (fractional) operator builder
bop'frac
  :: (forall a . (Num a, Fractional a) => a -> a -> a)
  -> Sexp
  -> Sexp
  -> RE Sexp
bop'frac op x y = case (x, y) of
  (Int a, Int b) -> reduce . Rational $ fromIntegral a `op` fromIntegral b
  (Int      a, Rational b) -> reduce . Rational $ fromIntegral a `op` b
  (Int      a, Float b   ) -> reduce . Float $ fromIntegral a `op` b
  (Rational a, Rational b) -> reduce . Rational $ a `op` b
  (Rational a, Float b   ) -> reduce . Float $ fromRational a `op` b
  (Float    a, Float b   ) -> reduce . Float $ a `op` b
  (Complex  a, b         ) -> reduce . Complex $ a `op` uncurry (:+) (toNum b)
  (a         , Complex b ) -> reduce . Complex $ uncurry (:+) (toNum a) `op` b
  (a         , b         ) -> bop'frac op b a

-- | Binary arithmetic (integral) operator builder
bop'int
  :: (forall a . (Ord a, Integral a) => a -> a -> a) -> Sexp -> Sexp -> RE Sexp
bop'int op x y = pure . Int $ unInt x `op` unInt y

-- | Binary arithmetic (real) operator builder
bop'real
  :: (forall a . (Num a, Real a) => a -> a -> a) -> Sexp -> Sexp -> RE Sexp
bop'real op x y = case (x, y) of
  (Int a, Int b) -> pure . Int $ a `op` b
  (a    , b    ) -> pure . Float $ (fst . toNum $ a) `op` (fst . toNum $ b)

-- | Binary arithmetic (floating) operator builder
bop'flt
  :: (forall a . (Num a, Floating a) => a -> a -> a) -> Sexp -> Sexp -> RE Sexp
bop'flt op x y = case (x, y) of
  (Int a, Int b) ->
    pure . Int . floor @Double $ fromIntegral a `op` fromIntegral b
  (Complex a, b        ) -> reduce . Complex $ a `op` uncurry (:+) (toNum b)
  (a        , Complex b) -> reduce . Complex $ uncurry (:+) (toNum a) `op` b
  (a        , b        ) -> reduce . Float $ on op (fst . toNum) a b

-- | Normalize (zero-imaginary) complex and (reducible) rational number if possible
reduce :: Sexp -> RE Sexp
reduce = \case
  a@Complex{} | (snd . toNum $ a) == 0 -> pure . Float . fst . toNum $ a
              | otherwise              -> pure a
  a@(Rational r) -> case (numerator r, denominator r) of
    (_, 0) -> err [errEval, errDivByZero]
    (n, d) | d == gcd n d -> pure . Int $ quot n d
           | otherwise    -> pure a
  a@Int{}   -> pure a
  a@Float{} -> pure a
  a         -> err [errEval, errNotNumber, show' a]

-- | Generate a uniform random number smaller than the given real number
random :: T Sexp Sexp
random s@(_, x) = g'real s >>= get'rng >>= \t@(env, g) ->
  let r a = randomR (0, a) g
      rand a = fst . r $ a
      next a = env { env'r = snd . r $ a }
  in  case x of
        Int a -> put (Int . rand $ a) t >>= put' (next a)
        a     -> put (Float . rand . fst . toNum $ a) t
          >>= put' (next . fst . toNum $ a)

-- | Get random number generator from the given Env
get'rng :: T a StdGen
get'rng s@(env, _) = from'genv' "*random-seed*" s >>= get >>= \case
  NIL -> put (env'r env) s
  a
    | integerp a
    -> let env'r = mkStdGen (fromIntegral . unInt $ a)
       in  put' (env { env'r }) s >>= put env'r >>= del'genv "*random-seed*"
    | otherwise
    -> err [errEval, "Invalid random-seed used:", show' a]

-- | Build functions to get a list item by index
nth :: Int -> T Sexp Sexp
nth i s = get s >>= \case
  List l -> case drop i l of
    []    -> put NIL s
    x : _ -> put x s
  a -> err [errEval, errMalformed, show' a]

-- | functional core of cons
cons :: T (Sexp, Sexp) Sexp
cons s = get s >>= \case
  (a, NIL   ) -> put (List [a]) s
  (a, List l) -> put (List (a : l)) s
  (a, b     ) -> put (Cons a b) s

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
pred' :: (Sexp -> Bool) -> T [Sexp] Sexp
pred' p s = g'unary s >>= eval >>= modify (pure . true't . p)

-- | Unary function builder
unary :: T Sexp Sexp -> T Sexp Sexp -> Fn
unary g f = g'unary >=> eval >=> g >=> f

-- | Binary function builder
binary :: T Sexp Sexp -> T (Sexp, Sexp) Sexp -> Fn
binary g f = g'binary >=> bimapM (eval >=> g) >=> f

-- | N-fold function builder (arithmetic)
nfold :: T Sexp Sexp -> (Sexp -> Sexp -> RE Sexp) -> String -> Fn
nfold g f msg = g'nary >=> mapM' (eval >=> g) >=> foldNum f msg

-- | N-fold function builder (logical)
nfold' :: T Sexp Sexp -> (Sexp -> Sexp -> Bool) -> String -> Fn
nfold' g op msg =
  g'nary >=> g'nempty msg >=> mapM' (eval >=> g) >=> \s@(_, l) ->
    put (and $ zipWith op l (tail l)) s >>= get >>= \case
      False -> put NIL s
      _     -> put (Bool True) s

-- | Fold S-exp numbers using the given binary arithemetic operator.
foldNum :: (Sexp -> Sexp -> RE Sexp) -> String -> Fn
foldNum f o s = get s >>= \case
  [] -> case o of
    "+" -> put (Int 0) s
    "*" -> put (Int 1) s
    _   -> err [errEval, errNoArgs, o]
  [x] -> case o of
    "-" -> put x s >>= modify (f (Int 0))
    "/" | zerop x   -> err [errEval, errDivByZero]
        | otherwise -> put x s >>= modify (f (Int 1))
    _ -> put x s
  (x : xs) -> case o of
    "/" | any zerop xs -> err [errEval, errDivByZero]
        | otherwise    -> put xs s >>= modify (foldM f x)

    _ -> put xs s >>= modify (foldM f x)

-- | Mirros 'Data.Bifunctor.bimap', but use one function when mapping over
bimap' :: (a -> b) -> (a, a) -> (b, b)
bimap' f (x, y) = (f x, f y)

-- | Map the list-form state result 'ST' to an action.
mapM' :: T a b -> T [a] [b]
mapM' f = go []
 where
  go r s@(_, xs) = case xs of
    []       -> put (reverse r) s
    x : rest -> put x s >>= f >>= \(env, x) -> go (x : r) (env, rest)

-- | Map the tuple-form state result to an action.
bimapM :: T a b -> T (a, a) (b, b)
bimapM f s@(_, (x, y)) =
  put x s >>= f >>= \t@(_, x) -> put y t >>= f >>= \u@(_, w) -> put (x, w) u

-- | Left-to-right monadic fold over the list-form state result with no base value.
foldlM :: T (a, a) a -> T [a] a
foldlM f s = get s >>= \case
  []       -> err [errEval, errNoArgs, "foldlM"]
  a : rest -> put rest s >>= go a
 where
  go a t@(_, xs) = case xs of
    []       -> put a t
    v : rest -> put (a, v) t >>= f >>= \(env, v) -> go v (env, rest)

-- | Right-to-left monadic fold over the list-form state result with no base value.
foldrM :: T (a, a) a -> T [a] a
foldrM f s = get s >>= \case
  [] -> err [errEval, errNoArgs, "foldrM"]
  a  -> put (init a) s >>= go (last a)
 where
  go a t@(_, xs) = case xs of
    []       -> put a t
    x : rest -> put rest t >>= go a >>= \u@(_, v) -> put (x, v) u >>= f

-- | Concatenates all the elements of the state result
concat' :: T Sexp Sexp
concat' s = get s >>= \case
  a | nilp a -> put NIL s
  List l     -> put l s >>= go []
  a          -> err [errEval, errNotList, show' a]
 where
  go r s@(_, xs) = case xs of
    []       -> put (concat . reverse $ r) s >>= modify (pure . List)
    x : rest -> case x of
      NIL    -> put rest s >>= go r
      List l -> put rest s >>= go (l : r)
      a      -> err [errEval, errNotList, show' a]

-- | Return the defined type of given S-exp
typeOf :: Sexp -> String
typeOf = \case
  Bool{}      -> "boolean"
  Int{}       -> "integer"
  Rational{}  -> "rational"
  Float{}     -> "float"
  Complex{}   -> "complex"
  Symbol{}    -> "symbol"
  Char{}      -> "char"
  String a    -> "char-array(" ++ show (length a) ++ ")"
  Quote{}     -> "cons"
  Backquote{} -> "cons"
  Cons{}      -> "cons"
  List{}      -> "cons"
  Vector{}    -> "vector"
  Function{}  -> "symbol"
  Macro{}     -> "symbol"
  a | nilp a    -> "null"
    | otherwise -> "undefined"

-- | Unbox an S-exp list object (partial)
unList :: Sexp -> [Sexp]
unList = \case
  NIL     -> []
  List xs -> xs
  a       -> die [errSys, show' a]

-- | Evaluate backquoted S-exp
eval'backquote :: Int -> T Sexp Sexp
eval'backquote d s = get s >>= \case
  a@At{}    -> err [errEval, errMalformed, show' a]
  a@Comma{} -> put a s >>= replace'cond d 1 id
  List [x]  -> put [x] s >>= splice d
  List xs   -> put xs s >>= splice d
  Backquote a ->
    put a s >>= eval'backquote (d + 1) >>= modify (pure . Backquote)
  a -> put a s

-- | Check if a given backquoted S-exp is replace'cond with the evalualed value
-- It will be replaced if it meets the conditions.
replace'cond :: Int -> Int -> (Sexp -> Sexp) -> T Sexp Sexp
replace'cond d n f s = get s >>= \x -> case compare' x d of
  LT -> pure s
  EQ -> replace d n f s
  GT -> err [errEval, errNotInBackquote, show' x]
 where
  compare' x = compare (count 0 x)
  count n = \case
    Comma a -> count (n + 1) a
    At    a -> count (n + 1) a
    _       -> n

-- | Substitute comma and at-sign with the evaluated value
replace :: Int -> Int -> (Sexp -> Sexp) -> T Sexp Sexp
replace d n f s = get s >>= \case
  Comma a@Comma{} -> put a s >>= replace d n (f . Comma)
  Comma a@At{}    -> put a s >>= replace d n (f . Comma)
  At    a@Comma{} -> put a s >>= replace d n (f . At)
  At    a@At{}    -> put a s >>= replace d n (f . At)
  Comma a         -> put a s >>= eval >>= modify (pure . f)
  At    a         -> put a s >>= eval >>= \t@(_, x) -> case x of
    List ls -> put ls t >>= mapM' (modify (pure . f)) >>= modify (pure . List)
    a | n > 1     -> err [errEval, errNotList, show' a]
      | otherwise -> pure t
  _ -> pure s

-- | Splice the given backquoted list
splice :: Int -> T [Sexp] Sexp
splice d = go []
 where
  go r s@(_, xs) = case xs of
    []       -> put (concat . reverse $ r) s >>= modify (pure . List)
    x : rest -> put x s >>= replace'cond d (length xs) id >>= \t@(_, v) ->
      case v of
        a@(List l) | spliceable x -> put rest t >>= go (l : r)
                   | otherwise    -> put rest t >>= go ([a] : r)
        a | length xs == 1 && spliceable x -> put a t
          | otherwise                      -> put rest t >>= go ([a] : r)

-- | Check if a given backquoted S-exp is spliceable
spliceable :: Sexp -> Bool
spliceable = \case
  Comma a@Comma{} -> spliceable a
  Comma a@At{}    -> spliceable a
  At    a@Comma{} -> spliceable a
  At    a@At{}    -> spliceable a
  Comma{}         -> False
  At{}            -> True
  _               -> False

-- | Evaluate body-part in a form
eval'body :: T [Sexp] Sexp
eval'body = mapM' eval >=> last_

-- | Sequentially bind a sequence (let*-like)
bind'seq :: T [Sexp] [Sexp]
bind'seq s = get s >>= \case
  [] -> pure s
  (List [Symbol k, a] : rest) ->
    put a s >>= eval >>= set'lenv k >>= put rest >>= bind'seq
  x@List{} : _ -> err [errEval, errMalformed, show' x]
  Quote x@(Symbol k) : rest ->
    put x s >>= from'venv k >>= put (List [x, x] : rest) >>= bind'seq
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
    put a s >>= local >>= f >>= put rest >>= eval'body >>= global s
  _ -> err [errEval, errMalformed, o]

-- | function builder of defining symbol values
defsym :: (String -> T Sexp Sexp) -> Fn
defsym f s = g'binary s >>= get >>= \case
  (x@(Symbol k), a) -> put a s >>= eval >>= f k >>= put x
  x                 -> err [errEval, errNotSymbol, show' (fst x)]

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
  , ("makunbound"           , f'makunbound)
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
  , ("numerator"            , f'numerator)
  , ("denominator"          , f'denominator)
  , ("rem"                  , f'rem)
  , ("gcd"                  , f'gcd)
  , ("lcm"                  , f'lcm)
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
  , ("truncate"             , f'truncate)
  , ("round"                , f'round)
  , ("ceiling"              , f'ceiling)
  , ("floor"                , f'floor)
  , ("float"                , f'float)
  , ("abs"                  , f'abs)
  , ("signum"               , f'signum)
  , ("1+"                   , f'1p)
  , ("1-"                   , f'1m)
  , ("realpart"             , f'realpart)
  , ("imagpart"             , f'imagpart)
  , ("phase"                , f'phase)
  , ("conjugate"            , f'conjugate)
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
  , ("length"               , f'length)
  , ("search"               , undefined)
  , ("subseq"               , undefined)
  , ("code-char"            , undefined)
  , ("char-code"            , undefined)
  , ("char"                 , undefined)
  -- REGEX
  , ("get-decoded-time"     , undefined)
  , ("get-universal-time"   , undefined)
  , ("decode-universal-time", undefined)
  , ("encode-universal-time", undefined)
  -- multiple-value-bind
  -- values
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
  , ("eq"                   , f'eq)
  , ("equal"                , f'equal)
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
  , ("append"               , f'append)
  , ("nthcdr"               , f'nthcdr)
  , ("butlast"              , f'butlast)
  , ("reverse"              , f'reverse)
  , ("sort"                 , f'sort)
  , ("remove-duplicates"    , undefined)
  , ("member"               , f'member)
  , ("mapcar"               , f'mapcar)
  , ("mapc"                 , f'mapc)
  , ("remove-if-not"        , undefined)
  , ("every"                , undefined)
  , ("some"                 , undefined)
  , ("push"                 , f'push)
  , ("pop"                  , f'pop)
  , ("assoc"                , undefined)
  , ("vector"               , undefined)
  , ("elt"                  , undefined)
  , ("aref"                 , undefined)
  , ("coerce"               , undefined)
  , ("map"                  , undefined)
  , ("make-hash-table"      , f'makeHashTable)
  , ("hash-table-count"     , undefined)
  , ("get-hash"             , undefined)
  , ("nth-value"            , undefined)
  , ("remhash"              , undefined)
  , ("maphash"              , undefined)
  , ("defun"                , f'defun)
  , ("lambda"               , f'lambda)
  , ("progn"                , f'progn)
  , ("prog1"                , undefined)
  , ("prog2"                , undefined)
  , ("loop"                 , f'loop)
  , ("do"                   , f'do)
  , ("dolist"               , f'dolist)
  , ("dotimes"              , f'dotimes)
  , ("if"                   , f'if)
  , ("when"                 , f'when)
  , ("unless"               , f'unless)
  , ("cond"                 , f'cond)
  , ("error"                , f'error)
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
  , ("type-of"              , f'typeOf)
  , ("describe"             , undefined)
  , ("atom"                 , f'atom)
  , ("symbolp"              , f'symbolp)
  , ("numberp"              , f'numberp)
  , ("integerp"             , f'integerp)
  , ("rationalp"            , f'rationalp)
  , ("floatp"               , f'floatp)
  , ("realp"                , f'realp)
  , ("complexp"             , f'complexp)
  , ("zerop"                , f'zerop)
  , ("oddp"                 , f'oddp)
  , ("evenp"                , f'evenp)
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
  NIL          -> "nil"
  Bool  _      -> "t"
  Int   intger -> show intger
  Float float  -> show float
  Rational rational ->
    show (numerator rational) ++ "/" ++ show (denominator rational)
  Complex complex ->
    "#C(" ++ unwords [show (realPart complex), show (imagPart complex)] ++ ")"
  Symbol    symbol    -> symbol
  Keyword   keyword   -> keyword
  Char      char      -> "#\\" ++ char
  String    string    -> "\"" ++ string ++ "\""
  Quote     sexp      -> "'" ++ show' sexp
  Backquote sexp      -> "`" ++ show' sexp
  At        sexp      -> ",@" ++ show' sexp
  Comma     sexp      -> "," ++ show' sexp
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


die :: [String] -> a
die = error . unwords

err :: [String] -> RE a
err = Left . unwords

errEval :: String
errEval = "*** Eval error ***"

errRepl :: String
errRepl = "*** REPL error ***"

errRead :: String
errRead = "*** Read error ***"

errSys :: String
errSys = "*** SYSTEM BROKEN *** Entering unreachable area. Fix it:"

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
errNotFloat = "Not a floating number:"

errNotRational :: String
errNotRational = "Not a rational number:"

errNotReal :: String
errNotReal = "Not a real number:"

errNotComplex :: String
errNotComplex = "Not a complex number:"

errNotList :: String
errNotList = "Not a list:"

errNotSeq :: String
errNotSeq = "Not a sequence:"

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

errNotInBackquote :: String
errNotInBackquote = "Not inside backquote:"

errNotAllowed :: String
errNotAllowed = "Operation not allowed:"


----------
-- REPL
----------
-- | REPL for SLISP
sl :: IO ()
sl = do
  seed        <- randomRIO (minBound :: Int, maxBound :: Int)
  historyFile <- getHomeDirectory <&> (</> ".slisp")
  runInputT (defaultSettings { historyFile = Just historyFile })
            (loop (init'env seed) print')
 where
  repl s@(env, _) printer str = case read' str >>= eval . (env, ) of
    Left  err      -> outputStrLn err >> loop s printer
    Right t@(_, x) -> printer x >> loop t printer

  loop s@(env, _) printer = do
    input <- getInputLine "SLISP> "
    case input of
      Nothing -> pure ()
      Just [] -> loop s print'
      Just ";" ->
        outputStrLn "Enabled paste-mode. Ctrl-d to finish"
          >>  mlLoop [] s printer
          >>= repl s printer
      Just ";;" ->
        outputStrLn "Enabled debug-mode. RET to quit" >> loop s pretty'
      Just ";;;"  -> d'symbolv env >> loop s printer
      Just ";;;;" -> d'symbolf env >> loop s printer
      Just str    -> repl s printer str

  mlLoop xs s printer = do
    input <- getInputLine "SLISP| "
    case input of
      Nothing -> pure . unlines . reverse $ xs
      Just x  -> mlLoop ((x ++ " ") : xs) s printer

-- Run SLISP externally: READ-EVAL
re :: String -> T Sexp Sexp
re stream s = read' stream >>= flip put s >>= eval

-- Run SLISP externally: READ-EVAL-PRINT
rep :: String -> IO ()
rep stream = do
  seed <- randomRIO (minBound :: Int, maxBound :: Int)
  case re stream (init'env seed) of
    Left  err    -> putStrLn err
    Right (_, x) -> putStrLn (show' x)


----------
-- Debug
----------
-- | Debug-mode printer
pretty' :: (MonadIO m, Pretty a) => a -> InputT m ()
pretty' = outputStrLn . TL.unpack . pretty

-- | blank line
__ :: MonadIO m => InputT m ()
__ = outputStrLn mempty

-- | Display symbol values in Env
d'symbolv :: MonadIO m => Env -> InputT m ()
d'symbolv Env {..} =
  __
    >> outputStrLn "*** random number generator state ***"
    >> outputStrLn (show env'r)
    >> __
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
