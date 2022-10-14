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
          | Boolean     Bool
          | Int         Integer
          | Float       Double
          | Symbol      String
          | Keyword     String
          | String      String
          | Quote       Sexp
          | Cons        Sexp Sexp
          | Seq         [Sexp]
          | List        [Sexp]
          | Vector      (V.Vector Sexp)
          | HashTable   (M.Map String Sexp)
          | Function    String Fn
          | Macro       String Fn

-- | Context of Result or Error
type RE = Either String

-- | Type for functions and macros
type Fn = ST [Sexp] -> RE (ST Sexp)


instance Eq Sexp where
  NIL         == NIL                 = True
  NIL         == Quote     NIL       = True
  NIL         == List      []        = True
  NIL         == Quote     (List []) = True
  Boolean   a == Boolean   b         = a == b
  Int       a == Int       b         = a == b
  Float     a == Float     b         = a == b
  Symbol    a == Symbol    b         = a == b
  Keyword   a == Keyword   b         = a == b
  String    a == String    b         = a == b
  Vector    a == Vector    b         = a == b
  HashTable a == HashTable b         = a == b
  _           == _                   = False


----------
-- Read
----------
-- | READ
read' :: String -> RE Sexp
read' input = case parse' sexp input of
  Ok ok (State stream _ _) | isEmpty stream -> pure ok
                           | otherwise      -> err [errRepl, errManySexp]
  Error _ -> err [errRead, errParsing]


type Parser = ParserS String

-- | S-expression
sexp :: Parser Sexp
sexp = between
  jump
  jump
  (choice [nil, str, bool, float, int, quote, key, sym, cons, vec, form])

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
-- | EVAL
eval :: ST Sexp -> RE (ST Sexp)
eval s = get s >>= \case
  Symbol k                 -> from'venv k s
  Quote  (List [])         -> put NIL s
  Quote  a                 -> put a s
  List   []                -> put NIL s
  List   es@(Symbol{} : _) -> put es s >>= apply
  List   (   a        : _) -> err [errEval, errInvalidFn, show' a]
  Seq    es                -> put es s >>= evalSeq
  c@Cons{}                 -> err [errEval, errInvalidFn, show' c]
  a                        -> put a s

-- | Apply the function of symbol name to the given arguments
apply :: Fn
apply s = head' s >>= get >>= \case
  Symbol k -> from'fenv k s >>= \(_, fn) -> fn s
  a        -> err [errEval, errNotSymbol, show' a]

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
  , ("or"                   , undefined)
  , ("not"                  , undefined)
  , ("and"                  , undefined)
  , ("="                    , undefined)
  , ("/="                   , undefined)
  , ("<"                    , undefined)
  , (">"                    , undefined)
  , ("<="                   , undefined)
  , (">="                   , undefined)
  , ("min"                  , undefined)
  , ("max"                  , undefined)
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
  , ("random"               , undefined)
  -- (setq *random-state* n)
  , ("ash"                  , undefined)
  -- logand
  -- logior
  -- logxor
  -- lognot
  -- BINARY-OCTAL-HEX LITERAL
  -- #b1010
  -- #o52
  -- #x2a
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
  , ("nineth"               , f'nineth)
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
  , ("dolist"               , undefined)
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
  , ("do"                   , undefined)
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
  , ("characterp"           , undefined)
  , ("alpha-char-p"         , undefined)
  , ("alphanumericp"        , undefined)
  , ("digit-char-p"         , undefined)
  , ("lower-case-p"         , undefined)
  , ("upper-case-p"         , undefined)
  , ("characterp"           , undefined)
  , ("boundp"               , f'boundp)
  , ("vectorp"              , undefined)
  , ("hash-table-p"         , undefined)
  , ("account-p"            , undefined)
  , ("macro-function"       , undefined)
  , ("typep"                , undefined)
  , ("symbol-value"         , f'symbolValue)
  , ("symbol-function"      , undefined)
  , ("symbol-plist"         , undefined)
  , ("function"             , undefined)
  ]

-- | set
f'set :: Fn
f'set s = g'binary s >>= evalList >>= \t@(_, e) -> case e of
  [Symbol k, a] -> put a t >>= set'venv k
  a             -> err [errEval, errNotSymbol, show' . head $ a]

-- | setq
f'setq :: Fn
f'setq s = g'evenary s >>= go
 where
  go x = get x >>= \case
    [Symbol k, a]       -> put a x >>= eval >>= set'venv k
    Symbol k : a : rest -> put a x >>= eval >>= set'venv k >>= put rest >>= go
    _                   -> err [errEval, errMalformed, "setq"]

-- | setf
f'setf :: Fn
f'setf _ = undefined

-- | getf
f'getf :: Fn
f'getf _ = undefined

-- | let
f'let :: Fn
f'let = deflet bindPar "let"

-- | let*
f'let' :: Fn
f'let' = deflet bindSeq "let*"

-- | defparameter
f'defparameter :: Fn
f'defparameter = defvar set'venv

-- | defvar
f'defvar :: Fn
f'defvar = defvar set'env'undef

-- | quote
f'quote :: Fn
f'quote = g'unary

-- | (+)
f'add :: Fn
f'add = nfold g'number (calb (+)) "+"

-- | (-)
f'sub :: Fn
f'sub = nfold g'number (calb (-)) "-"

-- | (*)
f'mul :: Fn
f'mul = nfold g'number (calb (*)) "*"

-- | (/)
f'div :: Fn
f'div = nfold g'number (calb (/)) "/"

-- | (%) or mod
f'mod :: Fn
f'mod = binary g'number (modify (uncurry (calb mod')))

-- | expt
f'expt :: Fn
f'expt = binary g'number (modify (uncurry (calb (**))))

-- | sqrt
f'sqrt :: Fn
f'sqrt = unary g'float (modify (calu sqrt))

-- | exp
f'exp :: Fn
f'exp = unary g'float (modify (calu exp))

-- | log
f'log :: Fn
f'log = unary g'float (modify (calu log))

-- | sin
f'sin :: Fn
f'sin = unary g'float (modify (calu sin))

-- | cos
f'cos :: Fn
f'cos = unary g'float (modify (calu cos))

-- | tan
f'tan :: Fn
f'tan = unary g'float (modify (calu tan))

-- | asin
f'asin :: Fn
f'asin = unary g'float (modify (calu asin))

-- | acos
f'acos :: Fn
f'acos = unary g'float (modify (calu acos))

-- | atan
f'atan :: Fn
f'atan = unary g'float (modify (calu atan))

-- | float
f'float :: Fn
f'float s = g'unary s >>= g'float

-- | abs
f'abs :: Fn
f'abs = unary pure (modify (calu abs))

-- | (1+)
f'1p :: Fn
f'1p = unary pure (modify (calu (+ 1)))

-- | (1-)
f'1m :: Fn
f'1m = unary pure (modify (calu (subtract 1)))

-- | atom
f'atom :: Fn
f'atom s = pred' s $ \case
  List{}       -> NIL
  Seq{}        -> NIL
  Quote List{} -> NIL
  Quote Seq{}  -> NIL
  _            -> Boolean True

-- | symbolp
f'symbolp :: Fn
f'symbolp s = pred' s $ \case
  Symbol{} -> Boolean True
  NIL      -> Boolean True
  _        -> NIL

-- | numberp
f'numberp :: Fn
f'numberp s = pred' s $ \case
  Int{}   -> Boolean True
  Float{} -> Boolean True
  _       -> NIL

-- | stringp
f'stringp :: Fn
f'stringp s = pred' s $ \case
  String{} -> Boolean True
  _        -> NIL

-- | listp
f'listp :: Fn
f'listp s = pred' s $ \case
  NIL    -> Boolean True
  Cons{} -> Boolean True
  List{} -> Boolean True
  _      -> NIL

-- | boundp
f'boundp :: Fn
f'boundp s = g'unary s >>= eval >>= g'symbol >>= \t@(_, Symbol k) ->
  case from'venv k t of
    Right _ -> put (Boolean True) t
    Left  _ -> put NIL t

-- | list
f'list :: Fn
f'list s = g'nary s >>= evalList >>= modify (pure . List)

-- | cons
f'cons :: Fn
f'cons s = g'binary s >>= evalList >>= \t@(_, e) -> case e of
  [a, NIL   ] -> put (List [a]) t
  [a, List l] -> put (List (a : l)) t
  [a, b     ] -> put (Cons a b) t
  _           -> err [errEval, errNotAllowed, "cons"]

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
f'cadr = g'unary >=> eval >=> car >=> cdr

-- | cdar
f'cdar :: Fn
f'cdar = g'unary >=> eval >=> cdr >=> car
-- | cddr
f'cddr :: Fn
f'cddr = g'unary >=> eval >=> cdr >=> cdr

-- | caaar
f'caaar :: Fn
f'caaar = g'unary >=> eval >=> car >=> car >=> car

-- | caadr
f'caadr :: Fn
f'caadr = g'unary >=> eval >=> car >=> car >=> cdr

-- | caadr
f'cadar :: Fn
f'cadar = g'unary >=> eval >=> car >=> cdr >=> car

-- | caadr
f'caddr :: Fn
f'caddr = g'unary >=> eval >=> car >=> cdr >=> cdr

-- | caadr
f'cdaar :: Fn
f'cdaar = g'unary >=> eval >=> cdr >=> car >=> car

-- | caadr
f'cdadr :: Fn
f'cdadr = g'unary >=> eval >=> cdr >=> car >=> cdr

-- | caadr
f'cddar :: Fn
f'cddar = g'unary >=> eval >=> cdr >=> cdr >=> car

-- | caadr
f'cdddr :: Fn
f'cdddr = g'unary >=> eval >=> cdr >=> cdr >=> cdr

-- | caaaar
f'caaaar :: Fn
f'caaaar = g'unary >=> eval >=> car >=> car >=> car >=> car

-- | caaaar
f'caaadr :: Fn
f'caaadr = g'unary >=> eval >=> car >=> car >=> car >=> cdr

-- | caaaar
f'caadar :: Fn
f'caadar = g'unary >=> eval >=> car >=> car >=> cdr >=> car

-- | caaaar
f'caaddr :: Fn
f'caaddr = g'unary >=> eval >=> car >=> car >=> cdr >=> cdr

-- | caaaar
f'cadaar :: Fn
f'cadaar = g'unary >=> eval >=> car >=> cdr >=> car >=> car

-- | caaaar
f'cadadr :: Fn
f'cadadr = g'unary >=> eval >=> car >=> cdr >=> car >=> cdr

-- | caaaar
f'caddar :: Fn
f'caddar = g'unary >=> eval >=> car >=> cdr >=> cdr >=> car

-- | caaaar
f'cadddr :: Fn
f'cadddr = g'unary >=> eval >=> car >=> cdr >=> cdr >=> cdr

-- | caaaar
f'cdaaar :: Fn
f'cdaaar = g'unary >=> eval >=> cdr >=> car >=> car >=> car

-- | caaaar
f'cdaadr :: Fn
f'cdaadr = g'unary >=> eval >=> cdr >=> car >=> car >=> cdr

-- | caaaar
f'cdadar :: Fn
f'cdadar = g'unary >=> eval >=> cdr >=> car >=> cdr >=> car

-- | caaaar
f'cdaddr :: Fn
f'cdaddr = g'unary >=> eval >=> cdr >=> car >=> cdr >=> cdr

-- | caaaar
f'cddaar :: Fn
f'cddaar = g'unary >=> eval >=> cdr >=> cdr >=> car >=> car

-- | caaaar
f'cddadr :: Fn
f'cddadr = g'unary >=> eval >=> cdr >=> cdr >=> car >=> cdr

-- | caaaar
f'cdddar :: Fn
f'cdddar = g'unary >=> eval >=> cdr >=> cdr >=> cdr >=> car

-- | caaaar
f'cddddr :: Fn
f'cddddr = g'unary >=> eval >=> cdr >=> cdr >=> cdr >=> cdr

-- | nth
f'nth :: Fn
f'nth s = g'binary s >>= \t@(_, [i, l]) ->
  put i t >>= eval >>= g'int >>= get >>= \(Int i) ->
    put l t >>= nth (fromIntegral i)

-- | first
f'first :: Fn
f'first s = g'unary s >>= nth 0

-- | second
f'second :: Fn
f'second s = g'unary s >>= nth 1

-- | third
f'third :: Fn
f'third s = g'unary s >>= nth 2

-- | fourth
f'fourth :: Fn
f'fourth s = g'unary s >>= nth 3

-- | fifth
f'fifth :: Fn
f'fifth s = g'unary s >>= nth 4

-- | sixth
f'sixth :: Fn
f'sixth s = g'unary s >>= nth 5

-- | seventh
f'seventh :: Fn
f'seventh s = g'unary s >>= nth 6

-- | eighth
f'eighth :: Fn
f'eighth s = g'unary s >>= nth 7

-- | nineth
f'nineth :: Fn
f'nineth s = g'unary s >>= nth 8

-- | tenth
f'tenth :: Fn
f'tenth s = g'unary s >>= nth 9

-- | rest
f'rest :: Fn
f'rest = f'cdr

-- | eval
f'eval :: Fn
f'eval s = g'unary s >>= eval >>= eval

-- | apply
f'apply :: Fn
f'apply = undefined
-- f'apply s = g'nary s >>= evalList >>= \t@(_, e) -> case e of
  -- (f : args) -> case put args t >>= last' of {}


-- | symbol-value
f'symbolValue :: Fn
f'symbolValue s = g'unary s >>= eval >>= g'symbol >>= eval


----------
-- Guard
----------
-- | Get the n-ary function's arguments
g'nary :: ST [Sexp] -> RE (ST [Sexp])
g'nary = arity (const True)

-- | Guard for unary function's arguments
g'unary :: ST [Sexp] -> RE (ST Sexp)
g'unary s = arity (== 1) s >>= head'

-- | Guard for binary function's arguments
g'binary :: ST [Sexp] -> RE (ST [Sexp])
g'binary = arity (== 2)

-- | Guard for even-ary(pairwise) function's arguments
g'evenary :: ST [Sexp] -> RE (ST [Sexp])
g'evenary = arity even >=> g'notNull "g'evenary"

-- | Guard for tuple state: transforms the result value into a tuple
g'tuple :: ST [Sexp] -> RE (ST (Sexp, Sexp))
g'tuple = \case
  s@(_, [x, y]) -> put (x, y) s
  a             -> err [errEval, errWrongNargs, show . length $ a]

-- | Guard for non-empty S-exp list
g'notNull :: String -> ST [a] -> RE (ST [a])
g'notNull caller s = get s >>= \case
  [] -> err [errEval, errNoArgs, caller]
  _  -> pure s

-- | Guard for bound symbols
g'bound :: ST Sexp -> RE (ST Sexp)
g'bound s = eval s >>= g'symbol >>= get >>= \(Symbol k) -> from'venv k s

-- | Guard for symbols
g'symbol :: ST Sexp -> RE (ST Sexp)
g'symbol s = get s >>= \case
  Symbol{} -> pure s
  a        -> err [errEval, errNotSymbol, show' a]

-- | Guard for numbers
g'number :: ST Sexp -> RE (ST Sexp)
g'number s = get s >>= \case
  Int{}   -> pure s
  Float{} -> pure s
  a       -> err [errEval, errNotNumber, show' a]

-- | Guard for strings
g'string :: ST Sexp -> RE (ST Sexp)
g'string s = get s >>= \case
  String{} -> pure s
  a        -> err [errEval, errNotString, show' a]

-- | Guard for lists
g'list :: ST Sexp -> RE (ST Sexp)
g'list s = get s >>= \case
  NIL    -> pure s
  Cons{} -> pure s
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

-- | Ensure that the state is a non-zero S-exp
g'nonzero :: ST Sexp -> RE (ST Sexp)
g'nonzero s = get s >>= \case
  Int   0 -> err [errEval, errDivByZero]
  Float 0 -> err [errEval, errDivByZero]
  _       -> pure s

-- | Ensure that the given key is not defined in the local env
g'nokey'lenv :: String -> ST Sexp -> RE (ST Sexp)
g'nokey'lenv k s@(Env {..}, _) = case M.lookup k env'l of
  Just _  -> err [errEval, errManySymbol, k]
  Nothing -> pure s


----------
-- Core
----------
-- | Evaluate a sequence
evalSeq :: ST [Sexp] -> RE (ST Sexp)
evalSeq s@(_, es) = case es of
  [e       ] -> put e s >>= eval
  (e : rest) -> put e s >>= eval >>= put rest >>= evalSeq
  []         -> put NIL s

-- | Evaluate a list
evalList :: ST [Sexp] -> RE (ST [Sexp])
evalList = go []
 where
  go r s@(_, es) = case es of
    e : rest -> put e s >>= eval >>= \(env, x) -> go (x : r) (env, rest)
    []       -> put (reverse r) s

-- | Parallelly bind a sequence (let-like)
bindPar :: ST [Sexp] -> RE (ST [Sexp])
bindPar s = get s >>= \case
  [] -> pure s
  (List [Symbol k, a] : rest) ->
    put a s
      >>= g'nokey'lenv k
      >>= xlocal
      >>= eval
      >>= global s
      >>= set'lenv k
      >>= put rest
      >>= bindPar
  x@List{} : _ -> err [errEval, errMalformed, show' x]
  Quote e@(Symbol k) : rest ->
    put e s >>= from'genv k >>= put (List [e, e] : rest) >>= bindPar
  a : rest -> put a s >>= g'symbol >>= put (List [a, NIL] : rest) >>= bindPar

-- | Sequentially bind a sequence (let*-like)
bindSeq :: ST [Sexp] -> RE (ST [Sexp])
bindSeq s = get s >>= \case
  [] -> pure s
  (List [Symbol k, a] : rest) ->
    put a s >>= eval >>= set'lenv k >>= put rest >>= bindSeq
  x@List{} : _ -> err [errEval, errMalformed, show' x]
  Quote e@(Symbol k) : rest ->
    put e s >>= from'genv k >>= put (List [e, e] : rest) >>= bindSeq
  a : rest -> put a s >>= g'symbol >>= put (List [a, NIL] : rest) >>= bindSeq

-- | let-function builder
deflet :: (ST [Sexp] -> RE (ST [Sexp])) -> String -> ST [Sexp] -> RE (ST Sexp)
deflet f o s = g'nary s >>= get >>= \case
  Quote e@Symbol{} : rest -> put (List [List [e, NIL]] : rest) s >>= deflet f o
  Quote (List a)   : rest -> put (List [List a] : rest) s >>= deflet f o
  List a : rest ->
    put a s >>= local >>= f >>= put (Seq rest) >>= eval >>= global s
  _ -> err [errEval, errMalformed, o]

-- | defvar-function builder
defvar :: (String -> ST Sexp -> RE (ST Sexp)) -> ST [Sexp] -> RE (ST Sexp)
defvar f s = g'binary s >>= get >>= \case
  [e@(Symbol k), a] -> put a s >>= eval >>= f k >>= put e
  x                 -> err [errEval, errNotSymbol, show' . head $ x]

-- | Predicate builder
pred' :: ST [Sexp] -> (Sexp -> Sexp) -> RE (ST Sexp)
pred' s p = g'unary s >>= eval >>= modify (pure . p)

-- | Unary arithmetic operator builder
calu
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a) -> Sexp -> RE Sexp
calu f = \case
  Int   a -> pure . Int . floor . f @Double . fromIntegral $ a
  Float a -> pure . Float . f $ a
  _       -> err [errEval, errNotAllowed, "calu"]

-- | Binary arithmetic operator builder
calb
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a -> a)
  -> Sexp
  -> Sexp
  -> RE Sexp
calb op x y = case (x, y) of
  (Int a, Int b) ->
    pure . Int . floor $ (op @Double) (fromIntegral a) (fromIntegral b)
  (Int   a, Float b) -> pure . Float $ fromIntegral a `op` b
  (Float a, Int b  ) -> pure . Float $ a `op` fromIntegral b
  (Float a, Float b) -> pure . Float $ a `op` b
  _                  -> err [errEval, errNotAllowed, "calb"]

-- | Unary arithmetic function builder
unary
  :: (ST Sexp -> RE (ST Sexp))
  -> (ST Sexp -> RE (ST Sexp))
  -> ST [Sexp]
  -> RE (ST Sexp)
unary g f = g'unary >=> eval >=> g >=> f

-- | Binary arithmetic function builder
binary
  :: (ST Sexp -> RE (ST Sexp))
  -> (ST (Sexp, Sexp) -> RE (ST Sexp))
  -> ST [Sexp]
  -> RE (ST Sexp)
binary g f = g'binary >=> evalList >=> mapM' g >=> g'tuple >=> f

-- | N-fold arithmetic function builder
nfold
  :: (ST Sexp -> RE (ST Sexp))
  -> (Sexp -> Sexp -> RE Sexp)
  -> String
  -> ST [Sexp]
  -> RE (ST Sexp)
nfold g f label = g'nary >=> evalList >=> mapM' g >=> fold' f label

-- | Fold arguments of a S-exp list using the given binary function
fold' :: (Sexp -> Sexp -> RE Sexp) -> String -> ST [Sexp] -> RE (ST Sexp)
fold' f o s = get s >>= \case
  [] -> case o of
    "+" -> put (Int 0) s
    "*" -> put (Int 1) s
    _   -> err [errEval, errNoArgs, o]
  [x] -> case o of
    "-" -> put x s >>= modify (f (Int 0))
    "/" -> put x s >>= g'nonzero >>= modify (f (Int 1))
    _   -> put x s
  (x : xs) -> case o of
    "/" -> put xs s >>= mapM' g'nonzero >>= modify (foldM f x)
    _   -> put xs s >>= modify (foldM f x)

-- | Build functions to control function's number of arguments
arity :: (Int -> Bool) -> ST [Sexp] -> RE (ST [Sexp])
arity p s@(_, e : args)
  | p nargs   = put args s
  | otherwise = err [errEval, errWrongNargs, show' e ++ ",", show nargs]
  where nargs = length args
arity _ _ = err [errEval, errNoArgs, "arity"]

-- | Build functions to get a list item by index
nth :: Int -> ST Sexp -> RE (ST Sexp)
nth i s = get s >>= \case
  List l -> case drop i l of
    []    -> put NIL s
    x : _ -> put x s >>= eval
  a -> err [errEval, errMalformed, show' a]

-- | functional core of cdr
car :: ST Sexp -> RE (ST Sexp)
car s = g'list s >>= get >>= \case
  Cons x _     -> put x s
  List (x : _) -> put x s
  _            -> put NIL s

-- | functional core of cdr
cdr :: ST Sexp -> RE (ST Sexp)
cdr s = g'list s >>= get >>= \case
  Cons _ y      -> put y s
  List (_ : xs) -> put (List xs) s
  _             -> put NIL s


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

-- | Head function for the state when the state result is a list
head' :: ST [a] -> RE (ST a)
head' s = g'notNull "head'" s >>= modify (pure . head)

-- | Tail function for the state when the state result is a list
tail' :: ST [a] -> RE (ST [a])
tail' s = g'notNull "tail'" s >>= modify (pure . tail)

-- | Last function for the state when the state result is a list
last' :: ST [a] -> RE (ST a)
last' s = g'notNull "last'" s >>= modify (pure . last)

-- | Init function for the state when the state result is a list
init' :: ST [a] -> RE (ST [a])
init' s = g'notNull "init'" s >>= modify (pure . init)


-- | Map the state result to an action.
-- This map is only valid when the result has multiple value, i.e., a list
mapM' :: (ST a -> RE (ST a)) -> ST [a] -> RE (ST [a])
mapM' f s@(env, _) = mapM f (sequence s) >>= sequence' >>= put' env
 where
  sequence' xs = put (go [] xs) init'env
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
  , env'f :: M.Map String Fn
  }
  deriving Show

instance Semigroup Env where
  (<>) = undefined

instance Monoid Env where
  mempty = Env mempty mempty mempty

-- |
init'env :: ST Sexp
init'env = (Env mempty mempty (M.fromList built'in), NIL)

-- |
set'venv :: String -> ST Sexp -> RE (ST Sexp)
set'venv k s@(Env {..}, _) | M.member k env'l = set'lenv k s
                           | otherwise        = set'genv k s
-- |
set'genv :: String -> ST Sexp -> RE (ST Sexp)
set'genv k s@(env@Env {..}, e) = put' (env { env'g = M.insert k e env'g }) s

-- |
set'lenv :: String -> ST Sexp -> RE (ST Sexp)
set'lenv k s@(env@Env {..}, e) = put' (env { env'l = M.insert k e env'l }) s

-- |
set'fenv :: String -> ST Fn -> RE (ST Fn)
set'fenv k s@(env@Env {..}, e) = put' (env { env'f = M.insert k e env'f }) s

-- | The same as `set'venv`, but set only when keys are not defined
set'env'undef :: String -> ST Sexp -> RE (ST Sexp)
set'env'undef k s@(Env {..}, _) = case M.lookup k env'g of
  Just _  -> pure s
  Nothing -> set'venv k s

-- | Get S-exp from the env by a symbol-value key
from'venv :: String -> ST a -> RE (ST Sexp)
from'venv k s = from'lenv k s <|> from'genv k s

-- | Get S-exp from the global-env by a symbol-value key
from'genv :: String -> ST a -> RE (ST Sexp)
from'genv = getter env'g errVoidSymbolVar

-- | Get S-exp from the local-env by a symbol-value key
from'lenv :: String -> ST a -> RE (ST Sexp)
from'lenv = getter env'l errVoidSymbolVar

-- | Get functions from the fn-env by a symbol-funtion key
from'fenv :: String -> ST a -> RE (ST Fn)
from'fenv = getter env'f errVoidSymbolFn

-- | Env getters builder
getter :: (Env -> M.Map String b) -> String -> String -> ST a -> RE (ST b)
getter f msg k s@(env, _) = case M.lookup k (f env) of
  Just v  -> put v s
  Nothing -> err [errEval, msg, k]

-- | When going into local-scope
local :: ST a -> RE (ST a)
local s@(env@Env {..}, _) =
  put' (env { env'g = env'l <> env'g, env'l = mempty }) s

-- | When getting out from local-scope
global :: ST b -> ST a -> RE (ST a)
global (g@Env{}, _) s@(l@Env{}, _) = put' (g { env'g = env'g l }) s

-- | Deactivate local env and use only global env
xlocal :: ST a -> RE (ST a)
xlocal s@(env@Env{}, _) = put' (env { env'l = mempty }) s


----------
-- Print
----------
-- | PRINT
print' :: MonadIO m => Sexp -> InputT m ()
print' = outputStrLn . show'

-- | Stringify S-expression
show' :: Sexp -> String
show' = \case
  NIL             -> "nil"
  Int     intger  -> show intger
  Float   float   -> show float
  Symbol  symbol  -> symbol
  Keyword keyword -> keyword
  String  string  -> "\"" ++ string ++ "\""
  Quote   sexp    -> "'" ++ show' sexp
  Boolean bool | bool      -> "t"
               | otherwise -> "nil"
  Seq       seq       -> show' (List seq)
  Vector    vector    -> "#(" ++ unwords (show' <$> V.toList vector) ++ ")"
  HashTable map       -> show map
  Function name fn    -> unwords [show fn, name]
  Macro    name macro -> unwords [show macro, name]
  List list           -> case list of
    [] -> "nil"
    _  -> "(" ++ unwords (show' <$> list) ++ ")"
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
errNoArgs = "No arguments, nothing to apply:"

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
      Just ";"    -> outputStrLn "Set to paste-mode." -- loop s paste
      Just ";;"   -> outputStrLn "Set to debug-mode." >> loop s debug
      Just ";;;"  -> d'symbolv env >> loop s mode
      Just ";;;;" -> d'symbolf env >> loop s mode
      Just str    -> case reader str >>= eval . (env, ) of
        Left  err      -> outputStrLn err >> loop s mode
        Right t@(_, e) -> printer e >> loop t mode

-- Run SLISP externally: READ-EVAL
re :: String -> RE (ST Sexp)
re stream = read' stream >>= flip put init'env >>= eval

-- Run SLISP externally: READ-EVAL-PRINT
rep :: String -> IO ()
rep stream = do
  case re stream of
    Left  err    -> putStrLn err
    Right (_, e) -> putStrLn . show' $ e


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
d'symbolv env@Env {..} =
  __
    >> outputStrLn "global symbol values:"
    >> pretty' (M.toList env'g)
    >> __
    >> outputStrLn "local symbol values (must be empty):"
    >> pretty' (M.toList env'l)
    >> __

-- | Display symbol functions in Env
d'symbolf :: MonadIO m => Env -> InputT m ()
d'symbolf env@Env {..} =
  __
    >> mapM_ outputStrLn
             ((\(a, b) -> unwords [show b, "\t", a]) <$> M.toList env'f)
    >> __
