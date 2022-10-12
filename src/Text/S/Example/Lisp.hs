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
import           Text.S.Combinator              ( between
                                                , choice
                                                , many
                                                , option
                                                )
import           Text.S.Internal                ( ($>)
                                                , Parser
                                                , Pretty(pretty)
                                                , Result(Error, Ok)
                                                , State(State)
                                                , Stream(isEmpty)
                                                , Text
                                                , parse'
                                                , try
                                                , void
                                                )
import           Text.S.Language                ( lispdef )
import           Text.S.Lexeme                  ( float
                                                , gap
                                                , identifier
                                                , integer
                                                , skips
                                                , stringLit
                                                , symbol
                                                )


-- | S-exp AST
data Sexp = NIL
          | Boolean     Bool
          | Int         Integer
          | Float       Double
          | Symbol      String
          | Keyword     String
          | StringLit   String
          | Quote       Sexp
          | Cons        Sexp Sexp
          | Seq         [Sexp]
          | List        [Sexp]
          | Vector      (V.Vector Sexp)
          | HashTable   (M.Map String Sexp)
          | Fn          [Sexp]
          | Macro       [Sexp]
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
  (choice [nil, str, bool, flt, int, quote, key, sym, cons, vec, form])

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
flt :: Parser Sexp
flt = Float <$> (option "0" (symbol ".") *> float <* end)

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
vec = Vector . V.fromList <$> between (symbol "#(") (symbol ")") (many sexp)

-- | Cons
cons :: Parser Sexp
cons = between (symbol "(") (symbol ")") pair
  where pair = sexp >>= \a -> symbol "." *> sexp >>= \b -> pure $ Cons a b

-- | Form
form :: Parser Sexp
form = List <$> between (symbol "(") (symbol ")") (many sexp)


----------
-- Eval
----------
-- | EVAL
eval :: ST Sexp -> RE (ST Sexp)
eval s = get s >>= \case
  Symbol k                 -> from'env k s
  Quote  (List [])         -> put NIL s
  Quote  a                 -> put a s
  List   []                -> put NIL s
  List   es@(Symbol{} : _) -> put es s >>= apply
  List   (   a        : _) -> err [errEval, errInvalidFn, show' a]
  Seq    es                -> put es s >>= evalSeq
  c@Cons{}                 -> err [errEval, errInvalidFn, show' c]
  a                        -> put a s

-- |
apply :: ST [Sexp] -> RE (ST Sexp)
apply s = head' s >>= get >>= \case
  Symbol "set"                   -> f'set s
  Symbol "setq"                  -> f'setq s
  Symbol "setf"                  -> f'setf s
  Symbol "getf"                  -> undefined
  Symbol "let"                   -> f'let s
  Symbol "let*"                  -> f'let' s
  Symbol "defparameter"          -> f'defparameter s
  Symbol "defvar"                -> f'defvar s
  Symbol "makeunbound"           -> undefined
  Symbol "quote"                 -> f'quote s
  Symbol "or"                    -> undefined
  Symbol "not"                   -> undefined
  Symbol "and"                   -> undefined
  Symbol "="                     -> undefined
  Symbol "/="                    -> undefined
  Symbol "<"                     -> undefined
  Symbol ">"                     -> undefined
  Symbol "<="                    -> undefined
  Symbol ">="                    -> undefined
  Symbol "min"                   -> undefined
  Symbol "max"                   -> undefined
  Symbol "+"                     -> f'add s
  Symbol "-"                     -> f'sub s
  Symbol "*"                     -> f'mul s
  Symbol "/"                     -> f'div s
  Symbol "mod"                   -> f'mod s
  -- DIVISION-BY-ZERO
  -- numerator
  -- denominator
  Symbol "rem"                   -> undefined
  Symbol "expt"                  -> f'expt s
  Symbol "sqrt"                  -> f'sqrt s
  Symbol "exp"                   -> f'exp s
  Symbol "log"                   -> f'log s
  Symbol "sin"                   -> f'sin s
  Symbol "cos"                   -> f'cos s
  Symbol "tan"                   -> f'tan s
  Symbol "asin"                  -> f'asin s
  Symbol "acos"                  -> f'acos s
  Symbol "atan"                  -> f'atan s
  Symbol "truncate"              -> undefined
  Symbol "round"                 -> undefined
  Symbol "ceiling"               -> undefined
  Symbol "floor"                 -> undefined
  Symbol "float"                 -> f'float s
  Symbol "abs"                   -> f'abs s
  Symbol "signum"                -> undefined
  Symbol "1+"                    -> f'1p s
  Symbol "1-"                    -> f'1m s
  -- COMPLEX-NUMBER
  -- realpart #c
  -- imagpart #c
  -- phase #c
  -- abs #c
  -- conjugate #c
  Symbol "random"                -> undefined
  -- (setq *random-state* n)
  Symbol "ash"                   -> undefined
  -- logand
  -- logior
  -- logxor
  -- lognot
  -- BINARY-OCTAL-HEX LITERAL
  -- #b1010
  -- #o52
  -- #x2a
  Symbol "format"                -> undefined
  -- \\ \" LITERAL ESCAPE
  Symbol "string="               -> undefined
  Symbol "string<"               -> undefined
  -- concatenate 'string 'list
  Symbol "concatenate"           -> undefined
  Symbol "string-downcase"       -> undefined
  Symbol "string-upcase"         -> undefined
  Symbol "string-capitalize"     -> undefined
  Symbol "string-trim"           -> undefined
  -- cl-ppcre:split
  Symbol "reduce"                -> undefined
  Symbol "length"                -> undefined
  Symbol "search"                -> undefined
  Symbol "subseq"                -> undefined
  -- CHARACTER LITERAL
  -- #\a #\space #\newline #\tab ..
  Symbol "code-char"             -> undefined
  Symbol "char-code"             -> undefined
  Symbol "char"                  -> undefined
  -- REGEX
  Symbol "get-decoded-time"      -> undefined
  Symbol "get-universal-time"    -> undefined
  Symbol "decode-universal-time" -> undefined
  Symbol "encode-universal-time" -> undefined
  -- multiple-value-bind
  Symbol "list"                  -> f'list s
  Symbol "cons"                  -> f'cons s
  Symbol "car"                   -> undefined
  Symbol "cdr"                   -> undefined
  Symbol "equal"                 -> undefined
  Symbol "nth"                   -> f'nth s
  Symbol "first"                 -> f'first s
  Symbol "second"                -> f'second s
  Symbol "third"                 -> f'third s
  Symbol "fourth"                -> f'fourth s
  Symbol "fifth"                 -> f'fifth s
  Symbol "sixth"                 -> f'sixth s
  Symbol "seventh"               -> f'seventh s
  Symbol "eighth"                -> f'eighth s
  Symbol "nineth"                -> f'nineth s
  Symbol "tenth"                 -> f'tenth s
  Symbol "rest"                  -> undefined
  Symbol "position"              -> undefined
  Symbol "append"                -> undefined
  Symbol "nthcdr"                -> undefined
  Symbol "butlast"               -> undefined
  Symbol "reverse"               -> undefined
  Symbol "sort"                  -> undefined
  Symbol "remove-duplicates"     -> undefined
  Symbol "member"                -> undefined
  Symbol "mapcar"                -> undefined
  Symbol "remove-if-not"         -> undefined
  Symbol "dolist"                -> undefined
  Symbol "every"                 -> undefined
  Symbol "some"                  -> undefined
  Symbol "push"                  -> undefined
  Symbol "pop"                   -> undefined
  Symbol "assoc"                 -> undefined
  Symbol "vector"                -> undefined
  Symbol "elt"                   -> undefined
  Symbol "aref"                  -> undefined
  Symbol "coerce"                -> undefined
  Symbol "map"                   -> undefined
  Symbol "make-hash-table"       -> undefined
  Symbol "hash-table-count"      -> undefined
  Symbol "get-hash"              -> undefined
  Symbol "nth-value"             -> undefined
  Symbol "remhash"               -> undefined
  Symbol "maphash"               -> undefined
  Symbol "defstruct"             -> undefined
  Symbol "account-id"            -> undefined
  -- MULTIPLE-VALUES: 'skip
  -- defclass
  -- OBJECTS: 'skip
  Symbol "defun"                 -> undefined
  Symbol "lambda"                -> undefined
  Symbol "progn"                 -> undefined
  Symbol "prog1"                 -> undefined
  Symbol "prog2"                 -> undefined
  Symbol "loop"                  -> undefined
  Symbol "do"                    -> undefined
  Symbol "dotimes"               -> undefined
  Symbol "if"                    -> undefined
  Symbol "when"                  -> undefined
  Symbol "cond"                  -> undefined
  Symbol "error"                 -> undefined
  -- EXCEPTIONS: 'skip
  -- handler-case
  -- define-condition
  -- handler-bind
  -- unwind-protect
  Symbol "open"                  -> undefined
  Symbol "close"                 -> undefined
  Symbol "with-open-file"        -> undefined
  Symbol "read-line"             -> undefined
  Symbol "load"                  -> undefined
  -- STREAM
  -- param: *standard-input*
  -- param: *standard-output*
  -- param: *error-output*
  -- make-string-input-stream
  -- make-string-output-stream
  Symbol "eval"                  -> undefined
  Symbol "defmacro"              -> undefined
  Symbol "macroexpand"           -> undefined
  Symbol "type-of"               -> undefined
  Symbol "describe"              -> undefined
  Symbol "atom"                  -> f'atom s
  Symbol "symbolp"               -> f'symbolp s
  Symbol "numberp"               -> f'numberp s
  Symbol "integerp"              -> undefined
  Symbol "rationalp"             -> undefined
  Symbol "floatp"                -> undefined
  Symbol "realp"                 -> undefined
  Symbol "complexp"              -> undefined
  Symbol "zerop"                 -> undefined
  Symbol "stringp"               -> f'stringp s
  Symbol "listp"                 -> f'listp s
  Symbol "characterp"            -> undefined
  Symbol "alpha-char-p"          -> undefined
  Symbol "alphanumericp"         -> undefined
  Symbol "digit-char-p"          -> undefined
  Symbol "lower-case-p"          -> undefined
  Symbol "upper-case-p"          -> undefined
  Symbol "characterp"            -> undefined
  Symbol "boundp"                -> f'boundp s
  Symbol "vectorp"               -> undefined
  Symbol "hash-table-p"          -> undefined
  Symbol "account-p"             -> undefined
  Symbol "macro-function"        -> undefined
  Symbol "typep"                 -> undefined
  Symbol "symbol-value"          -> f'symbolValue s
  Symbol "symbol-plist"          -> undefined
  Symbol k                       -> err [errEval, errVoidSymbolFn, k]
  a                              -> err [errEval, errNotAllowed, "apply"]


-- | set
f'set :: ST [Sexp] -> RE (ST Sexp)
f'set s = undefined

-- | setq
f'setq :: ST [Sexp] -> RE (ST Sexp)
f'setq s = undefined

-- | setf
f'setf :: ST [Sexp] -> RE (ST Sexp)
f'setf s = undefined

-- | let
f'let :: ST [Sexp] -> RE (ST Sexp)
f'let = deflet bindPar "let"

-- | let*
f'let' :: ST [Sexp] -> RE (ST Sexp)
f'let' = deflet bindSeq "let*"

-- | defparameter
f'defparameter :: ST [Sexp] -> RE (ST Sexp)
f'defparameter = defvar set'env

-- | defvar
f'defvar :: ST [Sexp] -> RE (ST Sexp)
f'defvar = defvar set'env'undef

-- | quote
f'quote :: ST [Sexp] -> RE (ST Sexp)
f'quote = g'unary

-- | (+)
f'add :: ST [Sexp] -> RE (ST Sexp)
f'add = nfold g'number (calb (+)) "+"

-- | (-)
f'sub :: ST [Sexp] -> RE (ST Sexp)
f'sub = nfold g'number (calb (-)) "-"

-- | (*)
f'mul :: ST [Sexp] -> RE (ST Sexp)
f'mul = nfold g'number (calb (*)) "*"

-- | (/)
f'div :: ST [Sexp] -> RE (ST Sexp)
f'div = nfold g'number (calb (/)) "/"

-- | (%) or mod
f'mod :: ST [Sexp] -> RE (ST Sexp)
f'mod = binary g'number (modify (uncurry (calb mod')))

-- | expt
f'expt :: ST [Sexp] -> RE (ST Sexp)
f'expt = binary g'number (modify (uncurry (calb (**))))

-- | sqrt
f'sqrt :: ST [Sexp] -> RE (ST Sexp)
f'sqrt = unary g'float (modify (calu sqrt))

-- | exp
f'exp :: ST [Sexp] -> RE (ST Sexp)
f'exp = unary g'float (modify (calu exp))

-- | log
f'log :: ST [Sexp] -> RE (ST Sexp)
f'log = unary g'float (modify (calu log))

-- | sin
f'sin :: ST [Sexp] -> RE (ST Sexp)
f'sin = unary g'float (modify (calu sin))

-- | cos
f'cos :: ST [Sexp] -> RE (ST Sexp)
f'cos = unary g'float (modify (calu cos))

-- | tan
f'tan :: ST [Sexp] -> RE (ST Sexp)
f'tan = unary g'float (modify (calu tan))

-- | asin
f'asin :: ST [Sexp] -> RE (ST Sexp)
f'asin = unary g'float (modify (calu asin))

-- | acos
f'acos :: ST [Sexp] -> RE (ST Sexp)
f'acos = unary g'float (modify (calu acos))

-- | atan
f'atan :: ST [Sexp] -> RE (ST Sexp)
f'atan = unary g'float (modify (calu atan))

-- | float
f'float :: ST [Sexp] -> RE (ST Sexp)
f'float s = g'unary s >>= g'float

-- | abs
f'abs :: ST [Sexp] -> RE (ST Sexp)
f'abs = unary pure (modify (calu abs))

-- | (1+)
f'1p :: ST [Sexp] -> RE (ST Sexp)
f'1p = unary pure (modify (calu (+ 1)))

-- | (1-)
f'1m :: ST [Sexp] -> RE (ST Sexp)
f'1m = unary pure (modify (calu (subtract 1)))

-- | atom
f'atom :: ST [Sexp] -> RE (ST Sexp)
f'atom s = pred' s $ \case
  List{}       -> NIL
  Seq{}        -> NIL
  Quote List{} -> NIL
  Quote Seq{}  -> NIL
  _            -> Boolean True

-- | symbolp
f'symbolp :: ST [Sexp] -> RE (ST Sexp)
f'symbolp s = pred' s $ \case
  Symbol{} -> Boolean True
  NIL      -> Boolean True
  _        -> NIL

-- | numberp
f'numberp :: ST [Sexp] -> RE (ST Sexp)
f'numberp s = pred' s $ \case
  Int{}   -> Boolean True
  Float{} -> Boolean True
  _       -> NIL

-- | stringp
f'stringp :: ST [Sexp] -> RE (ST Sexp)
f'stringp s = pred' s $ \case
  StringLit{} -> Boolean True
  _           -> NIL

-- | listp
f'listp :: ST [Sexp] -> RE (ST Sexp)
f'listp s = pred' s $ \case
  NIL    -> Boolean True
  List{} -> Boolean True
  _      -> NIL

-- | boundp
f'boundp :: ST [Sexp] -> RE (ST Sexp)
f'boundp s = g'unary s >>= eval >>= g'symbol >>= \s'@(_, Symbol k) ->
  case from'env k s' of
    Right _ -> put (Boolean True) s'
    Left  _ -> put NIL s'

-- | list
f'list :: ST [Sexp] -> RE (ST Sexp)
f'list s = g'nary s >>= evalList >>= modify (pure . List)

-- | cons
f'cons :: ST [Sexp] -> RE (ST Sexp)
f'cons s = g'binary s >>= evalList >>= get >>= \case
  [a, NIL   ] -> put (List [a]) s
  [a, List l] -> put (List (a : l)) s
  [a, b     ] -> put (Cons a b) s
  _           -> err [errEval, errNotAllowed, "cons"]

-- | nth
f'nth :: ST [Sexp] -> RE (ST Sexp)
f'nth s = g'binary s >>= \t@(_, [i, l]) ->
  put i t >>= eval >>= g'int >>= get >>= \(Int i) ->
    put l t >>= nth (fromIntegral i)

-- |
f'first :: ST [Sexp] -> RE (ST Sexp)
f'first s = g'unary s >>= nth 0

-- |
f'second :: ST [Sexp] -> RE (ST Sexp)
f'second s = g'unary s >>= nth 1

-- |
f'third :: ST [Sexp] -> RE (ST Sexp)
f'third s = g'unary s >>= nth 2

-- |
f'fourth :: ST [Sexp] -> RE (ST Sexp)
f'fourth s = g'unary s >>= nth 3

-- |
f'fifth :: ST [Sexp] -> RE (ST Sexp)
f'fifth s = g'unary s >>= nth 4

-- |
f'sixth :: ST [Sexp] -> RE (ST Sexp)
f'sixth s = g'unary s >>= nth 5

-- |
f'seventh :: ST [Sexp] -> RE (ST Sexp)
f'seventh s = g'unary s >>= nth 6

-- |
f'eighth :: ST [Sexp] -> RE (ST Sexp)
f'eighth s = g'unary s >>= nth 7

-- |
f'nineth :: ST [Sexp] -> RE (ST Sexp)
f'nineth s = g'unary s >>= nth 8

-- |
f'tenth :: ST [Sexp] -> RE (ST Sexp)
f'tenth s = g'unary s >>= nth 9

-- | symbol-value
f'symbolValue :: ST [Sexp] -> RE (ST Sexp)
f'symbolValue = unary g'symbol eval


----------
-- Guard
----------
-- | Get the n-ary function's arguments
g'nary :: ST [Sexp] -> RE (ST [Sexp])
g'nary = arity (const True)

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
  a             -> err [errEval, errWrongNargs]

-- | Guard for non-empty S-exp list
g'notNull :: String -> ST [a] -> RE (ST [a])
g'notNull caller s = get s >>= \case
  [] -> err [errEval, errNoArgs, caller]
  _  -> pure s

-- | Guard for bound symbols
g'bound :: ST Sexp -> RE (ST Sexp)
g'bound s = eval s >>= g'symbol >>= get >>= \(Symbol k) -> from'env k s

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
  StringLit{} -> pure s
  a           -> err [errEval, errNotString, show' a]

-- | Guard for lists
g'list :: ST Sexp -> RE (ST Sexp)
g'list s = get s >>= \case
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
  go r s@(env, es) = case es of
    e : rest -> put e s >>= eval >>= \(env', e') -> go (e' : r) (env', rest)
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
  Int   a -> pure . Int . floor . f $ fromIntegral a
  Float a -> pure . Float . f $ a
  a       -> err [errEval, errNotAllowed, "calu"]

-- | Binary arithmetic operator builder
calb
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a -> a)
  -> Sexp
  -> Sexp
  -> RE Sexp
calb op x y = case (x, y) of
  (Int   a, Int b  ) -> pure . Int . floor $ fromIntegral a `op` fromIntegral b
  (Int   a, Float b) -> pure . Float $ fromIntegral a `op` b
  (Float a, Int b  ) -> pure . Float $ a `op` fromIntegral b
  (Float a, Float b) -> pure . Float $ a `op` b
  _                  -> err [errEval, errNotAllowed, "calb"]

-- | Unary function builder
unary
  :: (ST Sexp -> RE (ST Sexp))
  -> (ST Sexp -> RE (ST Sexp))
  -> ST [Sexp]
  -> RE (ST Sexp)
unary g f = g'unary >=> eval >=> g >=> f

-- | Binary function builder
binary
  :: (ST Sexp -> RE (ST Sexp))
  -> (ST (Sexp, Sexp) -> RE (ST Sexp))
  -> ST [Sexp]
  -> RE (ST Sexp)
binary g f = g'binary >=> evalList >=> map' g >=> g'tuple >=> f

-- | N-fold function builder
nfold
  :: (ST Sexp -> RE (ST Sexp))
  -> (Sexp -> Sexp -> RE Sexp)
  -> String
  -> ST [Sexp]
  -> RE (ST Sexp)
nfold g f label = g'nary >=> evalList >=> map' g >=> fold' f label

-- | Fold arguments of a S-exp list using the given binary function
fold' :: (Sexp -> Sexp -> RE Sexp) -> String -> ST [Sexp] -> RE (ST Sexp)
fold' f o s = get s >>= \case
  [] -> case o of
    "+" -> put (Int 0) s
    "*" -> put (Int 1) s
    _   -> err [errEval, errNoArgs, o]
  [x] -> case o of
    "-" -> put x s >>= modify (f (Int 0))
    "/" -> put x s >>= modify (f (Int 1))
    _   -> put x s
  (x : xs) -> put xs s >>= modify (foldM f x)

-- | Build functions to control function's number of arguments
arity :: (Int -> Bool) -> ST [Sexp] -> RE (ST [Sexp])
arity p s@(_, e : args)
  | not (p nargs) = err [errEval, errWrongNargs, show' e ++ ",", show nargs]
  | otherwise     = put args s
  where nargs = length args
arity _ a = err [errEval, errNotAllowed, "arity"]

-- |
nth :: Int -> ST Sexp -> RE (ST Sexp)
nth i s = eval s >>= get >>= \case
  List l -> case drop i l of
    []    -> put NIL s
    x : _ -> put x s
  a -> err [errEval, errMalformed, show' a]


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

-- | Map the state result to an action.
-- This map is only valid when the result has multiple value, i.e., a list
map' :: (ST a -> RE (ST a)) -> ST [a] -> RE (ST [a])
map' f s@(env, es) = mapM f (sequence s) >>= sequence' >>= put' env
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
  }
  deriving Show

-- |
init'env :: ST Sexp
init'env = (Env mempty mempty, NIL)

-- |
set'env :: String -> ST Sexp -> RE (ST Sexp)
set'env k s@(Env {..}, _) | M.member k env'l = set'lenv k s
                          | otherwise        = set'genv k s

-- |
set'genv :: String -> ST Sexp -> RE (ST Sexp)
set'genv k s@(env@Env {..}, e) = put' (env { env'g = M.insert k e env'g }) s

-- |
set'lenv :: String -> ST Sexp -> RE (ST Sexp)
set'lenv k s@(env@Env {..}, e) = put' (env { env'l = M.insert k e env'l }) s

-- | The same as `set'env`, but set only when keys are not defined
set'env'undef :: String -> ST Sexp -> RE (ST Sexp)
set'env'undef k s@(Env {..}, _) = case M.lookup k env'g of
  Just _  -> pure s
  Nothing -> set'env k s

-- | Get S-exp value from the state by a symbol key
from'env :: String -> ST Sexp -> RE (ST Sexp)
from'env k s = from'lenv k s <|> from'genv k s

-- | Get S-exp value from the state global-env by a symbol key
from'genv :: String -> ST Sexp -> RE (ST Sexp)
from'genv k s@(Env {..}, _) = case M.lookup k env'g of
  Just v  -> put v s
  Nothing -> err [errEval, errVoidSymbolVar, k]

-- | Get S-exp value from the state local-env by a symbol key
from'lenv :: String -> ST Sexp -> RE (ST Sexp)
from'lenv k s@(Env {..}, _) = case M.lookup k env'l of
  Just v  -> put v s
  Nothing -> err [errEval, errVoidSymbolVar, k]

-- | When going into local-scope
local :: ST a -> RE (ST a)
local s@(env@Env {..}, _) =
  put' (env { env'g = env'l <> env'g, env'l = mempty }) s

-- | When getting out from local-scope
global :: ST b -> ST a -> RE (ST a)
global (g@Env{}, _) s@(l@Env{}, a) = put' (g { env'g = env'g l }) s

-- | Deactivate local env and use only global env
xlocal :: ST a -> RE (ST a)
xlocal s@(env@Env {..}, _) = put' (env { env'l = mempty }) s


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
  Seq       seq    -> show' (List seq)
  Vector    vector -> "#(" ++ unwords (show' <$> V.toList vector) ++ ")"
  HashTable map    -> show map
  Fn        fn     -> show' . head $ fn
  Macro     macro  -> show' . head $ macro
  List      list   -> case list of
    [] -> "nil"
    _  -> "(" ++ unwords (show' <$> list) ++ ")"
  Cons a b -> case (a, b) of
    (a, NIL     ) -> show' (List [a])
    (a, List [] ) -> show' (List [a])
    (a, Quote b ) -> "(" ++ unwords [show' a, "quote", show' b] ++ ")"
    (a, b@List{}) -> "(" ++ unwords [show' a, show' b] ++ ")"
    (a, Cons c d) -> "(" ++ unwords [show' a, dot c d] ++ ")"
    (a, b       ) -> "(" ++ dot a b ++ ")"
    where dot x y = unwords [show' x, ".", show' y]


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
  loop s mode@(reader, printer) = do
    input <- getInputLine "SLISP> "
    case input of
      Nothing   -> pure ()
      Just []   -> loop s normal
      Just ";;" -> loop s debug
      -- Just ";" -> loop env multiline
      Just str  -> case reader (fromString str) >>= flip put s >>= eval of
        Left  err       -> outputStrLn err >> loop s mode
        Right s'@(_, e) -> printer e >> loop s' mode


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
