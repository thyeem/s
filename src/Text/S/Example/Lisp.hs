{-# Language DeriveAnyClass #-}
{-# Language LambdaCase #-}
{-# Language NamedFieldPuns #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language StandaloneDeriving #-}
{-# Language TupleSections #-}

module Text.S.Example.Lisp where

import           Control.Monad                  ( foldM )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Dynamic
import           Data.Fixed                     ( mod' )
import           Data.List                      ( foldl1' )
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
-- Env
----------

type Env = M.Map String Sexp

(>?) :: Ord k => M.Map k a -> k -> Bool
(>?) = flip M.member

(??) :: Ord k => M.Map k a -> k -> Maybe a
(??) = flip M.lookup


----------
-- Read
----------
-- |
read' :: Text -> RE Sexp
read' s = case parse' sexp s of
  Ok ok (State stream _ _) | isEmpty stream -> pure ok
                           | otherwise      -> err [errRepl, errManySexp]
  Error _ -> err [errRead, errParsing]

sexp :: Parser Sexp
sexp =
  between jump jump (choice [nil, str, bool, real, int, quote, key, sym, form])

jump :: Parser ()
jump = skips lispdef

end :: Parser ()
end = gap <|> void (try (symbol ")"))

nil :: Parser Sexp
nil = NIL <$ symbol "nil" <* end

bool :: Parser Sexp
bool = Boolean <$> (symbol "t" <* end $> True)

int :: Parser Sexp
int = Int <$> integer <* end

real :: Parser Sexp
real = Real <$> float <* end

sym :: Parser Sexp
sym = Symbol <$> identifier lispdef

key :: Parser Sexp
key = Keyword . (":" ++) <$> (symbol ":" *> identifier lispdef)

str :: Parser Sexp
str = StringLit <$> stringLit

quote :: Parser Sexp
quote = symbol "'" *> (Quote <$> sexp)

form :: Parser Sexp
form = List <$> between (symbol "(") (symbol ")") (many sexp)

-- vector :: Parser Sexp
-- vector =
  -- List <$> between (symbol "[") (symbol "]") (endBy (many space) sexp)


----------
-- Eval
----------
-- |
eval :: Env -> Sexp -> RE (Env, Sexp)
eval env e = case e of
  Quote v -> pure (env, v)
  List [] -> pure (env, NIL)
  List (v@(Symbol "quote") : args) -> apply env v args
  List (v@(Symbol _) : args) -> evalList args >>= apply env v
  List (v : _) -> err [errEval, errInvalidFn, show' v]
  Symbol sym -> lookupSymbol env sym
  v -> pure (env, v)
 where
  evalList xs = mapM ((snd <$>) . eval env) xs
  lookupSymbol e x = case e ?? x of
    Just v  -> pure (e, v)
    Nothing -> err [errEval, errVoidSymbolVar, x]

-- |
apply :: Env -> Sexp -> [Sexp] -> RE (Env, Sexp)
apply env e args = case e of
  Symbol "list"  -> f'list env args
  Symbol "quote" -> unary f'quote env e args
  Symbol "+"     -> (env, ) <$> fold (f'calc (+) env) e args
  Symbol "-"     -> (env, ) <$> fold (f'calc (-) env) e args
  Symbol "*"     -> (env, ) <$> fold (f'calc (*) env) e args
  Symbol "/"     -> (env, ) <$> fold (f'calc (/) env) e args
  Symbol "%"     -> (env, ) <$> binary (f'calc mod') env e args
  Symbol "mod"   -> (env, ) <$> binary (f'calc mod') env e args
  Symbol "expt"  -> (env, ) <$> binary (f'calc (**)) env e args
  Symbol "1+"    -> unary f'quote env e args
  Symbol sym     -> err [errEval, errVoidSymbolFn, sym]
  _              -> err [errEval, errNotAllowed]

-- | list
f'list :: Env -> [Sexp] -> RE (Env, Sexp)
f'list env args = pure (env, List args)

-- | quote
f'quote :: Env -> Sexp -> RE (Env, Sexp)
f'quote env e = pure (env, e)

-- |
f'calc
  :: (forall a . (Num a, RealFrac a, Floating a) => a -> a -> a)
  -> Env
  -> Sexp
  -> Sexp
  -> RE Sexp
f'calc op _ a b = case (a, b) of
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

-- arity :: (b -> a) -> (c -> Bool) -> Env -> Sexp -> [Sexp] -> a
-- arity f p

-- | applies list args to the given unary function
unary :: (Env -> Sexp -> RE a) -> Env -> Sexp -> [Sexp] -> RE a
unary f env e args
  | nargs /= 1 = err [errEval, errWrongNargs, show' e ++ ",", show nargs]
  | otherwise  = f env (head args)
  where nargs = length args

-- | applies list args to the given binary function
binary :: (Env -> Sexp -> Sexp -> RE a) -> Env -> Sexp -> [Sexp] -> RE a
binary f env e args
  | nargs /= 2 = err [errEval, errWrongNargs, show' e ++ ",", show nargs]
  | otherwise  = f env (head args) (head . tail $ args)
  where nargs = length args

-- | applies list args to the given even-ary function
evenary :: (Env -> [Sexp] -> RE a) -> Env -> Sexp -> [Sexp] -> RE a
evenary f env e args
  | even nargs = err [errEval, errWrongNargs, show' e ++ ",", show nargs]
  | otherwise  = f env args
  where nargs = length args




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
  StringLit string  -> string
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
sl = runInputT (defaultSettings { historyFile }) (loop M.empty normal)
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
