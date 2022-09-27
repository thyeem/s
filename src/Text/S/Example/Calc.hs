module Text.S.Example.Calc where

import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.Text.Lazy                as TL
import           System.Console.Haskeline
import           Text.S


-- | Expr parser for left-associative infix operators
infixl' :: Stream s => ParserS s Double
infixl' = expr atom table
 where
  atom = strip number <|> parens infixl'
  table =
    [ [prefixU "-" negate, prefixU "+" id]
    , [postfixU "++" (+ 1), postfixU "--" (subtract 1)]
    , [infixL "^" (**)]
    , [infixL "*" (*), infixL "/" (/)]
    , [infixL "+" (+), infixL "-" (-)]
    ]

-- | Expr parser for right-associative infix operators
infixr' :: Stream s => ParserS s Double
infixr' = expr atom table
 where
  atom = strip number <|> parens infixr'
  table =
    [ [prefixU "-" negate, prefixU "+" id]
    , [postfixU "++" (+ 1), postfixU "--" (subtract 1)]
    , [infixR "^" (**)]
    , [infixR "*" (*), infixR "/" (/)]
    , [infixR "+" (+), infixR "-" (-)]
    ]

-- | Expr parser for prefix operators
prefix' :: Stream s => ParserS s Double
prefix' = expr atom table
 where
  atom = strip number
  table =
    [ [ prefixB "^" (**)
      , prefixB "*" (*)
      , prefixB "/" (/)
      , prefixB "+" (+)
      , prefixB "-" (-)
      ]
    ]

-- | Expr parser for postfix operators
postfix' :: Stream s => ParserS s Double
postfix' = expr atom table
 where
  atom = strip number
  table =
    [ [ postfixB "^" (**)
      , postfixB "*" (*)
      , postfixB "/" (/)
      , postfixB "+" (+)
      , postfixB "-" (-)
      ]
    ]

-- | print
print' :: (MonadIO m, Pretty a) => a -> InputT m ()
print' = outputStrLn . TL.unpack . pretty

-- | read-eval-print
rep :: MonadIO m => ParserS String Double -> String -> InputT m ()
rep parser input = case parse' parser input of
  Ok ok state@(State s _ _) | null . stateStream $ state -> print' ok
                            | otherwise                  -> err s
  Error (State s _ _) -> err s
  where err s = print' . unwords $ ["*** Error ***", s]

-- | Expr calculator
calc :: IO ()
calc = runInputT defaultSettings intro
 where
  intro = do
    input <- getInputLine
      "Choose an Expr Calculators [infixl, infixr, prefix, postfix]: "
    case input of
      Nothing        -> pure ()
      Just "q"       -> pure ()
      Just "infixl"  -> repl infixl'
      Just "infixr"  -> repl infixr'
      Just "prefix"  -> repl prefix'
      Just "postfix" -> repl postfix'
      Just _         -> intro

  repl parser = do
    input <- getInputLine "calc> "
    case input of
      Nothing    -> pure ()
      Just ""    -> repl parser
      Just "q"   -> intro
      Just input -> rep parser input *> repl parser
