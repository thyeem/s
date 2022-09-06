module Text.S.Example.Calc where

import           System.IO
import           Text.S


-- | Expr parser for left-associative infix operators
calc'infixl :: Stream s => ParserS s Double
calc'infixl = expr atom table
 where
  atom = strip float <|> parens calc'infixl
  table =
    [ [prefixU "-" negate, prefixU "+" id]
    , [postfixU "++" (+ 1), postfixU "--" (subtract 1)]
    , [infixL "^" (**)]
    , [infixL "*" (*), infixL "/" (/)]
    , [infixL "+" (+), infixL "-" (-)]
    ]

-- | Expr parser for right-associative infix operators
calc'infixr :: Stream s => ParserS s Double
calc'infixr = expr atom table
 where
  atom = strip float <|> parens calc'infixr
  table =
    [ [prefixU "-" negate, prefixU "+" id]
    , [postfixU "++" (+ 1), postfixU "--" (subtract 1)]
    , [infixR "^" (**)]
    , [infixR "*" (*), infixR "/" (/)]
    , [infixR "+" (+), infixR "-" (-)]
    ]

-- | Expr parser for prefix operators
calc'prefix :: Stream s => ParserS s Double
calc'prefix = expr atom table
 where
  atom = strip float
  table =
    [ [ prefixB "^" (**)
      , prefixB "*" (*)
      , prefixB "/" (/)
      , prefixB "+" (+)
      , prefixB "-" (-)
      ]
    ]

-- | Expr parser for postfix operators
calc'postfix :: Stream s => ParserS s Double
calc'postfix = expr atom table
 where
  atom = strip float
  table =
    [ [ postfixB "^" (**)
      , postfixB "*" (*)
      , postfixB "/" (/)
      , postfixB "+" (+)
      , postfixB "-" (-)
      ]
    ]

-- | Expr calculator selector
calc :: IO ()
calc = do
  parser <- read'
  case parser of
    "infixl"  -> repl calc'infixl
    "infixr"  -> repl calc'infixr
    "prefix"  -> repl calc'prefix
    "postfix" -> repl calc'postfix
    "q"       -> pure ()
    _         -> calc
 where
  read' =
    putStrLn mempty
      >> putStr "Choose an Expr Calculators [infixl, infixr, prefix, postfix]: "
      >> hFlush stdout
      >> getLine

-- | read-eval-print-loop
repl :: ParserS String Double -> IO ()
repl parser = do
  input <- read'
  if input == "q" then calc else eval' input >> repl parser
 where
  read' = putStrLn mempty >> putStr "calc> " >> hFlush stdout >> getLine
  eval' input = case t parser input of
    Ok ok s | null . stateStream $ s -> pp ok
            | otherwise              -> pp s
    Error s -> pp s
