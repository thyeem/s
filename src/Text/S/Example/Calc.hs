module Text.S.Example.Calc where


import           Control.DeepSeq                ( NFData )
import           Control.Monad                  ( unless )
import           System.IO
import           Text.S



-- |
calc'infixl :: (Stream s, NFData s) => ParserS s Double
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

-- |
calc'infixr :: (Stream s, NFData s) => ParserS s Double
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

-- |
calc'prefix :: (Stream s, NFData s) => ParserS s Double
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

-- |
calc'postfix :: (Stream s, NFData s) => ParserS s Double
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


-- |
repl :: IO ()
repl = do
  input <- read'
  unless (input == ":q" || input == ":quit") $ eval' input >> repl
 where
  read' = putStrLn mempty >> putStr "calc> " >> hFlush stdout >> getLine
  eval' input = case t calc'infixl input of
    Ok ok _ -> pp ok
    Error s -> pp s
