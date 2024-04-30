-- |
-- Module      : Text.S.Language
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module defines builders that can define
-- the basic specifications of a language.
module Text.S.Language
  ( LanguageSpec (..)
  , haskellSpec
  , lispSpec
  , javaSpec
  )
where

import Text.S.Base
import Text.S.Combinator
import Text.S.Internal

data LanguageSpec s = LanguageSpec
  { charLiteral :: String
  -- ^ Characters assigned to indicate char literal region
  , stringLiteral :: String
  -- ^ Characters assigned to indicate string literal region
  , commentLine :: String
  -- ^ Characters assigned to start a single-line comment
  , commentBegin :: String
  -- ^ Characters assigned to start a block comment
  , commentEnd :: String
  -- ^ Characters assigned to end a block comment
  , idenBegin :: S s Char
  -- ^ Characters assigned to start the name of identifiers
  , idenLetter :: S s Char
  -- ^ Characters assigned identifier names except for its first letter
  , reservedWords :: [String]
  -- ^ List of reserved names
  , reservedOps :: [String]
  -- ^ List of reserved operators and special charecters
  , caseSensitive :: Bool
  -- ^ Flag if the language letter is case-sensitive
  }

-- | Language spec of Haskell 2010
haskellSpec :: Stream s => LanguageSpec s
haskellSpec =
  LanguageSpec
    { charLiteral = "'"
    , stringLiteral = "\""
    , commentBegin = "{-"
    , commentEnd = "-}"
    , commentLine = "--"
    , idenBegin = alpha <|> char '_'
    , idenLetter = choice [alphaNum, char '_', char '\'']
    , reservedOps =
        [ "#"
        , "'"
        , "*"
        , ","
        , "-"
        , "--"
        , "->"
        , "-}"
        , "::"
        , ";"
        , "<"
        , "<-"
        , "<<"
        , "="
        , "=>"
        , ">"
        , "?"
        , "@"
        , "[|"
        , "\""
        , "\\"
        , "_"
        , "`"
        , "{"
        , "{-"
        , "|"
        , "|]"
        , "}"
        , "~"
        , "!"
        ]
    , reservedWords =
        [ "as"
        , "case"
        , "class"
        , "data"
        , "default"
        , "deriving"
        , "do"
        , "else"
        , "forall"
        , "foreign"
        , "hiding"
        , "if"
        , "import"
        , "in"
        , "infix"
        , "infixl"
        , "infixr"
        , "instance"
        , "instance"
        , "let"
        , "module"
        , "newtype"
        , "of"
        , "proc"
        , "qualified"
        , "rec"
        , "then"
        , "type"
        , "where"
        ]
    , caseSensitive = True
    }

-- | Language spec of LISP
lispSpec :: Stream s => LanguageSpec s
lispSpec =
  LanguageSpec
    { charLiteral = "#\\"
    , stringLiteral = "\""
    , commentBegin = "#|"
    , commentEnd = "|#"
    , commentLine = ";"
    , idenBegin = noneOf " ()\",'`:;#|\\"
    , idenLetter = noneOf " ()\",'`:;|\\"
    , reservedOps = ["?", "!", "[", "]", "{", "}"]
    , reservedWords = [".", "(", ")", "\"", ",", "'", "`", ":", ";", "#", "|", "\\"]
    , caseSensitive = True
    }

-- | Language spec of Java
javaSpec :: Stream s => LanguageSpec s
javaSpec =
  LanguageSpec
    { charLiteral = "'"
    , stringLiteral = "\""
    , commentBegin = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , idenBegin = alpha <|> oneOf "_$"
    , idenLetter = alphaNum <|> oneOf "_$"
    , reservedOps =
        [ "!"
        , "$"
        , "("
        , ")"
        , "*"
        , "+"
        , "-"
        , "."
        , "<"
        , "="
        , ">"
        , "?"
        , "["
        , "\""
        , "\\"
        , "]"
        , "^"
        , "{"
        , "|"
        , "}"
        , "'"
        ]
    , reservedWords =
        [ "abstract"
        , "assert"
        , "boolean"
        , "break"
        , "byte"
        , "case"
        , "catch"
        , "char"
        , "class"
        , "const"
        , "continue"
        , "default"
        , "do"
        , "double"
        , "else"
        , "enum"
        , "extends"
        , "false"
        , "final"
        , "finally"
        , "float"
        , "for"
        , "if"
        , "implements"
        , "import"
        , "instanceof"
        , "int"
        , "interface"
        , "long"
        , "native"
        , "new"
        , "null"
        , "package"
        , "private"
        , "protected"
        , "public"
        , "return"
        , "short"
        , "static"
        , "strictfp"
        , "super"
        , "switch"
        , "synchronized"
        , "this"
        , "throw"
        , "throws"
        , "transient"
        , "true"
        , "try"
        , "void"
        , "volatile"
        , "while"
        ]
    , caseSensitive = True
    }
