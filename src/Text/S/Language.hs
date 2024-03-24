-- |
-- Module      : Text.S.Language
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
module Text.S.Language
  ( module Text.S.Language
  )
where

import Text.S.Base
import Text.S.Combinator
import Text.S.Internal

data LanguageDef s = LanguageDef
  { defCaseSensitive :: Bool
  -- ^ Flag if the language letter is case-sensitive
  , defCharLiteralMark :: S s String
  -- ^ Characters assigned to indicate char literal region
  , defStringLiteralMark :: S s String
  -- ^ Characters assigned to indicate string literal region
  , defCommentBlockBegin :: S s String
  -- ^ Characters assigned to start a block comment
  , defCommentBlockEnd :: S s String
  -- ^ Characters assigned to end a block comment
  , defCommentLine :: S s String
  -- ^ Characters assigned to start a single-line comment
  , defIdentifierBegin :: S s Char
  -- ^ Characters assigned to start the name of identifiers
  , defIdentifierName :: S s String
  -- ^ Characters assigned identifier names except for its first letter
  , defKeywords :: [String]
  -- ^ List of reserved names
  , defReservedOps :: [String]
  -- ^ List of reserved operators and special charecters
  }

-- | Default Language definition
def :: Stream s => LanguageDef s
def =
  LanguageDef
    { defCaseSensitive = True
    , defCharLiteralMark = string "'"
    , defStringLiteralMark = string "\""
    , defCommentBlockBegin = string "/*"
    , defCommentBlockEnd = string "*/"
    , defCommentLine = string "//"
    , defIdentifierBegin = choice [char '_', alpha]
    , defIdentifierName = many alphaNum
    , defReservedOps = []
    , defKeywords = []
    }
{-# INLINE def #-}

-- | Language definition of Haskell 2010
haskelldef :: Stream s => LanguageDef s
haskelldef =
  LanguageDef
    { defCaseSensitive = True
    , defCharLiteralMark = string "'"
    , defStringLiteralMark = string "\""
    , defCommentBlockBegin = string "{-"
    , defCommentBlockEnd = string "-}"
    , defCommentLine = string "--"
    , defIdentifierBegin = alpha
    , defIdentifierName = many $ choice [alphaNum, char '\'', char '_']
    , defReservedOps =
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
    , defKeywords =
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
    }

-- | Language definition of SLISP
lispdef :: Stream s => LanguageDef s
lispdef =
  LanguageDef
    { defCaseSensitive = True
    , defCharLiteralMark = string "#\\"
    , defStringLiteralMark = string "\""
    , defCommentBlockBegin = string "#|"
    , defCommentBlockEnd = string "|#"
    , defCommentLine = string ";"
    , defIdentifierBegin = noneOf " ()\",'`:;#|\\"
    , defIdentifierName = many $ noneOf " ()\",'`:;|\\"
    , defReservedOps = ["?", "!", "[", "]", "{", "}"]
    , defKeywords = [".", "(", ")", "\"", ",", "'", "`", ":", ";", "#", "|", "\\"]
    }

-- | Language definition of Java
javaDef :: Stream s => LanguageDef s
javaDef =
  LanguageDef
    { defCaseSensitive = True
    , defCharLiteralMark = string "'"
    , defStringLiteralMark = string "\""
    , defCommentBlockBegin = string "/*"
    , defCommentBlockEnd = string "*/"
    , defCommentLine = string "//"
    , defIdentifierBegin = alpha
    , defIdentifierName = many $ alphaNum <|> char '_'
    , defReservedOps =
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
    , defKeywords =
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
        , "final"
        , "finally"
        , "float"
        , "for"
        , "goto"
        , "if"
        , "implements"
        , "import"
        , "instanceof"
        , "int"
        , "interface"
        , "long"
        , "native"
        , "new"
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
        , "try"
        , "void"
        , "volatile"
        , "while"
        ]
    }
