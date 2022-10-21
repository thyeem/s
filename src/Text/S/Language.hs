-----------------------------------------------------------------------------
-- |
-- Module      : Text.S.Language
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
--
-----------------------------------------------------------------------------

module Text.S.Language
  ( module Text.S.Language
  ) where

import           Control.Applicative            ( (<|>) )
import           Text.S.Base
import           Text.S.Combinator
import           Text.S.Internal

-- |
data LanguageDef s = LanguageDef
  {
    -- | Flag if the language letter is case-sensitive
    defCaseSensitive     :: Bool
  ,

    -- | Characters assigned to indicate char literal region
    defCharLiteralMark   :: ParserS s String
  ,

    -- | Characters assigned to indicate string literal region
    defStringLiteralMark :: ParserS s String
  ,

    -- | Characters assigned to start a block comment
    defCommentBlockBegin :: ParserS s String
  ,

    -- | Characters assigned to end a block comment
    defCommentBlockEnd   :: ParserS s String
  ,

    -- | Characters assigned to start a single-line comment
    defCommentLine       :: ParserS s String
  ,

    -- | Characters assigned to start the name of identifiers
    defIdentifierBegin   :: ParserS s Char
  ,

    -- | Characters assigned identifier names except for its first letter
    defIdentifierName    :: ParserS s String
  ,

    -- | List of reserved names
    defKeywords          :: [String]
  ,

    -- | List of reserved operators and special charecters
    defReservedOps       :: [String]
  }


-- | Default Language definition
def :: Stream s => LanguageDef s
def = LanguageDef { defCaseSensitive     = True
                  , defCharLiteralMark   = string "'"
                  , defStringLiteralMark = string "\""
                  , defCommentBlockBegin = string "/*"
                  , defCommentBlockEnd   = string "*/"
                  , defCommentLine       = string "//"
                  , defIdentifierBegin   = choice [char '_', alpha]
                  , defIdentifierName    = many alphaNum
                  , defReservedOps       = []
                  , defKeywords          = []
                  }


-- | Language definition of Haskell 2010
haskelldef :: Stream s => LanguageDef s
haskelldef = LanguageDef
  { defCaseSensitive     = True
  , defCharLiteralMark   = string "'"
  , defStringLiteralMark = string "\""
  , defCommentBlockBegin = string "{-"
  , defCommentBlockEnd   = string "-}"
  , defCommentLine       = string "--"
  , defIdentifierBegin   = alpha
  , defIdentifierName    = many $ choice [alphaNum, char '\'', char '_']
  , defReservedOps       = [ "#"
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
  , defKeywords          = [ "as"
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
lispdef = LanguageDef { defCaseSensitive     = True
                      , defCharLiteralMark   = string "#\\"
                      , defStringLiteralMark = string "\""
                      , defCommentBlockBegin = string "#|"
                      , defCommentBlockEnd   = string "|#"
                      , defCommentLine       = string ";"
                      , defIdentifierBegin   = noneOf " ()\",'`:;#|\\"
                      , defIdentifierName    = many $ noneOf " ()\",'`:;|\\"
                      , defReservedOps       = ["?", "!", "[", "]", "{", "}"]
                      , defKeywords          = []
                      }

-- | Language definition of Java
javaDef :: Stream s => LanguageDef s
javaDef = LanguageDef
  { defCaseSensitive     = True
  , defCharLiteralMark   = string "'"
  , defStringLiteralMark = string "\""
  , defCommentBlockBegin = string "/*"
  , defCommentBlockEnd   = string "*/"
  , defCommentLine       = string "//"
  , defIdentifierBegin   = alpha
  , defIdentifierName    = many $ alphaNum <|> char '_'
  , defReservedOps       = [ "!"
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
  , defKeywords          = [ "abstract"
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
