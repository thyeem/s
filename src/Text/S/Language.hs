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


-- |
data LanguageDef = LanguageDef
  {
    -- | Flag if the language letter is case-sensitive
    defCaseSensitive     :: Bool
  ,

    -- | Letters assigned that a block comment starts with
    defCommentBlockBegin :: [String]
  ,

    -- | Letters assigned that a block comment ends
    defCommentBlockEnd   :: [String]
  ,

    -- | Letters assigned that a single line comment starts with
    defCommentLine       :: [String]
  ,

    -- | Letters assigned that an identifier starts with
    defIdentifierBegin   :: [String]
  ,

    -- | Letters assigned for a name of an identifier except for its first letter
    defIdentifierName    :: [String]
  ,

    -- | List of reserved names
    defReservedNames     :: [String]
  ,

    -- | List of reserved operators and special charecters
    defReservedSpecials  :: [String]
  }


-- | Default Language definition
defaultDef :: LanguageDef
defaultDef = LanguageDef { defCaseSensitive     = True
                         , defCommentBlockBegin = ["/*"]
                         , defCommentBlockEnd   = ["*/"]
                         , defCommentLine       = ["//"]
                         , defIdentifierBegin   = ["_", "alpha"]
                         , defIdentifierName    = ["alphaNum"]
                         , defReservedSpecials  = []
                         , defReservedNames     = []
                         }


-- | Language definition of Haskell 2010
haskellDef :: LanguageDef
haskellDef = LanguageDef
  { defCaseSensitive     = True
  , defCommentBlockBegin = ["{-"]
  , defCommentBlockEnd   = ["-}"]
  , defCommentLine       = ["--"]
  , defIdentifierBegin   = ["alpha"]
  , defIdentifierName    = ["alphaNum", "'", "_"]
  , defReservedSpecials  = [ "#"
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
  , defReservedNames     = [ "as"
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


-- | Language definition of Java
javaDef :: LanguageDef
javaDef = LanguageDef
  { defCaseSensitive     = True
  , defCommentBlockBegin = ["/*"]
  , defCommentBlockEnd   = ["*/"]
  , defCommentLine       = ["//"]
  , defIdentifierBegin   = ["alpha"]
  , defIdentifierName    = ["alphaNum", "_"]
  , defReservedSpecials  = [ "!"
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
  , defReservedNames     = [ "abstract"
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
