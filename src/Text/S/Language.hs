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

    -- | Characters assigned to indicate char literal region
    defCharLiteralMark   :: String
  ,

    -- | Characters assigned to indicate string literal region
    defStringLiteralMark :: String
  ,

    -- | Characters assigned to start a block comment
    defCommentBlockBegin :: [String]
  ,

    -- | Characters assigned to end a block comment
    defCommentBlockEnd   :: [String]
  ,

    -- | Characters assigned to start a single-line comment
    defCommentLine       :: [String]
  ,

    -- | Characters assigned to start the name of identifiers
    defIdentifierBegin   :: [String]
  ,

    -- | Characters assigned identifier names except for its first letter
    defIdentifierName    :: [String]
  ,

    -- | List of reserved names
    defReservedNames     :: [String]
  ,

    -- | List of reserved operators and special charecters
    defReservedSpecials  :: [String]
  }


-- | Default Language definition
defDef :: LanguageDef
defDef = LanguageDef { defCaseSensitive     = True
                     , defCharLiteralMark   = "'"
                     , defStringLiteralMark = "\""
                     , defCommentBlockBegin = ["'''"]
                     , defCommentBlockEnd   = ["'''"]
                     , defCommentLine       = ["#"]
                     , defIdentifierBegin   = ["_", "alpha"]
                     , defIdentifierName    = ["alphaNum"]
                     , defReservedSpecials  = []
                     , defReservedNames     = ["sofia"]
                     }


-- | Language definition of Haskell 2010
haskellDef :: LanguageDef
haskellDef = LanguageDef
  { defCaseSensitive     = True
  , defCharLiteralMark   = "'"
  , defStringLiteralMark = "\""
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
  , defCharLiteralMark   = "'"
  , defStringLiteralMark = "\""
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
