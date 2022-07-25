-----------------------------------------------------------------------------
-- |
-- Module      : Text.S.Lexer
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module constructs a lexer or a tokenizer parser to parse
-- lexical units (lexemes) based on parser combinators.
--
-----------------------------------------------------------------------------

module Text.S.Lexer
  ( module Text.S.Lexer
  ) where

import           Control.DeepSeq                ( NFData
                                                , force
                                                )
import           Data.Char                      ( digitToInt
                                                , isAlpha
                                                , toLower
                                                , toUpper
                                                )
import           Text.S.Base
import           Text.S.Combinator
import           Text.S.Internal
import           Text.S.Language



-- | token parsers: lexer
type Lexer s a = LanguageDef -> Parser'S s a


-------------------------
-- lang-def-dependent
-------------------------
-- | generate a lexical parser for the given parser @p@
token :: (Stream s, NFData s) => Parser'S s a -> (LanguageDef -> Parser'S s a)
token p def = p <* jump def

-- |
reserved
  :: (Stream s, NFData s) => String -> (LanguageDef -> Parser'S s String)
reserved name = token (string name)

-- | skipping unnecesary part including whitespaces and comments
-- >>> :{
--   unwrap' $ t
--     (jump defaultDef)
--     "  // line-comment starts\n \r\n  /*inner block*/  end-of-jump"
-- :}
-- "end-of-jump"
--
jump :: (Stream s, NFData s) => LanguageDef -> Parser'S s ()
jump def =
  skipMany $ skipSome space <|> skipSome (commentLine def) <|> skipSome
    (commentBlock def)


-- |
commentLine :: (Stream s, NFData s) => LanguageDef -> Parser'S s String
commentLine def = p *> manyTill anychar eol
  where p = choice $ string <$> defCommentLine def

-- |
commentBlock :: (Stream s, NFData s) => LanguageDef -> Parser'S s String
commentBlock def = bra *> manyTill anychar ket
 where
  bra = choice $ string <$> defCommentBlockBegin def
  ket = choice $ string <$> defCommentBlockEnd def


-- |
caseGuard :: (Stream s, NFData s) => Bool -> String -> Parser'S s String
caseGuard sensitive s | sensitive = string s
                      | otherwise = mapM char' s
 where
  char' c | isAlpha c = char (toLower c) <|> char (toUpper c)
          | otherwise = char c

-- |
identifier :: (Stream s, NFData s) => LanguageDef -> Parser'S s String
identifier def = (<>) <$> begin <*> nameLeft
 where
  begin    = choice $ selectParser <$> defIdentifierBegin def
  nameLeft = choice $ selectParser <$> defIdentifierName def

-- |
selectParser :: (Stream s, NFData s) => String -> Parser'S s String
selectParser x = case x of
  "alpha"    -> some alpha
  "alphaNum" -> some alphaNum
  "letter"   -> some letter
  "digit"    -> some digit
  "lower"    -> some lower
  "upper"    -> some upper
  c          -> string c




-------------------------
-- lang-def-independent
-------------------------

-- |
-- hexadecimal :: Stream s => Parser'S s Integer

number
  :: (Stream s, NFData s) => Integer -> Parser'S s String -> Parser'S s Integer
number base parser = do
  p <- parser
  let f x d = base * x + toInteger (digitToInt d)
  let num = force $ foldl f 0 p
  return num
