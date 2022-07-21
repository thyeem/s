module Text.S.Lexer
  ( module Text.S.Lexer
  ) where

import           Data.Char                      ( isAlpha
                                                , toLower
                                                , toUpper
                                                )
import           Text.S.Base
import           Text.S.Combinator
import           Text.S.Internal


-------------------------
-- token parsers: lexer
-------------------------
-- | generate a lexical parser for the given parser @p@
token
  :: Stream s
  => Parser'S s String  -- parser for line comment mark
  -> Parser'S s String  -- parser for block comment opener
  -> Parser'S s String  -- parser for block comment closure
  -> Parser'S s a       -- parser for target lexeme
  -> Parser'S s a
token s bra ket p = p <* jump (lineComment s) (blockComment bra ket)

-- |
-- symbol :: Stream s => String -> Parser'S s String
-- symbol name = token (string name)


-- | skipping unnecesary part including whitespaces and comments
-- >>> :{
--   unwrap' $ t
--     (jump (lineComment $ string "#")
--           (blockComment (string "/*")
--           (string "*/")))
--     "  ## line-comment starts\n \r\n  /*inner block*/  end-of-jump"
-- :}
-- "end-of-jump"
--
jump :: Stream s => Parser'S s a -> Parser'S s a -> Parser'S s ()
jump lc bc = skipMany $ skipSome space <|> skipSome lc <|> skipSome bc

lineComment :: Stream s => Parser'S s String -> Parser'S s String
lineComment p = p *> manyTill anychar eol

blockComment
  :: Stream s => Parser'S s String -> Parser'S s String -> Parser'S s String
blockComment bra ket = bra *> manyTill anychar ket

caseGuard :: Stream s => Bool -> String -> Parser'S s String
caseGuard sensitive s | sensitive = string s
                      | otherwise = mapM char' s
 where
  char' c | isAlpha c = char (toLower c) <|> char (toUpper c)
          | otherwise = char c

identifier
  :: Stream s => Parser'S s Char -> Parser'S s String -> Parser'S s String
identifier start body = (:) <$> start <*> body
