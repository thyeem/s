-- |
-- Module      : Text.S.Lexeme
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- This module combines basic character parsers to construct parsers
-- for @__lexemes__@ or @__lexical tokens__@.
module Text.S.Lexeme
  ( nil
  , alphas
  , alphaNums
  , digits
  , digits_
  , hexDigits
  , specials
  , spaces
  , binary
  , octal
  , decimal
  , hexadecimal
  , zeros
  , natural
  , sign
  , sign'
  , integer
  , integer'
  , numeral
  , number
  , float
  , float'
  , floatA
  , floatB
  , floating
  , floating'
  , floatingA
  , floatingB
  , genFloating
  , scientific
  , scientific'
  , genScientific
  , strip
  , genCharLit
  , genStringLit
  , Lexer (..)
  , genLexer
  , updateLexer
  )
where

import Data.Char (digitToInt, readLitChar, toLower)
import Data.Functor (($>))
import Data.List (foldl', foldl1')
import qualified Data.Set as S
import Text.S.Base
import Text.S.Combinator
import Text.S.Internal
import Text.S.Language

-- | Parser for monoid's identity element
nil :: Stream s => S s [a]
nil = pure mempty

-- | Parses one or more alphabetical characters
--
-- >>> ta alphas "sofia & maria"
-- "sofia"
alphas :: Stream s => S s String
alphas = some alpha
{-# INLINE alphas #-}

-- | Parses one or more alphabetical or numeric characters
--
-- >>> ta alphaNums "maria2024 -> return 7yo;"
-- "maria2024"
alphaNums :: Stream s => S s String
alphaNums = some alphaNum
{-# INLINE alphaNums #-}

-- | Parses one or more digits
--
-- >>> ta digits "10fc63fb, base-16"
-- "10"
digits :: Stream s => S s String
digits = some digit
{-# INLINE digits #-}

-- | Parses one or more digits that may contain underscores
--
-- >>> ta digits_ "1_000_000"
-- "1000000"
digits_ :: Stream s => S s String
digits_ = some (skip (char '_') *> digit)
{-# INLINE digits_ #-}

-- | Parses one or more hexadecimal digits (Base16)
--
-- >>> ta hexDigits "10fc63fb, base-16"
-- "10fc63fb"
--
-- >>> ta hexDigits "0x10fc63fb, base-16"
-- "0x10fc63fb"
hexDigits :: Stream s => S s String
hexDigits =
  option mempty (string "0x")
    >>= \x -> (x ++) <$> some hexDigit
{-# INLINE hexDigits #-}

-- | Parses one or more special characters
specials :: Stream s => S s String
specials = some special
{-# INLINE specials #-}

-- | Parses one or more space
--
-- >>> ta spaces "  \n\tthe end of the pandemic"
-- "  \n\t"
spaces :: Stream s => S s String
spaces = some space
{-# INLINE spaces #-}

-- | Parses binary digits (base-2)
--
-- >>> ta binary "0b1010"
-- 10
binary :: (Stream s, Num a) => S s a
binary = char '0' *> oneOf "bB" *> numeral 2 (some $ oneOf "01")
{-# INLINE binary #-}

-- | Parses octal digits (base-8)
--
-- >>> ta octal "0o1234"
-- 668
octal :: (Stream s, Num a) => S s a
octal = char '0' *> oneOf "oO" *> numeral 8 (some octDigit)
{-# INLINE octal #-}

-- | Parses natural numbers or decimal digits (base-10)
-- This includes numbers with leading zeros
--
-- >>> ta decimal "00123456789"
-- 123456789
decimal :: (Stream s, Num a) => S s a
decimal = numeral 10 digits
{-# INLINE decimal #-}

-- | Parses a hexadecimal digits (base-16)
--
-- >>> ta hexadecimal "0xEFEFEF"
-- 15724527
hexadecimal :: (Stream s, Num a) => S s a
hexadecimal = char '0' *> oneOf "xX" *> numeral 16 (some hexDigit)
{-# INLINE hexadecimal #-}

-- | Parses numbers with leading-zeros
--
-- >>> ta zeros "000002022"
-- 2022
zeros :: (Stream s, Num a) => S s a
zeros = char '0' *> decimal
{-# INLINE zeros #-}

-- | Parses natural numbers (non-leading zeros and signs)
--
-- >>> ta natural "27182818284"
-- 27182818284
natural :: Stream s => S s Integer
natural = try digit *> try (anycharBut '0') *> decimal
{-# INLINE natural #-}

-- | Parses a sign (@+@ or @-@) as a numeric function
--
-- >>> ta (sign <*> floating)  "-273.15 in Celsius"
-- -273.15
sign :: (Stream s, Num a) => S s (a -> a)
sign = (char '-' $> negate) <|> (char '+' $> id) <|> pure id
{-# INLINE sign #-}

-- | Parses a sign (@+@ or @-@) as a string
-- >>> ta sign' "-273.15 in Celsius"
-- "-"
sign' :: Stream s => S s String
sign' = option mempty (string "-" <|> (string "+" $> mempty))

-- | Parses a permissive-integer ((opt)sign + 'permissive' numbers)
--
-- >>> ta integer "1_000_000"
-- 1000000
integer :: Stream s => S s Integer
integer = sign <*> numeral 10 digits_
{-# INLINE integer #-}

-- | Parses a strict-integer ((opt)sign + 'strict' numbers)
--
-- >>> ta (sign <*> integer')  "-273.15 in Celsius"
-- -273
integer' :: Stream s => S s Integer
integer' = sign <*> decimal
{-# INLINE integer' #-}

-- | Convert a string parser into a positional numeral parser with base
numeral :: (Stream s, Num a) => a -> S s String -> S s a
numeral base parser = foldl' f 0 <$> parser
 where
  f x d = base * x + fromIntegral (digitToInt d)
{-# INLINE numeral #-}

-- | Parses general form of number (float + integer)
--
-- >>> ta number "12.12"
-- 12.12
--
-- >>> ta number ".141592"
-- 0.141592
--
-- >>> ta number "2718.2818284e-3"
-- 2.7182818284
number :: Stream s => S s Double
number = float <|> (fromIntegral <$> integer)
{-# INLINE number #-}

-- | Parses floating numbers including scientific notations.
-- In floating part, either whole or fractional number can be omitted.
--
-- [permissive 'floating'] or [permissive 'scientific']
--
-- See also 'floating' and 'scientific''.
--
-- >>> ta float "3."
-- 3.0
--
-- >>> ta float "1_048_576.1024"
-- 1048576.1024
--
-- >>> ta float "3.141592e-5"
-- 3.141592e-5
float :: Stream s => S s Double
float = scientific <|> floating
{-# INLINE float #-}

-- | Parses floating numbers including scientific notations.
-- Every part in the coefficient is strictly included.
--
-- [strict 'floating'] or [strict 'scientific']
--
-- See also `floating'` and `scientific'`.
float' :: Stream s => S s Double
float' = scientific' <|> floating'
{-# INLINE float' #-}

-- | Parses floating numbers including scientific notations.
-- The coefficients in scientific format allow the omitted forms.
-- The whole-number-part in floating form is optional.
--
-- [permissive whole-number 'floating'] or [permissive 'scientific']
--
-- See also 'floatingA' and `scientific'`.
--
-- >>> ta floatA "3.e-5"
-- 3.0e-5
--
-- >>> ta floatA "3."
-- 3.0
floatA :: Stream s => S s Double
floatA = scientific <|> floatingA
{-# INLINE floatA #-}

-- | Parses floating numbers including scientific notations.
-- The coefficients in scientific format allow the omitted forms.
-- The deciaml-point-part in floating form is optional.
--
-- [permissive fractional 'floating'] or [permissive 'scientific']
--
-- See also 'floatingB' and `scientific'`.
--
-- >>> ta floatB "141.592e-5"
-- 1.41592e-3
--
-- >>> ta floatB ".141592"
-- 0.141592
floatB :: Stream s => S s Double
floatB = scientific <|> floatingB
{-# INLINE floatB #-}

-- | Parses floating number in most-flexible format of
-- [(opt)whole-number] + [decimal-point(.)] + [(opt)decimal-part]
--
-- This is the most permissive form and either part can be omitted.
--
-- >>> ta floating "-3.141592"
-- -3.141592
--
-- >>> ta floating ".141592"
-- 0.141592
--
-- >>> ta floating "-3."
-- -3.0
floating :: Stream s => S s Double
floating = floatingA <|> floatingB
{-# INLINE floating #-}

-- | Parses floating number in strict format of
-- [whole-number] + [decimal-point(.)] + [decimal-part]
--
-- This is the most strict form and every part must be included.
floating' :: Stream s => S s Double
floating' = read <$> genFloating digits digits
{-# INLINE floating' #-}

-- | Parses floating number in partially permissive format of
-- [whole-number] + [decimal-point(.)] + [(opt) decimal-part]
--
-- This is the floating form with optional decimal part.
--
-- >>> ta floatingA "3."
-- 3.0
floatingA :: Stream s => S s Double
floatingA = read <$> genFloating digits_ (option "0" digits)
{-# INLINE floatingA #-}

-- | Parses floating number in partially permissive format of
-- [(opt) whole-number] + [decimal-point(.)] + [decimal-part]
--
-- This is the floating form with optional whole number part.
--
-- >>> ta floatingB ".141592"
-- 0.141592
floatingB :: Stream s => S s Double
floatingB = read <$> genFloating (option "0" digits_) digits
{-# INLINE floatingB #-}

-- | Parser builder for several types of floating numbers
genFloating
  :: Stream s => S s String -> S s String -> S s String
genFloating wholeNumber decimalPart =
  foldl1'
    (liftA2 (++))
    [sign', wholeNumber, string ".", decimalPart]
{-# INLINEABLE genFloating #-}

-- | Parses floating number in scientific notations of
-- [floating or int coefficient] + [e or E] + [exponent]
--
-- This allow the omitted forms in the coefficient with
-- optional whole number parts or optional decimal parts.
--
-- >>> ta scientific "2_000E-3"
-- 2.0
--
-- >>> ta scientific ".27182818284E+1"
-- 2.7182818284
scientific :: Stream s => S s Double
scientific = read <$> genScientific (floating <|> fromIntegral <$> integer)
{-# INLINEABLE scientific #-}

-- | Parses floating number in 'strict' form of scientific notations like
--
-- @strict coefficient@ + @e@ or @E@ + @exponent@
--
-- Every part in the coefficient must be strict.
--
-- See 'floating'.
--
-- >>> ta scientific' "271828.18284E-5"
-- 2.7182818284
scientific' :: Stream s => S s Double
scientific' = read <$> genScientific (floating' <|> fromIntegral <$> integer')
{-# INLINEABLE scientific' #-}

-- | Parser builder for several types of numbers in scientific format
genScientific :: Stream s => S s Double -> S s String
genScientific coeff = liftA2 (++) (show <$> coeff) exponent
 where
  exponent = liftA2 (:) (oneOf "eE") (liftA2 (++) sign' digits)
{-# INLINEABLE genScientific #-}

-- | Remove any leading and trailing whitespaces when parsing with @p@
--
-- Peeling whitespaces off is independent of any language syntax.
-- Use this when you just want to strip whitespaces around targets
--
-- >>> ta (strip float) "  3.1415926535\n  "
-- 3.1415926535
strip :: Stream s => S s a -> S s a
strip = between (skipMany space) (skipMany space)
{-# INLINE strip #-}

-- | Character literal parser builder
genCharLit :: Stream s => S s String -> S s Char
genCharLit mark = between mark (mark <?> "end-of-char-literal") readChar
 where
  readChar = do
    s <- try $ count 4 anychar <|> manyTill eof anychar
    case readLitChar s of
      [(a, s')] -> a <$ skipCount (length s - length s') anychar
      _ -> fail "failed to read any char literal"
{-# INLINE genCharLit #-}

-- | String literal parser builder
genStringLit :: Stream s => S s String -> S s String
genStringLit mark = between mark mark (concat <$> many character)
 where
  character = nonEscaped <|> escaped
  -- TODO: improve performance (sequence-level rather than parser-level)
  nonEscaped = some (forbid mark *> forbid (char '\\') *> anychar)
  escaped = char '\\' *> (simple <|> unicode)
  unicode = do
    u <- oneOf "uU"
    s <- if u == 'u' then count 4 hexDigit else count 8 hexDigit
    pure $ concat ["\\", [u], s]
  simple =
    oneOf "\\\"'0nrtbvf" >>= \x -> pure $ case x of
      '"' -> "\""
      '\\' -> "\\"
      '\'' -> "'"
      '0' -> "\0"
      'n' -> "\n"
      'r' -> "\r"
      't' -> "\t"
      'b' -> "\b"
      'v' -> "\v"
      'f' -> "\f"
      _ -> die "unreachable"
{-# INLINEABLE genStringLit #-}

data Lexer s a = Lexer
  { tidy' :: S s ()
  -- ^ Skip whitespaces and comments
  , gap' :: S s ()
  -- ^ Ensure one or more spaces or comment
  , token' :: S s a -> S s a
  -- ^ Wrapper for lexeme parsers, single semantic units
  , symbol' :: String -> S s String
  -- ^ Parses any string symbol to comsume. The same as 'string'
  , parens' :: S s a -> S s a
  -- ^ Parses string between parentheses
  , braces' :: S s a -> S s a
  -- ^ Parses string between curly braces
  , angles' :: S s a -> S s a
  -- ^ Parses string between angle brackets
  , squares' :: S s a -> S s a
  -- ^ Parses string between square brackets
  , cmtL' :: S s String
  -- ^ Parses a single line comment
  , cmtB' :: S s String
  -- ^ Parses a multi-line block comment
  , identifier' :: S s String
  -- ^ Parses an identifier
  , operator' :: S s String
  -- ^ Parses operators or special-chars based on the given 'LanguageSpec'
  , charLit' :: S s Char
  -- ^ Parses a single @char literal@
  , stringLit' :: S s String
  -- ^ Parses a single @string literal@
  }

genLexer :: Stream s => LanguageSpec s -> Lexer s a
genLexer def =
  let tidy = skipMany $ choice [spaces, cmtL, cmtB]
      gap = skipSome $ choice [spaces, cmtL, cmtB]
      token = (<* tidy)
      symbol = token . string
      parens = between (symbol "(") (symbol ")")
      braces = between (symbol "{") (symbol "}")
      angles = between (symbol "<") (symbol ">")
      squares = between (symbol "[") (symbol "]")
   in Lexer
        { tidy' = tidy
        , gap' = gap
        , token' = token
        , symbol' = symbol
        , parens' = parens
        , braces' = braces
        , angles' = angles
        , squares' = squares
        , cmtL' = cmtL
        , cmtB' = cmtB
        , identifier' = identifier def
        , operator' = operator
        , charLit' = charLit
        , stringLit' = stringLit
        }
 where
  cmtL =
    if null line
      then fail mempty
      else some (string line) *> manyTill (void eol <|> eof) (anycharBut '\n')
   where
    line = commentLine def

  cmtB =
    if null begin || null end
      then fail mempty
      else string begin *> anystringBut end <* string end
   where
    begin = commentBegin def
    end = commentEnd def

  identifier def = do
    found <- liftA2 (:) (idenBegin def) (many $ idenLetter def)
    if isReserved found
      then fail $ unwords ["reserved identifier used:", show found]
      else pure found
   where
    isReserved name
      | caseSensitive def = S.member name set
      | otherwise = S.member (lower name) set
    lower = (toLower <$>)
    reservedNames = reservedWords def
    set
      | caseSensitive def = S.fromList reservedNames
      | otherwise = S.fromList $ lower <$> reservedNames

  operator = do
    op <- specials
    if isReserved op
      then fail $ unwords ["reserved operator used:", show op]
      else pure op
   where
    isReserved o = S.member o set
    set = S.fromList (reservedOps def)

  charLit = genCharLit . string . charLiteral $ def

  stringLit = genStringLit . string . stringLiteral $ def
{-# INLINEABLE genLexer #-}

updateLexer :: Stream s => Lexer s a -> Lexer s a
updateLexer lexer =
  let tidy = tidy' lexer
      gap = gap' lexer
      token = (<* tidy' lexer)
      symbol = token . string
      parens = between (symbol "(") (symbol ")")
      braces = between (symbol "{") (symbol "}")
      angles = between (symbol "<") (symbol ">")
      squares = between (symbol "[") (symbol "]")
   in lexer
        { tidy' = tidy
        , gap' = gap
        , token' = token
        , symbol' = symbol
        , parens' = parens
        , braces' = braces
        , angles' = angles
        , squares' = squares
        }
{-# INLINEABLE updateLexer #-}
