{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.S.Example.JSON where

import Text.S
import Text.S.Lexer

-- | Defines the unit of JSON structure
data JSON
  = NULL -- null   | null
  | B Bool -- bool   | true, false
  | N Rational -- number | 1234, 1.234, 1.234e-9,...
  | Q String -- string | "json-parser"
  | A [JSON] -- array  | [1, true, "", {}, [],...]
  | O [Pair] -- object | {"key": JSON}
  deriving (Show)

-- | Key-Value pair in JSON Object
data Pair = Pair Key JSON deriving (Show)

-- | Key string of of JSON Object
newtype Key = K String deriving (Show)

-- | Parse the whole JSON structure: the outermost function of JSON parser
--
-- >>> import Text.S.Example.JSON
--
-- >>> ta parseJSON "{\"object\": {\"array\": [null, false, 0.125]}}"
-- O [Pair (K "object") (O [Pair (K "array") (A [NULL,B False,N (1 % 8)])])]
parseJSON :: Stream s => S s JSON
parseJSON =
  strip $
    choice [parseString, parseObject, parseArray, parseNULL, parseBool, parseNumber]
{-# INLINE parseJSON #-}

-- | Parse JSON @__null__@ value
--
-- >>> ta parseNULL "null"
-- NULL
parseNULL :: Stream s => S s JSON
parseNULL = NULL <$ symbol "null"
{-# INLINE parseNULL #-}

-- | Parse JSON bool, @__true__@ and @__false__@
--
-- >>> ta parseBool "true"
-- B True
--
-- >>> ta parseBool "false"
-- B False
parseBool :: Stream s => S s JSON
parseBool = B <$> choice [true, false]
 where
  true = True <$ symbol "true"
  false = False <$ symbol "false"
{-# INLINE parseBool #-}

-- | Parse JSON @__number__@ like: @1234, 1.234, 1.234e-9,..@
--
-- >>> ta parseNumber "0.25"
-- N (1 % 4)
parseNumber :: Stream s => S s JSON
parseNumber = N . toRational <$> number
{-# INLINE parseNumber #-}

-- | Parse JSON @__string__@ like: @"json-parser",..@
--
-- >>> ta parseString "\"JSON-parser\""
-- Q "JSON-parser"
parseString :: Stream s => S s JSON
parseString = Q <$> stringLit
{-# INLINE parseString #-}

-- | Parse JSON @__array__@ like: @[1, true, "", {}, [],..]@
--
-- >>> ta parseArray "[2.5, true, \"JSON\"]"
-- A [N (5 % 2),B True,Q "JSON"]
parseArray :: Stream s => S s JSON
parseArray = A <$> between (symbol "[") (symbol "]") (sepBy (symbol ",") parseJSON)
{-# INLINE parseArray #-}

-- | Parse JSON @__object__@ like: @{"key": JSON}@
--
-- >>> ta parseObject "{\"class\": [\"JavaScript\",\"HTML\",\"CSS\"]}"
-- O [Pair (K "class") (A [Q "JavaScript",Q "HTML",Q "CSS"])]
parseObject :: Stream s => S s JSON
parseObject = O <$> between (symbol "{") (symbol "}") (sepBy (symbol ",") parsePair)
 where
  parseKey = K <$> stringLit
  parsePair = parseKey >>= \key -> Pair key <$> (symbol ":" *> parseJSON)
{-# INLINE parseObject #-}

-- | Wrapper for 'parseJSON' to check if it ends with @EOF@
jsonParser :: Stream s => S s JSON
jsonParser = parseJSON <* eof
{-# INLINE jsonParser #-}

deriving instance Pretty JSON
