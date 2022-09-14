module Text.S.Example.JSON where

import           Text.S


-- | Defines the unit of JSON structure
data JSON = NULL            -- null   | null
          | B Bool          -- bool   | true, false
          | N Rational      -- number | 1234, 1.234, 1.234e-9,...
          | S String        -- string | "json-parser"
          | A [JSON]        -- array  | [1, true, "", {}, [],...]
          | O [Pair]        -- object | {"key": JSON}
          deriving Show

-- | Key-Value pair in JSON Object
data Pair = Pair Key JSON
  deriving Show

-- | Key string of of JSON Object
newtype Key = K String
  deriving Show



-- | Parse the whole JSON structure: the outermost function of JSON parser
--
-- >>> import Text.S.Example.JSON
--
-- >>> t' parseJSON "{\"object\": {\"array\": [null, false, 0.125]}}"
-- O [Pair (K "object") (O [Pair (K "array") (A [NULL,B False,N (1 % 8)])])]
--
parseJSON :: Stream s => ParserS s JSON
parseJSON = strip $ choice
  [parseNULL, parseBool, parseNumber, parseString, parseArray, parseObject]
{-# INLINE parseJSON #-}

-- | Parse JSON @__null__@ value
--
-- >>> t' parseNULL "null"
-- NULL
--
parseNULL :: Stream s => ParserS s JSON
parseNULL = NULL <$ strip (symbol "null")
{-# INLINE parseNULL #-}

-- | Parse JSON bool, @__true__@ and @__false__@
--
-- >>> t' parseBool "true"
-- B True
--
-- >>> t' parseBool "false"
-- B False
--
parseBool :: Stream s => ParserS s JSON
parseBool = B <$> choice [true, false]
 where
  true  = True <$ strip (symbol "true")
  false = False <$ strip (symbol "false")
{-# INLINE parseBool #-}

-- | Parse JSON @__number__@ like: @1234, 1.234, 1.234e-9,..@
--
-- >>> t' parseNumber "0.25"
-- N (1 % 4)
--
parseNumber :: Stream s => ParserS s JSON
parseNumber = N . toRational <$> strip float
{-# INLINE parseNumber #-}

-- | Parse JSON @__string__@ like: @"json-parser",..@
--
-- >>> t' parseString "\"JSON-parser\""
-- S "JSON-parser"
--
parseString :: Stream s => ParserS s JSON
parseString = S <$> strip stringLit
{-# INLINE parseString #-}

-- | Parse JSON @__array__@ like: @[1, true, "", {}, [],..]@
--
-- >>> t' parseArray "[2.5, true, \"JSON\"]"
-- A [N (5 % 2),B True,S "JSON"]
--
parseArray :: Stream s => ParserS s JSON
parseArray =
  A <$> between (symbol "[") (symbol "]") (sepBy (symbol ",") parseJSON)
{-# INLINE parseArray #-}

-- | Parse JSON @__object__@ like: @{"key": JSON}@
--
-- >>> t' parseObject "{\"class\": [\"JavaScript\",\"HTML\",\"CSS\"]}"
-- O [Pair (K "class") (A [S "JavaScript",S "HTML",S "CSS"])]
--
parseObject :: Stream s => ParserS s JSON
parseObject = O
  <$> between (symbol "{") (symbol "}") (sepBy (symbol ",") parsePair)
 where
  parseKey  = K <$> strip stringLit
  parsePair = parseKey >>= \key -> Pair key <$> (symbol ":" *> parseJSON)
{-# INLINE parseObject #-}


-- | Wrapper for 'parseJSON' to check if it ends with @EOF@
jsonParser :: Stream s => ParserS s JSON
jsonParser = parseJSON <* eof
{-# INLINE jsonParser #-}

deriving instance Pretty JSON
