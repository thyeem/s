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
jsonParser :: Parser JSON
jsonParser = strip $ choice
  [parseNULL, parseBool, parseNumber, parseString, parseArray, parseObject]
{-# INLINE jsonParser #-}

-- | Parse JSON nil-value -> null
parseNULL :: Parser JSON
parseNULL = NULL <$ strip (symbol "null")
{-# INLINE parseNULL #-}

-- | Parse JSON bool -> true, false
parseBool :: Parser JSON
parseBool = B <$> choice [true, false]
 where
  true  = True <$ strip (symbol "true")
  false = False <$ strip (symbol "false")
{-# INLINE parseBool #-}

-- | Parse JSON number like: 1234, 1.234, 1.234e-9,...
parseNumber :: Parser JSON
parseNumber = N . toRational <$> strip float
{-# INLINE parseNumber #-}

-- | Parse JSON string like: "json-parser",...
parseString :: Parser JSON
parseString = S <$> strip stringLit
{-# INLINE parseString #-}

-- | Parse JSON array like: [1, true, "", {}, [],...]
parseArray :: Parser JSON
parseArray = A <$> between (char '[') (char ']') (sepBy (char ',') jsonParser)
{-# INLINE parseArray #-}

-- | Parse JSON Object like: {"key": JSON}
parseObject :: Parser JSON
parseObject = O <$> between (char '{') (char '}') (sepBy (char ',') parsePair)
 where
  parseKey  = K <$> strip stringLit
  parsePair = parseKey >>= \key -> Pair key <$> (char ':' *> jsonParser)
{-# INLINE parseObject #-}


deriving instance Pretty JSON
