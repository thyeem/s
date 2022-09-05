module Text.S.Example.JSON where

import           Text.S


-- | Defines the unit of JSON structure
data JSON = NULL            -- null   | null
          | B Bool          -- bool   | true, false
          | N Rational      -- number | 1234, 1.234, 1.234e-9,...
          | S String        -- string | "json-parser"
          | A [JSON]        -- array  | [1, true, "", {}, [],...]
          | O [KeyValue]    -- object | {"key": JSON}
          deriving Show

-- | Key-Value pair in JSON Object
data KeyValue = KV Key JSON
  deriving Show

-- | Key string of of JSON Object
newtype Key = K String
  deriving Show

-- |
parseJSON :: Parser JSON
parseJSON = strip $ choice
  [parseNULL, parseBool, parseNumber, parseString, parseArray, parseObject]

-- | parse JSON nil-value -> null
parseNULL :: Parser JSON
parseNULL = NULL <$ strip (symbol "null")

-- | parse JSON bool -> true, false
parseBool :: Parser JSON
parseBool = B <$> choice [true, false]
 where
  true  = True <$ strip (symbol "true")
  false = False <$ strip (symbol "false")

-- | parse JSON number like: 1234, 1.234, 1.234e-9,...
parseNumber :: Parser JSON
parseNumber = N . toRational <$> strip float

-- | parse JSON string like: "json-parser",...
parseString :: Parser JSON
parseString = S <$> strip stringLit

-- | parse JSON array like: [1, true, "", {}, [],...]
parseArray :: Parser JSON
parseArray = A <$> between (char '[') (char ']') (sepBy parseJSON (char ','))

-- | parse JSON Object like: {"key": JSON}
parseObject :: Parser JSON
parseObject = O
  <$> between (char '{') (char '}') (sepBy parseKeyValue (char ','))
 where
  parseKey      = K <$> strip stringLit
  parseKeyValue = parseKey >>= \key -> KV key <$> (char ':' *> parseJSON)
