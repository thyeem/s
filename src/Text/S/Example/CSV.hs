module Text.S.Example.CSV where

import Text.S
import Text.S.Lexer

-- | CSV data is a Record set
type CSV = [Record]

-- | Record is represented as List of string field elements
type Record = [String]

-- $setup
-- >>> import Text.S

-- | Parse multiple CSV records separated by end-of-line or @EOL@
--
-- >>> r1 = "\"Letter\",\"Frequency\",\"Percentage\""
-- >>> r2 = "\"A\",24373121,8.1"
-- >>> r3 = "\"B\",4762938,1.6"
-- >>> r4 = "\"C\",8982417,3.0"
-- >>> pp $ ta parseCSV (init . unlines $ [r1,r2,r3,r4])
-- [
--     [ ""Letter""
--     , ""Frequency""
--     , ""Percentage""
--     ]
-- ,
--     [ ""A""
--     , "24373121"
--     , "8.1"
--     ]
-- ,
--     [ ""B""
--     , "4762938"
--     , "1.6"
--     ]
-- ,
--     [ ""C""
--     , "8982417"
--     , "3.0"
--     ]
-- ]
--
-- >>> csv ="key,val\n1,{\"type\":\"point\",\"coord\":\"[0.5,-1.5]\"}"
-- >>> pp $ ta parseCSV csv
-- [
--     [ "key"
--     , "val"
--     ]
-- ,
--     [ "1"
--     , "{"type":"point""
--     , ""coord":"[0.5,-1.5]"}"
--     ]
-- ]
parseCSV :: Stream s => S s CSV
parseCSV = filter (not . null) <$> sepBy eol parseRecord
{-# INLINE parseCSV #-}

-- | Parse Comma-separated values from a single 'Record'
--
-- >>> record = "36,1963,47,\"Gregory Peck\",\"To Kill a Mockingbird\""
-- >>> pp $ ta parseRecord record
-- [ "36"
-- , "1963"
-- , "47"
-- , ""Gregory Peck""
-- , ""To Kill a Mockingbird""
-- ]
parseRecord :: Stream s => S s Record
parseRecord = sepBy (symbol ",") field
 where
  field = concat <$> many (quoted <|> unquoted)
  quoted = show <$> stringLit
  unquoted = some (noneOf ",\"\n\r")
{-# INLINE parseRecord #-}

-- | Wrapper for 'parseCSV' to check if it ends with @EOF@
csvParser :: Stream s => S s CSV
csvParser = parseCSV <* eof
{-# INLINE csvParser #-}
