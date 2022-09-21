{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.S.Example.CSV where

import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Text.S                         ( (<|>)
                                                , ParserS
                                                , Pretty
                                                , Stream
                                                , between
                                                , endBy
                                                , eof
                                                , eol
                                                , many
                                                , noneOf
                                                , sepBy
                                                , sepBy1
                                                , skipOptional
                                                , symbol
                                                )


-- | CSV data is a Record set
type CSV = [Record]

-- | Record is represented as Vector of string field elements
type Record = Vector String

-- $setup
-- >>> import Text.S

-- | Parse multiple CSV records separated by end-of-line or @EOL@
--
-- >>> r1 = "\"Letter\",\"Frequency\",\"Percentage\""
-- >>> r2 = "\"A\",24373121,8.1"
-- >>> r3 = "\"B\",4762938,1.6"
-- >>> r4 = "\"C\",8982417,3.0"
-- >>> pp $ t' parseCSV (unlines [r1,r2,r3,r4])
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
-- , [ "" ]
-- ]
--
-- >>> csv ="key,val\n1,{\"type\":\"point\",\"coord\":\"[0.5,-1.5]\"}"
-- >>> pp $ t' parseCSV csv
-- [
--     [ "key"
--     , "val"
--     ]
-- ,
--     [ "1"
--     , "{"type":"point""
--     , ""coord":"[0.5"
--     , "-1.5]"}"
--     ]
-- ]
--
parseCSV :: Stream s => ParserS s CSV
parseCSV = sepBy eol parseRecord
{-# INLINE parseCSV #-}

-- | Parse Comma-separated values from a single 'Record'
--
-- >>> record = "36,1963,47,\"Gregory Peck\",\"To Kill a Mockingbird\""
-- >>> pp $ t' parseRecord record
-- [ "36"
-- , "1963"
-- , "47"
-- , ""Gregory Peck""
-- , ""To Kill a Mockingbird""
-- ]
--
parseRecord :: Stream s => ParserS s Record
parseRecord = V.fromList <$> sepBy (symbol ",") (go [])
 where
  go x = field >>= \f -> if null f then pure x else go (x <> f)
  field  = quoted <|> many (noneOf ",\n\r")
  quoted = show <$> between (symbol "\"") (symbol "\"") (many $ noneOf "\"")
{-# INLINE parseRecord #-}

-- | Wrapper for 'parseCSV' to check if it ends with @EOF@
csvParser :: Stream s => ParserS s CSV
csvParser = parseCSV <* eof
{-# INLINE csvParser #-}

deriving instance Pretty Record
