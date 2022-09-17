module Text.S.Example.CSV where

import           Control.Applicative            ( liftA2 )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Text.S


type CSV = [Record]

type Record = Vector String


-- | Parse multiple CSV records separated by end-of-line or @EOL@
parseCSV :: Stream s => ParserS s CSV
parseCSV = endBy1 eol parseRecord
{-# INLINE parseCSV #-}

-- | Parse Comma-separated values from a single 'Record'
parseRecord :: Stream s => ParserS s Record
parseRecord = V.fromList <$> sepBy (symbol ",") field
 where
  field = go []
   where
    go x = do
      f <- g
      if null f then pure x else go (x <> f)

    g      = show <$> quoted <|> many (noneOf ",\n\r")
    quoted = between (symbol "\"") (symbol "\"") (anystringBut "\"")

{-# INLINE parseRecord #-}

-- | Wrapper for 'parseCSV' to check if it ends with @EOF@
csvParser :: Stream s => ParserS s CSV
csvParser = parseCSV <* eof
{-# INLINE csvParser #-}

deriving instance Pretty Record
