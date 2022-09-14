module Text.S.Example.CSV where

import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Text.S


type CSV = [Record]

type Record = Vector String


-- |
parseCSV :: Stream s => ParserS s CSV
parseCSV = sepBy1 eol parseRecord

-- |
parseRecord :: Stream s => ParserS s Record
parseRecord = V.fromList <$> sepBy (symbol ",") field
  where field = stringLit <|> many (noneOf ",\n\r")

-- |
csvParser :: Stream s => ParserS s CSV
csvParser = parseCSV <* eof

deriving instance Pretty Record
