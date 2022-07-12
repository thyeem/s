module S where

data SourceLoc = SourceLoc
  { sourceName   :: FilePath
  , sourceLine   :: !Int
  , sourceColumn :: !Int
  }
  deriving (Show, Eq)

initSourceLoc :: FilePath -> SourceLoc
initSourceLoc n = SourceLoc n 1 1
