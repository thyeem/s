import System.FilePath.Glob (glob)
import Test.DocTest

main :: IO ()
main = do
  paths <- glob "src/**/*.hs"
  doctest $ "-isrc" : paths
