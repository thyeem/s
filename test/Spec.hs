import           Data.List
import           GHC.TypeNats
import           Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Text/S/Combinator.hs", "src/Text/S/Internal.hs"]
