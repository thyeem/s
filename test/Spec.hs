import           Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "-fobject-code"
  , "-XDeriveGeneric"
  , "-XDeriveAnyClass"
  , "-XFlexibleInstances"
  , "-XRankNTypes"
  , "-XRecordWildCards"
  , "-XTupleSections"
  , "-XTypeSynonymInstances"
  , "src/Text/S.hs"
  , "src/Text/S/Base.hs"
  , "src/Text/S/Combinator.hs"
  , "src/Text/S/Internal.hs"
  , "src/Text/S/Language.hs"
  , "src/Text/S/Lexeme.hs"
  ]
