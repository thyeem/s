-- |
-- Module      : Text.S
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
--
-- Parser S is a generailized parser combinator easy-to-use/read.
-- Not verbose at all, rather the most simplified yet robust.
--
module Text.S
  ( module Text.S
  , module Text.S.Internal
  , module Text.S.Combinator
  , module Text.S.Token
  , module Text.S.Language
  ) where

import           Text.RawString.QQ              ( r )
import           Text.S.Combinator
import           Text.S.Internal
import           Text.S.Language
import           Text.S.Token



type Parser = Parser'S String
-- type Parser a = Parser'S C.ByteString a

parseFromFile
  :: Stream s => Parser'S s a -> FilePath -> IO (Either ParseError a)
parseFromFile parser file = do
  stream <- readStream file
  let state = initState file stream
  return . parse parser $ state

parse :: Stream s => Parser'S s a -> State s -> Either ParseError a
parse parser state = case result of
  Ok    ok  -> Right ok
  Error err -> Left err
  where Return result state' = runParser parser state

runParser :: Stream s => Parser'S s a -> State s -> Return a s
runParser parser state = unpack parser state fOk fError
 where
  fOk    = Return . Ok
  fError = Return . Error





---------------------
-- debug section
---------------------

fakeSource :: Source
fakeSource = initSource "/fake/path/fakeName.s"

fakeErrorMessages :: [Message]
fakeErrorMessages =
  [ "Error: The Sun has become a blackhole."
  , "Error: The Riemann conjecture has just proved."
  ]

fakeError :: ParseError
fakeError = ParseError fakeSource fakeErrorMessages


testStream = [r|
  public class Simple {

    /* block comment
    */

    public static void main(String[] args) { /* Wow  */
        System.out.println("get/*ting better");
    }
    // line comment
}
|]


testSource = initSource "simple.java"

testErrors = replicate 3 fakeError

testState = State testStream testSource testErrors

debugs s = State s fakeSource []
