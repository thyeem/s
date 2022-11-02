-----------------------------------------------------------------------------
-- |
-- Module      : Text.S
-- License     : MIT
--
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
--
-- The parser-S is a generalized parser and its combinator easy-to-read.
-- It is designed to focus on usability, scalability and non-verbosity.
--
-----------------------------------------------------------------------------

module Text.S
  ( module Text.S.Base
  , module Text.S.Combinator
  , module Text.S.Internal
  , module Text.S.Lexeme
  , module Text.S.Language
  , module Text.S.Expr
  ) where

import           Text.S.Base
import           Text.S.Combinator
import           Text.S.Expr
import           Text.S.Internal
import           Text.S.Language
import           Text.S.Lexeme
