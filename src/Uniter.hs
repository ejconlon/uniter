-- | Slimmest useful interface
-- TODO finish with stuff from Interface
module Uniter
  ( Alignable (..)
  , Unitable (..)
  , uniterEmitEq
  , uniterAddNode
  , uniterFresh
  , uniteTerm
  ) where

import Uniter.Align (Alignable (..))
import Uniter.Core (Unitable (..), uniteTerm, uniterAddNode, uniterEmitEq, uniterFresh)
