-- | Slimmest useful interface
-- TODO finish with stuff from Interface
module Uniter
  ( MonadHalt (..)
  , Alignable (..)
  , UnalignableErr (..)
  , BoundId (..)
  , Unitable (..)
  , uniterEmitEq
  , uniterEmitAllEq
  , uniterAddNode
  , uniterFresh
  , uniteTerm
  , GraphState
  , ProcessErr (..)
  , module Uniter.FreeEnv
  , module Uniter.Interface
  ) where

import Uniter.Align (Alignable (..), UnalignableErr (..))
import Uniter.Core (BoundId (..), Unitable (..), uniteTerm, uniterAddNode, uniterEmitAllEq, uniterEmitEq, uniterFresh)
import Uniter.FreeEnv
import Uniter.Graph (GraphState)
import Uniter.Halt (MonadHalt (..))
import Uniter.Interface
import Uniter.Process (ProcessErr (..))
