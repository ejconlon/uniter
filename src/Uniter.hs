-- | Slimmest useful interface
module Uniter
  ( Alignable (..)
  , UnalignableErr (..)
  , BoundId (..)
  , UniterT
  , Unitable (..)
  , constrainEq
  , constrainAllEq
  , addNode
  , freshVar
  , uniteTerm
  , ProcessErr (..)
  , module Uniter.Unitable.Driver
  ) where

import Uniter.Align (Alignable (..), UnalignableErr (..))
import Uniter.Core (BoundId (..))
import Uniter.Monad (UniterT, addNode, constrainAllEq, constrainEq, freshVar)
import Uniter.Process (ProcessErr (..))
import Uniter.Unitable.Class (Unitable (..), uniteTerm)
import Uniter.Unitable.Driver
