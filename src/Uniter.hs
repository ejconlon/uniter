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
  , module Uniter.Driver
  ) where

import Uniter.Align (Alignable (..), UnalignableErr (..))
import Uniter.Class (Unitable (..), UniterT, addNode, constrainAllEq, constrainEq, freshVar, uniteTerm)
import Uniter.Core (BoundId (..))
import Uniter.Driver
import Uniter.Process (ProcessErr (..))
