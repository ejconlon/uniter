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
import Uniter.Core (BoundId (..), Unitable (..), UniterT, addNode, constrainAllEq, constrainEq, freshVar, uniteTerm)
import Uniter.Driver
import Uniter.Process (ProcessErr (..))
