{-# LANGUAGE UndecidableInstances #-}

-- | Core definitions for the 'Unitable' typeclass and interpretations of the 'UniterM' effect.
module Uniter.Core
  ( BoundId (..)
  , Node
  , Event (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

-- | An opaque vertex ID in our graph representation
newtype BoundId = BoundId { unBoundId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, Enum, NFData)

-- | A 'Node' is a structure with all the holes filled with 'BoundId's.
type Node g = g BoundId

data Event g =
    EventAddNode !(Node g) !BoundId
  | EventConstrainEq !BoundId !BoundId !BoundId
  | EventFreshVar !BoundId

deriving instance Eq (Node g) => Eq (Event g)
deriving instance Ord (Node g) => Ord (Event g)
deriving instance Show (Node g) => Show (Event g)
