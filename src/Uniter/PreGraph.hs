{-# LANGUAGE UndecidableInstances #-}

module Uniter.PreGraph
  ( PreElem (..)
  , PreGraph (..)
  , empty
  , toList
  , fromList
  , insert
  , lookup
  ) where

import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import Prelude hiding (lookup)
import Uniter.Core (BoundId (..), Node)

data PreElem g =
    PreElemNode !(Node g)
  | PreElemEq !BoundId !BoundId
  | PreElemFresh

deriving stock instance Eq (g BoundId) => Eq (PreElem g)
deriving stock instance Ord (g BoundId) => Ord (PreElem g)
deriving stock instance Show (g BoundId) => Show (PreElem g)

newtype PreGraph g = PreGraph { unPreGraph :: IntLikeMap BoundId (PreElem g) }

deriving newtype instance Eq (Node g) => Eq (PreGraph g)
deriving stock instance Show (Node g) => Show (PreGraph g)

empty :: PreGraph g
empty = PreGraph ILM.empty

toList :: PreGraph g -> [(BoundId, PreElem g)]
toList = ILM.toList . unPreGraph

fromList :: [(BoundId, PreElem g)] -> PreGraph g
fromList = PreGraph . ILM.fromList

insert :: BoundId -> PreElem g -> PreGraph g -> PreGraph g
insert x y = PreGraph . ILM.insert x y . unPreGraph

lookup :: BoundId -> PreGraph g -> Maybe (PreElem g)
lookup x = ILM.lookup x . unPreGraph
