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
import Uniter.Core (Node, TyBinder, UniqueId (..))

data PreElem g =
    PreElemNode !(Node g)
  | PreElemEq !UniqueId !UniqueId
  | PreElemMeta !TyBinder
  | PreElemSkolem !TyBinder

deriving stock instance Eq (Node g) => Eq (PreElem g)
deriving stock instance Ord (Node g) => Ord (PreElem g)
deriving stock instance Show (Node g) => Show (PreElem g)

newtype PreGraph g = PreGraph { unPreGraph :: IntLikeMap UniqueId (PreElem g) }

deriving newtype instance Eq (Node g) => Eq (PreGraph g)
deriving stock instance Show (Node g) => Show (PreGraph g)

empty :: PreGraph g
empty = PreGraph ILM.empty

toList :: PreGraph g -> [(UniqueId, PreElem g)]
toList = ILM.toList . unPreGraph

fromList :: [(UniqueId, PreElem g)] -> PreGraph g
fromList = PreGraph . ILM.fromList

insert :: UniqueId -> PreElem g -> PreGraph g -> PreGraph g
insert x y = PreGraph . ILM.insert x y . unPreGraph

lookup :: UniqueId -> PreGraph g -> Maybe (PreElem g)
lookup x = ILM.lookup x . unPreGraph
