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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (lookup)
import Uniter.Core (Node, SynVar (..))

data PreElem g =
    PreElemNode !(Node g)
  | PreElemEq !SynVar !SynVar
  | PreElemFresh

deriving stock instance Eq (g SynVar) => Eq (PreElem g)
deriving stock instance Ord (g SynVar) => Ord (PreElem g)
deriving stock instance Show (g SynVar) => Show (PreElem g)

newtype PreGraph g = PreGraph { unPreGraph :: Map SynVar (PreElem g) }

deriving newtype instance Eq (Node g) => Eq (PreGraph g)
deriving stock instance Show (Node g) => Show (PreGraph g)

empty :: PreGraph g
empty = PreGraph Map.empty

toList :: PreGraph g -> [(SynVar, PreElem g)]
toList = Map.toList . unPreGraph

fromList :: [(SynVar, PreElem g)] -> PreGraph g
fromList = PreGraph . Map.fromList

insert :: SynVar -> PreElem g -> PreGraph g -> PreGraph g
insert x y = PreGraph . Map.insert x y . unPreGraph

lookup :: SynVar -> PreGraph g -> Maybe (PreElem g)
lookup x = Map.lookup x . unPreGraph
