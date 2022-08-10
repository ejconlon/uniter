{-# LANGUAGE UndecidableInstances #-}

module Uniter.Graph
  ( Elem (..)
  , elemTraversal
  , Graph (..)
  , empty
  , toList
  , fromList
  , insert
  , lookup
  , resolveVar
  , resolveNode
  , weakenElem
  , weaken
  ) where

import Data.Bifunctor (second)
import Data.Functor.Foldable (Base, Corecursive (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lens.Micro (Traversal')
import Prelude hiding (lookup)
import Uniter.Core (Node, SynVar (..))
import Uniter.PreGraph (PreElem (..), PreGraph)
import qualified Uniter.PreGraph as UP

data Elem g =
    ElemNode !(Node g)
  | ElemFresh

deriving stock instance Eq (g SynVar) => Eq (Elem g)
deriving stock instance Ord (g SynVar) => Ord (Elem g)
deriving stock instance Show (g SynVar) => Show (Elem g)

-- | A traversal over 'Elem's - can get or replace all 'SynVar's
elemTraversal :: Traversable g => Traversal' (Elem g) SynVar
elemTraversal f = \case
  ElemFresh -> pure ElemFresh
  ElemNode fb -> fmap ElemNode (traverse f fb)

newtype Graph g = Graph { unGraph :: Map SynVar (Elem g) }

deriving newtype instance Eq (Node g) => Eq (Graph g)
deriving stock instance Show (Node g) => Show (Graph g)

empty :: Graph g
empty = Graph Map.empty

toList :: Graph g -> [(SynVar, Elem g)]
toList = Map.toList . unGraph

fromList :: [(SynVar, Elem g)] -> Graph g
fromList = Graph . Map.fromList

insert :: SynVar -> Elem g -> Graph g -> Graph g
insert x y = Graph . Map.insert x y . unGraph

lookup :: SynVar -> Graph g -> Maybe (Elem g)
lookup x = Map.lookup x . unGraph

-- TODO need to be careful about recursive structures
-- Also need to be careful to share memory for extracted items through memoization
resolveVar :: (Corecursive u, Base u ~ g, Traversable g) => SynVar -> Graph g -> Either SynVar u
resolveVar v gr@(Graph m) =
  case Map.lookup v m of
    Nothing -> Left v
    Just j ->
      case j of
        ElemNode x -> resolveNode x gr
        _ -> Left v

-- See notes on resolveVar
resolveNode :: (Corecursive u, Base u ~ g, Traversable g) => Node g -> Graph g -> Either SynVar u
resolveNode n gr = fmap embed (traverse (`resolveVar` gr) n)

weakenElem :: Elem g -> PreElem g
weakenElem = \case
  ElemNode g -> PreElemNode g
  ElemFresh -> PreElemFresh

weaken :: Graph g -> PreGraph g
weaken = UP.fromList . fmap (second weakenElem) . toList
