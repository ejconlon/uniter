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
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import Lens.Micro (Traversal')
import Prelude hiding (lookup)
import Uniter.Core (BoundId (..), Node)
import Uniter.PreGraph (PreElem (..), PreGraph)
import qualified Uniter.PreGraph as UP

data Elem g =
    ElemNode !(Node g)
  | ElemFresh

deriving stock instance Eq (g BoundId) => Eq (Elem g)
deriving stock instance Ord (g BoundId) => Ord (Elem g)
deriving stock instance Show (g BoundId) => Show (Elem g)

-- | A traversal over 'Elem's - can get or replace all 'BoundId's
elemTraversal :: Traversable g => Traversal' (Elem g) BoundId
elemTraversal f = \case
  ElemFresh -> pure ElemFresh
  ElemNode fb -> fmap ElemNode (traverse f fb)

newtype Graph g = Graph { unGraph :: IntLikeMap BoundId (Elem g) }

deriving newtype instance Eq (Node g) => Eq (Graph g)
deriving stock instance Show (Node g) => Show (Graph g)

empty :: Graph g
empty = Graph ILM.empty

toList :: Graph g -> [(BoundId, Elem g)]
toList = ILM.toList . unGraph

fromList :: [(BoundId, Elem g)] -> Graph g
fromList = Graph . ILM.fromList

insert :: BoundId -> Elem g -> Graph g -> Graph g
insert x y = Graph . ILM.insert x y . unGraph

lookup :: BoundId -> Graph g -> Maybe (Elem g)
lookup x = ILM.lookup x . unGraph

-- TODO need to be careful about recursive structures
-- Also need to be careful to share memory for extracted items through memoization
resolveVar :: (Corecursive u, Base u ~ g, Traversable g) => BoundId -> Graph g -> Either BoundId u
resolveVar v gr@(Graph m) =
  case ILM.lookup v m of
    Nothing -> Left v
    Just j ->
      case j of
        ElemNode x -> resolveNode x gr
        _ -> Left v

-- See notes on resolveVar
resolveNode :: (Corecursive u, Base u ~ g, Traversable g) => Node g -> Graph g -> Either BoundId u
resolveNode n gr = fmap embed (traverse (`resolveVar` gr) n)

weakenElem :: Elem g -> PreElem g
weakenElem = \case
  ElemNode g -> PreElemNode g
  ElemFresh -> PreElemFresh

weaken :: Graph g -> PreGraph g
weaken = UP.fromList . fmap (second weakenElem) . toList
