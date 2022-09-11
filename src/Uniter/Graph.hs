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
  , resolveTm
  , weakenElem
  , weaken
  ) where

import Data.Bifunctor (second)
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Corecursive (..))
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import Lens.Micro (Traversal')
import Prelude hiding (lookup)
import Uniter.Core (Node, SpecTm, TyVar, UniqueId (..))
import Uniter.PreGraph (PreElem (..), PreGraph)
import qualified Uniter.PreGraph as UP

data Elem g =
    ElemNode !(Node g)
  | ElemMeta
  | ElemSkolem !TyVar

deriving stock instance Eq (Node g) => Eq (Elem g)
deriving stock instance Ord (Node g) => Ord (Elem g)
deriving stock instance Show (Node g) => Show (Elem g)

-- | A traversal over 'Elem's - can get or replace all 'UniqueId's
elemTraversal :: Traversable g => Traversal' (Elem g) UniqueId
elemTraversal f = \case
  ElemNode fb -> fmap ElemNode (traverse f fb)
  ElemMeta -> pure ElemMeta
  ElemSkolem tyv -> pure (ElemSkolem tyv)

newtype Graph g = Graph { unGraph :: IntLikeMap UniqueId (Elem g) }

deriving newtype instance Eq (Node g) => Eq (Graph g)
deriving stock instance Show (Node g) => Show (Graph g)

empty :: Graph g
empty = Graph ILM.empty

toList :: Graph g -> [(UniqueId, Elem g)]
toList = ILM.toList . unGraph

fromList :: [(UniqueId, Elem g)] -> Graph g
fromList = Graph . ILM.fromList

insert :: UniqueId -> Elem g -> Graph g -> Graph g
insert x y = Graph . ILM.insert x y . unGraph

lookup :: UniqueId -> Graph g -> Maybe (Elem g)
lookup x = ILM.lookup x . unGraph

-- TODO need to be careful about recursive structures
-- Also need to be careful to share memory for extracted items through memoization
resolveVar :: (Corecursive u, Base u ~ g, Traversable g) => UniqueId -> Graph g -> Either UniqueId u
resolveVar v gr@(Graph m) =
  case ILM.lookup v m of
    Nothing -> Left v
    Just j ->
      case j of
        ElemNode x -> resolveNode x gr
        _ -> Left v

-- See notes on resolveVar
resolveNode :: (Corecursive u, Base u ~ g, Traversable g) => Node g -> Graph g -> Either UniqueId u
resolveNode n gr = fmap embed (traverse (`resolveVar` gr) n)

-- See notes on resolveVar
resolveTm :: (Bitraversable h, Corecursive u, Base u ~ g, Traversable g) => SpecTm h UniqueId -> Graph g -> Either UniqueId (SpecTm h u)
resolveTm h gr = traverse (`resolveVar` gr) h

weakenElem :: Elem g -> PreElem g
weakenElem = \case
  ElemNode g -> PreElemNode g
  ElemMeta -> PreElemMeta
  ElemSkolem tyv -> PreElemSkolem tyv

weaken :: Graph g -> PreGraph g
weaken = UP.fromList . fmap (second weakenElem) . toList
