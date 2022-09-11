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
  , SimpleResErr (..)
  , resolveVar
  , ComplexResErr (..)
  , resolveGenVar
  , resolveTm
  , weakenElem
  , weaken
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState, StateT (..), evalStateT, gets, modify')
import Data.Bifunctor (second)
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Corecursive (..))
import Data.Sequence (Seq (..))
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import qualified IntLike.Set as ILS
import Lens.Micro (Traversal')
import Prelude hiding (lookup)
import Uniter.Core (ForAll (..), GenBinder, GenQuant, GenTy, Node, Quant (..), SpecTm, TyVar, UniqueId (..), embedGenTy)
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

data ResEnv g = ResEnv
  { reGraph :: !(Graph g)
  , rePath :: !(IntLikeSet UniqueId)
  }

data SimpleResErr =
    SimpleResErrLoop !UniqueId
  | SimpleResErrNotFound !UniqueId
  | SimpleResErrUnsolvedMeta !UniqueId
  | SimpleResErrUnsupportedSkolem !UniqueId !TyVar
  deriving stock (Eq, Show)

instance Exception SimpleResErr

newtype SimpleResM g u a = SimpleResM { unSimpleResM :: ReaderT (ResEnv g) (StateT (IntLikeMap UniqueId u) (Except SimpleResErr)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (ResEnv g), MonadState (IntLikeMap UniqueId u), MonadError SimpleResErr)

runSimpleResM :: SimpleResM g u a -> Graph g -> Either SimpleResErr a
runSimpleResM m gr = runExcept (evalStateT (runReaderT (unSimpleResM m) (ResEnv gr ILS.empty)) ILM.empty)

resolveVarM :: (Corecursive u, Base u ~ g, Traversable g) => UniqueId -> SimpleResM g u u
resolveVarM v = do
  mw <- gets (ILM.lookup v)
  case mw of
    Just w -> pure w
    Nothing -> do
      ResEnv (Graph m) path <- ask
      if ILS.member v path
        then throwError (SimpleResErrLoop v)
        else case ILM.lookup v m of
          Nothing -> throwError (SimpleResErrNotFound v)
          Just j -> do
            w <- case j of
              ElemNode x -> local (\re -> re { rePath = ILS.insert v (rePath re) }) (resolveNodeM x)
              ElemMeta -> throwError (SimpleResErrUnsolvedMeta v)
              ElemSkolem tyv -> throwError (SimpleResErrUnsupportedSkolem v tyv)
            modify' (ILM.insert v w)
            pure w

resolveNodeM :: (Corecursive u, Base u ~ g, Traversable g) => Node g -> SimpleResM g u u
resolveNodeM n = fmap embed (traverse resolveVarM n)

resolveVar :: (Corecursive u, Base u ~ g, Traversable g) => UniqueId -> Graph g -> Either SimpleResErr u
resolveVar = runSimpleResM . resolveVarM

data ComplexResErr =
    ComplexResErrLoop !UniqueId
  | ComplexResErrNotFound !UniqueId
  deriving stock (Eq, Show)

instance Exception ComplexResErr

data ComplexResSt g = ComplexResSt
  { crsCache :: !(IntLikeMap UniqueId (GenTy g))
  , crsVars :: !(Seq GenBinder)
  }

newtype ComplexResM g a = ComplexResM { unComplexResM :: ReaderT (ResEnv g) (StateT (ComplexResSt g) (Except ComplexResErr)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (ResEnv g), MonadState (ComplexResSt g), MonadError ComplexResErr)

runComplexResM :: ComplexResM g a -> Graph g -> Either ComplexResErr (Quant GenBinder a)
runComplexResM m gr = fmap (\(a, st) -> go st a) (runExcept (runStateT (runReaderT (unComplexResM m) (ResEnv gr ILS.empty)) (ComplexResSt ILM.empty Empty))) where
  go (ComplexResSt _ vars) a =
    case vars of
      Empty -> QuantBare a
      _ -> QuantForAll (ForAll vars a)

resolveGenVarM :: Traversable g => UniqueId -> ComplexResM g (GenTy g)
resolveGenVarM v = do
  mw <- gets (ILM.lookup v . crsCache)
  case mw of
    Just w -> pure w
    Nothing -> do
      ResEnv (Graph m) path <- ask
      if ILS.member v path
        then throwError (ComplexResErrLoop v)
        else case ILM.lookup v m of
          Nothing -> throwError (ComplexResErrNotFound v)
          Just j -> do
            w <- case j of
              ElemNode x -> local (\re -> re { rePath = ILS.insert v (rePath re) }) (resolveGenNodeM x)
              ElemMeta -> error "TODO"
              ElemSkolem _tyv -> error "TODO"
            modify' (\st -> st { crsCache = ILM.insert v w (crsCache st) })
            pure w

resolveGenNodeM :: Traversable g => Node g -> ComplexResM g (GenTy g)
resolveGenNodeM = fmap embedGenTy . traverse resolveGenVarM

resolveGenVar :: Traversable g => UniqueId -> Graph g -> Either ComplexResErr (GenQuant g)
resolveGenVar = runComplexResM . resolveGenVarM

resolveTm :: (Bitraversable h, Traversable g) => SpecTm h UniqueId -> Graph g -> Either ComplexResErr (SpecTm h (GenQuant g))
resolveTm h gr = traverse (`resolveGenVar` gr) h

weakenElem :: Elem g -> PreElem g
weakenElem = \case
  ElemNode g -> PreElemNode g
  ElemMeta -> PreElemMeta
  ElemSkolem tyv -> PreElemSkolem tyv

weaken :: Graph g -> PreGraph g
weaken = UP.fromList . fmap (second weakenElem) . toList
