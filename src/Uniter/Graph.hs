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
import Control.Monad ((>=>))
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT, gets, modify')
import Control.Monad.Trans.Writer.Strict (WriterT (..))
import Data.Bifunctor (Bifunctor (..), second)
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Corecursive (..))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import qualified IntLike.Set as ILS
import Lens.Micro (Traversal')
import Prelude hiding (lookup)
import Uniter.Core (BoundTy (..), BoundTyF (..), ForAll (..), GenQuant, Index (..), Node, Quant (..),
                    SpecTm (..), TyVar, UniqueId (..), embedBoundTy, SpecFinal, SpecTmF (..), SpecInit)
import Uniter.PreGraph (PreElem (..), PreGraph)
import qualified Uniter.PreGraph as UP

data Elem g =
    ElemNode !(Node g)
  | ElemMeta !(Maybe TyVar)
  | ElemSkolem !TyVar

deriving stock instance Eq (Node g) => Eq (Elem g)
deriving stock instance Ord (Node g) => Ord (Elem g)
deriving stock instance Show (Node g) => Show (Elem g)

-- | A traversal over 'Elem's - can get or replace all 'UniqueId's
elemTraversal :: Traversable g => Traversal' (Elem g) UniqueId
elemTraversal f = \case
  ElemNode fb -> fmap ElemNode (traverse f fb)
  e -> pure e

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
  | SimpleResErrUnsolvedMeta !UniqueId !(Maybe TyVar)
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
              ElemMeta mtyv -> throwError (SimpleResErrUnsolvedMeta v mtyv)
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

newtype ResIndex = ResIndex { unResIndex :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

data ComplexResSt g = ComplexResSt
  { crsCache :: !(IntLikeMap UniqueId (BoundTy g ResIndex, Set ResIndex))
  , crsVars :: !(Seq (Maybe TyVar))
  }

newtype ComplexResM g a = ComplexResM { unComplexResM :: ReaderT (ResEnv g) (StateT (ComplexResSt g) (Except ComplexResErr)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (ResEnv g), MonadState (ComplexResSt g), MonadError ComplexResErr)

runComplexResM :: ComplexResM g a -> Graph g -> Either ComplexResErr a
runComplexResM m gr = fmap fst (runExcept (runStateT (runReaderT (unComplexResM m) (ResEnv gr ILS.empty)) (ComplexResSt ILM.empty Empty)))

newVarM :: Maybe TyVar -> ComplexResM g (BoundTy g ResIndex, Set ResIndex)
newVarM mtyv = do
  ix <- state $ \st ->
    let ix = ResIndex (Seq.length (crsVars st))
        st' = st { crsVars = crsVars st :|> mtyv }
      in (ix, st')
  pure (BoundTy (BoundTyVarF ix), Set.singleton ix)

resolveGenVarM :: Traversable g => UniqueId -> ComplexResM g (BoundTy g ResIndex, Set ResIndex)
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
            p <- case j of
              ElemNode x -> local (\re -> re { rePath = ILS.insert v (rePath re) }) (resolveGenNodeM x)
              ElemMeta mtyv -> newVarM mtyv
              ElemSkolem tyv -> newVarM (Just tyv)
            modify' (\st -> st { crsCache = ILM.insert v p (crsCache st) })
            pure p

traverseWriter :: (Monoid w, Applicative m, Traversable g) => (a -> m (b, w)) -> g a -> m (g b, w)
traverseWriter f ga = runWriterT (traverse (WriterT . f) ga)

resolveGenNodeM :: Traversable g => Node g -> ComplexResM g (BoundTy g ResIndex, Set ResIndex)
resolveGenNodeM = fmap (first embedBoundTy) . traverseWriter resolveGenVarM

reindexTy :: Functor g => (i -> j) -> BoundTy g i -> BoundTy g j
reindexTy f = go where
  go (BoundTy bt) = BoundTy $ case bt of
    BoundTyVarF i -> BoundTyVarF (f i)
    BoundTyEmbedF gbt -> BoundTyEmbedF (fmap go gbt)

abstractTyM :: Traversable g => BoundTy g ResIndex -> Set ResIndex -> ComplexResM g (GenQuant g)
abstractTyM bt is =
  let failIndex i = error ("Internal error: missing index " ++ show i)
  in if Set.null is
    then pure $! QuantBare (reindexTy failIndex bt)
    else do
      let renaming = ILM.fromList (zip (Set.toAscList is) (fmap Index [0 ..]))
          finalBt = reindexTy (\i -> fromMaybe (failIndex i) (ILM.lookup i renaming)) bt
      vars <- gets crsVars
      let finalVars = Seq.foldlWithIndex (\acc j mtyv -> if ILM.member (ResIndex j) renaming then acc :|> mtyv else acc) Empty vars
      pure $! QuantForAll (ForAll finalVars finalBt)

resolveTmM :: (Bitraversable h, Traversable g) => SpecInit h g -> ComplexResM g (SpecTm h g (BoundTy g ResIndex) ResIndex, Set ResIndex)
resolveTmM = error "TODO" -- traverseWriter resolveGenVarM

reindexTm :: (Bifunctor h, Functor g) => (i -> j) -> SpecTm h g (BoundTy g i) i -> SpecTm h g (BoundTy g j) j
reindexTm f = goTm where
  goTm (SpecTm st) = SpecTm $ case st of
     SpecTmEmbedF gh -> SpecTmEmbedF (bimap goTy goTm gh)
     SpecTmSpecF ann bts h -> SpecTmSpecF ann (fmap f bts) (goTm h)
  goTy = reindexTy f

abstractTmM :: (Bitraversable h, Traversable g) => SpecTm h g (BoundTy g ResIndex) ResIndex -> Set ResIndex -> ComplexResM g (SpecFinal h g)
abstractTmM h is = error "TODO"
  -- let failIndex i = error ("Internal error: missing index " ++ show i)
  -- in if Set.null is
  --   then pure $! QuantBare (reindexTm failIndex h)
  --   else do
  --     let renaming = ILM.fromList (zip (Set.toAscList is) (fmap Index [0 ..]))
  --         finalH = reindexTm (\i -> fromMaybe (failIndex i) (ILM.lookup i renaming)) h
  --     vars <- gets crsVars
  --     let finalVars = Seq.foldlWithIndex (\acc j mtyv -> if ILM.member (ResIndex j) renaming then acc :|> mtyv else acc) Empty vars
  --     pure $! QuantForAll (ForAll finalVars finalH)

resolveGenVar :: Traversable g => UniqueId -> Graph g -> Either ComplexResErr (GenQuant g)
resolveGenVar = runComplexResM . (resolveGenVarM >=> uncurry abstractTyM)

resolveTm :: (Bitraversable h, Traversable g) => SpecInit h g -> Graph g -> Either ComplexResErr (SpecFinal h g)
resolveTm = runComplexResM . (resolveTmM >=> uncurry abstractTmM)

weakenElem :: Elem g -> PreElem g
weakenElem = \case
  ElemNode g -> PreElemNode g
  ElemMeta mtyv -> PreElemMeta mtyv
  ElemSkolem tyv -> PreElemSkolem tyv

weaken :: Graph g -> PreGraph g
weaken = UP.fromList . fmap (second weakenElem) . toList
