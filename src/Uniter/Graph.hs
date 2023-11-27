{-# LANGUAGE UndecidableInstances #-}

-- | (Import this qualified)
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
  )
where

import Control.Exception (Exception)
import Control.Monad ((>=>))
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT, gets, modify')
import Control.Monad.Trans.Writer.Strict (WriterT (..))
import Data.Bifunctor (Bifunctor (..), second)
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Corecursive (..))
import Data.Kind (Type)
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
import Uniter.Core
  ( BoundTy (..)
  , BoundTyF (..)
  , ForAll (..)
  , Index (..)
  , Node
  , PolyTy
  , SpecFinal
  , SpecInit
  , SpecTm (..)
  , TyBinder (..)
  , UniqueId (..)
  , embedBoundTy
  )
import Uniter.PreGraph (PreElem (..), PreGraph)
import qualified Uniter.PreGraph as UP
import Prelude hiding (lookup)

data Elem (g :: Type -> Type)
  = ElemNode !(Node g)
  | ElemMeta !TyBinder
  | ElemSkolem !TyBinder

deriving stock instance (Eq (Node g)) => Eq (Elem g)

deriving stock instance (Ord (Node g)) => Ord (Elem g)

deriving stock instance (Show (Node g)) => Show (Elem g)

-- | A traversal over 'Elem's - can get or replace all 'UniqueId's
elemTraversal :: (Traversable g) => Traversal' (Elem g) UniqueId
elemTraversal f = \case
  ElemNode fb -> fmap ElemNode (traverse f fb)
  e -> pure e

newtype Graph (g :: Type -> Type) = Graph {unGraph :: IntLikeMap UniqueId (Elem g)}

deriving newtype instance (Eq (Node g)) => Eq (Graph g)

deriving stock instance (Show (Node g)) => Show (Graph g)

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

-- What follows are methods to pull terms and types out of the graph.
-- Here this is called "resolution." There are two variants:
-- 1. "simple" - just extracting (mono-)types
-- 2. "complex" - extracting quantified/specialized/reconstructed terms and their polytypes

-- When resolving, the graph is constant, and we maintain a scoped path
-- to detect cycles.
data ResEnv g = ResEnv
  { reGraph :: !(Graph g)
  , rePath :: !(IntLikeSet UniqueId)
  }

-- | Resolution can go wrong in many ways...
data SimpleResErr
  = SimpleResErrLoop !UniqueId
  | SimpleResErrNotFound !UniqueId
  | SimpleResErrUnsolvedMeta !UniqueId !TyBinder
  | SimpleResErrUnsupportedSkolem !UniqueId !TyBinder
  deriving stock (Eq, Show)

instance Exception SimpleResErr

newtype SimpleResM g u a = SimpleResM {unSimpleResM :: ReaderT (ResEnv g) (StateT (IntLikeMap UniqueId u) (Except SimpleResErr)) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (ResEnv g), MonadState (IntLikeMap UniqueId u), MonadError SimpleResErr)

runSimpleResM :: SimpleResM g u a -> Graph g -> Either SimpleResErr a
runSimpleResM m gr = runExcept (evalStateT (runReaderT (unSimpleResM m) (ResEnv gr ILS.empty)) ILM.empty)

-- Resolve a type by metavar id.
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
              ElemNode x -> local (\re -> re {rePath = ILS.insert v (rePath re)}) (resolveNodeM x)
              ElemMeta tyb -> throwError (SimpleResErrUnsolvedMeta v tyb)
              ElemSkolem tyb -> throwError (SimpleResErrUnsupportedSkolem v tyb)
            modify' (ILM.insert v w)
            pure w

-- Resolve a type by node.
resolveNodeM :: (Corecursive u, Base u ~ g, Traversable g) => Node g -> SimpleResM g u u
resolveNodeM n = fmap embed (traverse resolveVarM n)

-- | Resolve a type by metavar id.
resolveVar :: (Corecursive u, Base u ~ g, Traversable g) => UniqueId -> Graph g -> Either SimpleResErr u
resolveVar = runSimpleResM . resolveVarM

-- | Resolution can go wrong in many ways...
data ComplexResErr
  = ComplexResErrLoop !UniqueId
  | ComplexResErrNotFound !UniqueId
  deriving stock (Eq, Show)

instance Exception ComplexResErr

-- de Bruijn index into the vars sequence
newtype ResIndex = ResIndex {unResIndex :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

data ComplexResSt g = ComplexResSt
  { crsCache :: !(IntLikeMap UniqueId (BoundTy g ResIndex, Set ResIndex))
  , crsVars :: !(Seq TyBinder)
  }

newtype ComplexResM g a = ComplexResM {unComplexResM :: ReaderT (ResEnv g) (StateT (ComplexResSt g) (Except ComplexResErr)) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (ResEnv g), MonadState (ComplexResSt g), MonadError ComplexResErr)

runComplexResM :: ComplexResM g a -> Graph g -> Either ComplexResErr a
runComplexResM m gr = fmap fst (runExcept (runStateT (runReaderT (unComplexResM m) (ResEnv gr ILS.empty)) (ComplexResSt ILM.empty Empty)))

newVarM :: TyBinder -> ComplexResM g (BoundTy g ResIndex, Set ResIndex)
newVarM tyb = do
  ix <- state $ \st ->
    let ix = ResIndex (Seq.length (crsVars st))
        st' = st {crsVars = crsVars st :|> tyb}
    in  (ix, st')
  pure (BoundTy (BoundTyVarF ix), Set.singleton ix)

resolveGenVarM :: (Traversable g) => UniqueId -> ComplexResM g (BoundTy g ResIndex, Set ResIndex)
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
              ElemNode x -> local (\re -> re {rePath = ILS.insert v (rePath re)}) (resolveGenNodeM x)
              ElemMeta tyb -> newVarM tyb
              ElemSkolem tyb -> newVarM tyb
            modify' (\st -> st {crsCache = ILM.insert v p (crsCache st)})
            pure p

bitraverseWriter :: (Monoid w, Applicative m, Bitraversable h) => (x -> m (y, w)) -> (a -> m (b, w)) -> h x a -> m (h y b, w)
bitraverseWriter f g hxa = runWriterT (bitraverse (WriterT . f) (WriterT . g) hxa)

traverseWriter :: (Monoid w, Applicative m, Traversable g) => (a -> m (b, w)) -> g a -> m (g b, w)
traverseWriter f ga = runWriterT (traverse (WriterT . f) ga)

resolveGenNodeM :: (Traversable g) => Node g -> ComplexResM g (BoundTy g ResIndex, Set ResIndex)
resolveGenNodeM = fmap (first embedBoundTy) . traverseWriter resolveGenVarM

reindexTy :: (Functor g) => (i -> j) -> BoundTy g i -> BoundTy g j
reindexTy f = go
 where
  go (BoundTy bt) = BoundTy $ case bt of
    BoundTyVarF i -> BoundTyVarF (f i)
    BoundTyEmbedF gbt -> BoundTyEmbedF (fmap go gbt)

failIndex :: (Show i) => i -> a
failIndex i = error ("Internal error: missing index " ++ show i)

abstractTyM :: (Traversable g) => BoundTy g ResIndex -> Set ResIndex -> ComplexResM g (PolyTy g)
abstractTyM bt is =
  if Set.null is
    then pure (ForAll Seq.empty (reindexTy failIndex bt))
    else do
      let renaming = ILM.fromList (zip (Set.toAscList is) (fmap Index [0 ..]))
          finalBt = reindexTy (\i -> fromMaybe (failIndex i) (ILM.lookup i renaming)) bt
      vars <- gets crsVars
      let finalVars = Seq.foldlWithIndex (\acc j tyb -> if ILM.member (ResIndex j) renaming then acc :|> tyb else acc) Empty vars
      pure (ForAll finalVars finalBt)

sealTy :: (Traversable g) => BoundTy g ResIndex -> Set ResIndex -> BoundTy g Index
sealTy bt is =
  let renaming = ILM.fromList (zip (Set.toAscList is) (fmap Index [0 ..]))
  in  reindexTy (\i -> fromMaybe (failIndex i) (ILM.lookup i renaming)) bt

resolveTmM :: (Bitraversable h, Traversable g) => SpecInit h g -> ComplexResM g (SpecTm h g (BoundTy g ResIndex) (BoundTy g ResIndex), Set ResIndex)
resolveTmM = bitraverseWriter resolveGenVarM resolveGenVarM

abstractTmM :: (Bitraversable h, Traversable g) => SpecTm h g (BoundTy g ResIndex) (BoundTy g ResIndex) -> Set ResIndex -> ComplexResM g (SpecFinal h g)
abstractTmM h is = do
  h' <- bitraverse (`abstractTyM` is) (pure . flip sealTy is) h
  if Set.null is
    then pure (ForAll Seq.empty h')
    else do
      vars <- gets crsVars
      let finalVars = Seq.foldlWithIndex (\acc j mtyv -> if Set.member (ResIndex j) is then acc :|> mtyv else acc) Empty vars
      pure (ForAll finalVars h')

-- | For debugging, find the polytype associated with the given id.
resolveGenVar :: (Traversable g) => UniqueId -> Graph g -> Either ComplexResErr (PolyTy g)
resolveGenVar = runComplexResM . (resolveGenVarM >=> uncurry abstractTyM)

-- | Resolve the partially-reconstructed term.
resolveTm :: (Bitraversable h, Traversable g) => SpecInit h g -> Graph g -> Either ComplexResErr (SpecFinal h g)
resolveTm = runComplexResM . (resolveTmM >=> uncurry abstractTmM)

-- | For debugging, turn an Elem into a PreElem
weakenElem :: Elem g -> PreElem g
weakenElem = \case
  ElemNode g -> PreElemNode g
  ElemMeta tyb -> PreElemMeta tyb
  ElemSkolem tyb -> PreElemSkolem tyb

-- | For debugging, turn a Graph into a PreGraph
weaken :: Graph g -> PreGraph g
weaken = UP.fromList . fmap (second weakenElem) . toList
