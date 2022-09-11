{-# LANGUAGE UndecidableInstances #-}

module Uniter.Reunitable.Monad
  ( ReuniterEnv (..)
  , newReuniterEnv
  , ReuniterState (..)
  , newReuniterState
  , ReuniterErr (..)
  , ReuniterM
  , runReuniterM
  , constrainEq
  , addBaseTy
  , addSrcQuant
  , freshMetaVar
  , freshSkolemVar
  , bindTmVar
  , resolveTmVar
  , preGraph
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), State, gets, modify', runState)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Uniter.Core (BoundTy (..), BoundTyF (..), Event (..), ForAll (..), Index (..), Node, Pair (..), Quant (..),
                    SpecTm, SrcQuant, TmVar, TyVar, UniqueId (..), Var (..), bindSpecTm)
import Uniter.OrderedMap (OrderedMap)
import qualified Uniter.OrderedMap as OM
import Uniter.PreGraph (PreElem (..), PreGraph (..))
import qualified Uniter.PreGraph as UP

data ReuniterEnv g = ReuniterEnv
  { reTmFree :: !(Map TmVar (SrcQuant g))
  -- ^ Map of tm var to type definition (static)
  , reBound :: !(OrderedMap Var UniqueId)
  -- ^ Map of var to type metavars (scoped)
  }

deriving instance Eq (g (BoundTy Index g)) => Eq (ReuniterEnv g)
deriving instance Show (g (BoundTy Index g)) => Show (ReuniterEnv g)

newReuniterEnv :: Map TmVar (SrcQuant g) -> ReuniterEnv g
newReuniterEnv fm = ReuniterEnv fm OM.empty

data ReuniterState g = ReuniterState
  { rsUnique :: !UniqueId
  -- ^ Next unused type metavar
  , rsEvents :: !(Seq (Event g))
  -- ^ Emitted events (snocced in order)
  }

deriving instance Eq (Node g) => Eq (ReuniterState g)
deriving instance Show (Node g) => Show (ReuniterState g)

newReuniterState :: UniqueId -> ReuniterState f
newReuniterState uniq = ReuniterState uniq Empty

data ReuniterErr =
    ReuniterErrMissingVar !Var
  | ReuniterErrMissingTyVar !Index
  deriving stock (Eq, Ord, Show)

instance Exception ReuniterErr

-- | A monad supporting the necessary effects to start unification
newtype ReuniterM g a = ReuniterM { unReuniterM :: ReaderT (ReuniterEnv g) (ExceptT ReuniterErr (State (ReuniterState g))) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (ReuniterEnv g), MonadError ReuniterErr, MonadState (ReuniterState g))

runReuniterM :: ReuniterM g a -> ReuniterEnv g -> ReuniterState g -> (Either ReuniterErr a, ReuniterState g)
runReuniterM m r = runState (runExceptT (runReaderT (unReuniterM m) r))

allocId ::  ReuniterM g UniqueId
allocId = state $ \(ReuniterState x y) -> (x, ReuniterState (succ x) y)

addEvent :: Event g -> ReuniterM g ()
addEvent ev = ReuniterM $ modify' $ \(ReuniterState x y) -> ReuniterState x (y :|> ev)

withEvent :: (UniqueId -> Event g) -> ReuniterM g UniqueId
withEvent f = do
  k <- allocId
  let ev = f k
  addEvent ev
  pure k

constrainEq :: UniqueId -> UniqueId -> ReuniterM g UniqueId
constrainEq i j = withEvent (EventConstrainEq i j)

addBaseTy :: Node g -> ReuniterM g UniqueId
addBaseTy gb = withEvent (EventAddNode gb)

addSrcQuant :: Traversable g => SrcQuant g -> ReuniterM g UniqueId
addSrcQuant = \case
  QuantBare g -> addBoundTy Empty g
  QuantForAll (ForAll tyVars g) -> do
    us <- traverse freshSkolemVar tyVars
    addBoundTy us g

addBoundTy :: Traversable g => Seq UniqueId -> BoundTy Index g -> ReuniterM g UniqueId
addBoundTy tyVars = go where
  go (BoundTy gf) = case gf of
    BoundTyVarF ix@(Index i) ->
      case Seq.lookup i tyVars of
        Nothing -> throwError (ReuniterErrMissingTyVar ix)
        Just u -> pure u
    BoundTyEmbedF g -> traverse go g >>= addBaseTy

freshMetaVar :: ReuniterM g UniqueId
freshMetaVar = withEvent EventNewMetaVar

freshSkolemVar :: TyVar -> ReuniterM g UniqueId
freshSkolemVar = withEvent . EventNewSkolemVar

bindVarFun :: Var -> UniqueId -> ReuniterEnv g -> ReuniterEnv g
bindVarFun v b re = re { reBound = OM.snoc (reBound re) v b }

bindVar :: Var -> UniqueId -> ReuniterM g a -> ReuniterM g a
bindVar v = local . bindVarFun v

bindTmVar :: TmVar -> UniqueId -> ReuniterM g a -> ReuniterM g a
bindTmVar = bindVar . VarTm

resolveBoundMaybe :: Var -> ReuniterM g (Maybe (Index, UniqueId))
resolveBoundMaybe v = asks (OM.lookup v . reBound)

bindAllTyVarsFun :: Seq (Pair TyVar UniqueId) -> ReuniterEnv u -> ReuniterEnv u
bindAllTyVarsFun ps re = re { reBound = OM.snocAll (reBound re) (fmap (first VarTy) ps) }

bindAllTyVars :: Seq (Pair TyVar UniqueId) -> ReuniterM g a -> ReuniterM g a
bindAllTyVars ps = local (bindAllTyVarsFun ps)

resolveTyVar :: TyVar -> ReuniterM g UniqueId
resolveTyVar tyv = do
  let v = VarTy tyv
  mx <- resolveBoundMaybe v
  case mx of
    Just (_, b) -> pure b
    Nothing -> throwError (ReuniterErrMissingVar v)

resolveTmVarRaw :: (Traversable g)
  => TmVar -> (Maybe Index -> UniqueId -> ReuniterM g (UniqueId, SpecTm h UniqueId))
  -> ReuniterM g (UniqueId, SpecTm h UniqueId)
resolveTmVarRaw tmv f = do
  -- First check if this term var has been bound
  mx <- resolveBoundMaybe (VarTm tmv)
  case mx of
    -- If it has been bound, just return its metavar
    Just (i, b) -> f (Just i) b
    -- Otherwise we need to check free variables
    Nothing -> do
      my <- asks (Map.lookup tmv . reTmFree)
      case my of
        -- Not there? Meaningless var to us - throw an error
        Nothing -> throwError (ReuniterErrMissingVar (VarTm tmv))
        -- If we found a definition, specialize
        Just q ->
          case q of
            -- No new type binders
            QuantBare body -> do
              u <- addBoundTy Empty body
              f Nothing u
            -- New type binders
            QuantForAll (ForAll tyvs body) -> do
              -- Allocate skolem vars for the instantiations of all type variables
              ps <- traverse (\tyv -> fmap (Pair tyv) (freshSkolemVar tyv)) tyvs
              let us = fmap pairVal ps
              u <- addBoundTy us body
              -- Bind metavars and apply function in the environment
              (bodyId, bodySpec) <- bindAllTyVars ps (f Nothing u)
              -- Create spec term
              let spec = bindSpecTm us bodySpec
              pure (bodyId, spec)

resolveTmVar :: (Traversable g)
  => TmVar -> SpecTm h UniqueId -> (Index -> SpecTm h UniqueId)
  -> ReuniterM g (UniqueId, SpecTm h UniqueId)
resolveTmVar tmv onFree onBound = resolveTmVarRaw tmv $ \mi b ->
  pure (b, maybe onFree onBound mi)

recordEvent :: Event g -> PreGraph g -> PreGraph g
recordEvent = \case
  EventAddNode n k -> UP.insert k (PreElemNode n)
  EventConstrainEq i j k -> UP.insert k (PreElemEq i j)
  EventNewMetaVar k -> UP.insert k PreElemMeta
  EventNewSkolemVar tyv k -> UP.insert k (PreElemSkolem tyv)

-- | Generate a 'PreGraph' (a 'Graph' with equality nodes) from the current set of events.
preGraph :: ReuniterM g (PreGraph g)
preGraph = fmap (foldr recordEvent UP.empty) (gets rsEvents)
