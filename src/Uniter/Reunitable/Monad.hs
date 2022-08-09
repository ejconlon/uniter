{-# LANGUAGE UndecidableInstances #-}

module Uniter.Reunitable.Monad
  ( ReuniterState (..)
  , newReuniterState
  , ReuniterM
  , runReuniterM
  , constrainEq
  , addBaseTy
  , addGenTy
  , freshVar
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
import Data.Typeable (Typeable)
import Uniter.Core (BoundId, Event (..))
import Uniter.PreGraph (PreElem (..), PreGraph (..))
import qualified Uniter.PreGraph as UP
import Uniter.Reunitable.Core (ForAll (..), GenTy (..), GenTyF (..), Index, Pair (..), Quant (..), SpecTm, TmVar, TyVar,
                               Var (..), bindSpecTm)
import Uniter.Reunitable.OrderedMap (OrderedMap)
import qualified Uniter.Reunitable.OrderedMap as OM

data ReuniterEnv g = ReuniterEnv
  { reTmFree :: Map TmVar (Quant g)
  -- ^ Map of tm var to type definition (static)
  , reBound :: OrderedMap Var BoundId
  -- ^ Map of var to type metavars (scoped)
  }

deriving instance Eq (g (GenTy g)) => Eq (ReuniterEnv g)
deriving instance Show (g (GenTy g)) => Show (ReuniterEnv g)

data ReuniterState g = ReuniterState
  { rsIdSrc :: !BoundId
  -- ^ Next unused type metavar
  , rsEvents :: !(Seq (Event g))
  -- ^ Emitted events (snocced in order)
  }

deriving instance Eq (g BoundId) => Eq (ReuniterState g)
deriving instance Show (g BoundId) => Show (ReuniterState g)

newReuniterState :: BoundId -> ReuniterState f
newReuniterState uniq = ReuniterState uniq Empty

data ReuniterErr e =
    ReuniterErrMissingVar !Var
  | ReuniterErrEmbed !e
  deriving stock (Eq, Show)

instance (Show e, Typeable e) => Exception (ReuniterErr e)

-- | A monad supporting the necessary effects to start unification
newtype ReuniterM e g a = ReuniterM { unReuniterM :: ReaderT (ReuniterEnv g) (ExceptT (ReuniterErr e) (State (ReuniterState g))) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (ReuniterEnv g), MonadError (ReuniterErr e), MonadState (ReuniterState g))

runReuniterM :: ReuniterM e g a -> ReuniterEnv g -> ReuniterState g -> (Either (ReuniterErr e) a, ReuniterState g)
runReuniterM m r = runState (runExceptT (runReaderT (unReuniterM m) r))

allocId ::  ReuniterM e g BoundId
allocId = ReuniterM $ state $ \(ReuniterState x y) -> (x, ReuniterState (succ x) y)

addEvent :: Event g -> ReuniterM e g ()
addEvent ev = ReuniterM $ modify' $ \(ReuniterState x y) -> ReuniterState x (y :|> ev)

withEvent :: (BoundId -> Event g) -> ReuniterM e g BoundId
withEvent f = do
  k <- allocId
  addEvent (f k)
  pure k

constrainEq :: BoundId -> BoundId -> ReuniterM e g BoundId
constrainEq i j = withEvent (EventConstrainEq i j)

addBaseTy :: g BoundId -> ReuniterM e g BoundId
addBaseTy gb = withEvent (EventAddNode gb)

addGenTy :: Traversable g => GenTy g -> ReuniterM e g BoundId
addGenTy (GenTy gt) = case gt of
  GenTyVarF tyv -> resolveTyVar tyv
  GenTyEmbedF gg -> traverse addGenTy gg >>= addBaseTy

freshVar :: ReuniterM e g BoundId
freshVar = withEvent EventFreshVar

bindTmVar :: TmVar -> BoundId -> ReuniterM e g a -> ReuniterM e g a
bindTmVar = undefined

resolveBoundMaybe :: Var -> ReuniterM e g (Maybe (Index, BoundId))
resolveBoundMaybe v = asks (OM.lookup v . reBound)

bindAllTyVarsFun :: Seq (Pair TyVar BoundId) -> ReuniterEnv u -> ReuniterEnv u
bindAllTyVarsFun ps re = re { reBound = OM.snocAll (reBound re) (fmap (first VarTy) ps) }

bindAllTyVars :: Seq (Pair TyVar BoundId) -> ReuniterM e g a -> ReuniterM e g a
bindAllTyVars ps = local (bindAllTyVarsFun ps)

resolveTyVar :: TyVar -> ReuniterM e g BoundId
resolveTyVar tyv = do
  let v = VarTy tyv
  mx <- resolveBoundMaybe v
  case mx of
    Just (_, b) -> pure b
    Nothing -> throwError (ReuniterErrMissingVar v)

resolveTmVarRaw :: (Traversable g)
  => TmVar -> (Maybe Index -> BoundId -> ReuniterM e g (BoundId, SpecTm h BoundId))
  -> ReuniterM e g (BoundId, SpecTm h BoundId)
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
              addGenTy body >>= f Nothing
            -- New type binders
            QuantForAll (ForAll tyvs body) -> do
              -- Allocate metavars for the instantiations of all type variables
              ps <- traverse (\tyv -> fmap (Pair tyv) freshVar) tyvs
              -- Bind metavars and apply function in the environment
              (bodyId, bodySpec) <- bindAllTyVars ps (addGenTy body >>= f Nothing)
              -- Create spec term
              let spec = bindSpecTm ps bodySpec
              pure (bodyId, spec)

resolveTmVar :: (Traversable g)
  => TmVar -> SpecTm h BoundId -> (Index -> SpecTm h BoundId)
  -> ReuniterM e g (BoundId, SpecTm h BoundId)
resolveTmVar tmv onFree onBound = resolveTmVarRaw tmv $ \mi b ->
  pure (b, maybe onFree onBound mi)

recordEvent :: Event g -> PreGraph g -> PreGraph g
recordEvent = \case
  EventAddNode n k -> UP.insert k (PreElemNode n)
  EventConstrainEq i j k -> UP.insert k (PreElemEq i j)
  EventFreshVar k -> UP.insert k PreElemFresh

-- | Generate a 'PreGraph' (a 'Graph' with equality nodes) from the current set of events.
preGraph :: ReuniterM e g (PreGraph g)
preGraph = fmap (foldr recordEvent UP.empty) (gets rsEvents)
