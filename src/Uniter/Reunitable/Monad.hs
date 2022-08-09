{-# LANGUAGE UndecidableInstances #-}

module Uniter.Reunitable.Monad
  ( ReuniterState (..)
  , newReuniterState
  , ReuniterM
  , runReuniterM
  , constrainEq
  , constrainAllEq
  , addNode
  , freshVar
  , preGraph
  ) where

import Control.Monad.Except (MonadError (..), ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), gets, modify', State, runState)
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import Uniter.Core (BoundId, Event (..), Node)
import Uniter.PreGraph (PreElem (..), PreGraph (..))
import qualified Uniter.PreGraph as UP
import Data.Map.Strict (Map)
import Data.String (IsString)
import Data.Text (Text)
import Control.Exception (Exception)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)

newtype TyVar = TyVar { unTyVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype TmVar = TmVar { unTmVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data ForAll a = ForAll
  { forAllBinders :: !(Seq TyVar)
  , forAllBody :: !a
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data ReuniterEnv u = ReuniterEnv
  { reTmFree :: Map TmVar (ForAll u)
  -- ^ Map of tm var to type definition (static)
  , reTmBound :: Map TmVar BoundId
  -- ^ Map of tm var to type id (scoped)
  , reTyBound :: Map TyVar BoundId
  -- ^ Map of ty var to type id (scoped)
  } deriving stock (Eq, Show)

data ReuniterState g = ReuniterState
  { rsIdSrc :: !BoundId
  -- ^ Next unused id
  , rsEvents :: !(Seq (Event g))
  -- ^ Emitted events (snocced in order)
  }

deriving instance Eq (Node g) => Eq (ReuniterState g)
deriving instance Show (Node g) => Show (ReuniterState g)

newReuniterState :: BoundId -> ReuniterState f
newReuniterState uniq = ReuniterState uniq Empty

data ReuniterErr e =
    ReuniterErrMissingTmVar !TmVar
  | ReuniterErrMissingTyVar !TyVar
  | ReuniterErrEmbed !e
  deriving stock (Eq, Show)

instance (Show e, Typeable e) => Exception (ReuniterErr e)

-- | A monad supporting the necessary effects to start unification
newtype ReuniterM e u g a = ReuniterM { unReuniterM :: ReaderT (ReuniterEnv u) (ExceptT (ReuniterErr e) (State (ReuniterState g))) a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadError e (ReuniterM e u g) where
  throwError = ReuniterM . throwError . ReuniterErrEmbed
  catchError _m _f = error "TODO"

runReuniterM :: ReuniterM e u g a -> ReuniterEnv u -> ReuniterState g -> (Either (ReuniterErr e) a, ReuniterState g)
runReuniterM m r = runState (runExceptT (runReaderT (unReuniterM m) r))

allocId ::  ReuniterM e u g BoundId
allocId = ReuniterM $ state $ \(ReuniterState x y) -> (x, ReuniterState (succ x) y)

addEvent :: Event g -> ReuniterM e u g ()
addEvent ev = ReuniterM $ modify' $ \(ReuniterState x y) -> ReuniterState x (y :|> ev)

withEvent :: (BoundId -> Event g) -> ReuniterM e u g BoundId
withEvent f = do
  k <- allocId
  addEvent (f k)
  pure k

-- | Emit equality constraints on two IDs.
constrainEq :: BoundId -> BoundId -> ReuniterM e u g BoundId
constrainEq i j = withEvent (EventConstrainEq i j)

-- | Emit equality constraints on all IDs.
constrainAllEq :: Foldable t => BoundId -> t BoundId -> ReuniterM e u g BoundId
constrainAllEq i0 js0 = go i0 (toList js0) where
  go !i = \case
    [] -> pure i
    j:js -> do
      k <- constrainEq i j
      go k js

-- | Allocate an ID for the given 'Node'.
addNode :: Node g -> ReuniterM e u g BoundId
addNode n = withEvent (EventAddNode n)

-- | Allocate a fresh ID.
freshVar :: ReuniterM e u g BoundId
freshVar = withEvent EventFreshVar

resolveTmBound :: TmVar -> ReuniterM e u g BoundId
resolveTmBound v = do
  m <- ReuniterM $ asks reTmBound
  case Map.lookup v m of
    Nothing -> ReuniterM $ throwError (ReuniterErrMissingTmVar v)
    Just b -> pure b

resolveTyBound :: TyVar -> ReuniterM e u g BoundId
resolveTyBound v = do
  m <- ReuniterM $ asks reTyBound
  case Map.lookup v m of
    Nothing -> ReuniterM $ throwError (ReuniterErrMissingTyVar v)
    Just b -> pure b

resolveTm :: TmVar -> (BoundId -> ReuniterM e u g a) -> ReuniterM e u g a
resolveTm = undefined

recordEvent :: Event g -> PreGraph g -> PreGraph g
recordEvent = \case
  EventAddNode n k -> UP.insert k (PreElemNode n)
  EventConstrainEq i j k -> UP.insert k (PreElemEq i j)
  EventFreshVar k -> UP.insert k PreElemFresh

-- | Generate a 'PreGraph' (a 'Graph' with equality nodes) from the current set of events.
preGraph :: ReuniterM e u g (PreGraph g)
preGraph = fmap (foldr recordEvent UP.empty) (ReuniterM (gets rsEvents))
