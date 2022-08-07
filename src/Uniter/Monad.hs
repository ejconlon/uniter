{-# LANGUAGE UndecidableInstances #-}

module Uniter.Monad
  ( UniterState (..)
  , newUniterState
  , UniterT
  , runUniterT
  , constrainEq
  , constrainAllEq
  , addNode
  , freshVar
  , preGraph
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, mapStateT, modify')
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.State.Strict (liftCatch)
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import Uniter.Core (BoundId, Event (..), Node)
import Uniter.PreGraph (PreElem (..), PreGraph (..))
import qualified Uniter.PreGraph as UP

data UniterState g = UniterState
  { usIdSrc :: !BoundId
  , usEvents :: !(Seq (Event g))
  }

deriving instance Eq (Node g) => Eq (UniterState g)
deriving instance Show (Node g) => Show (UniterState g)

newUniterState :: BoundId -> UniterState f
newUniterState uniq = UniterState uniq Empty

-- | A monad transformer supporting the necessary effects to start unification
newtype UniterT g m a = UniterT { unUniterT :: StateT (UniterState g) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (UniterT g) where
  lift = UniterT . lift

instance MonadReader r m => MonadReader r (UniterT g m) where
  ask = lift ask
  reader = lift . reader
  local f m = UniterT (mapStateT (local f) (unUniterT m))

instance MonadState s m => MonadState s (UniterT g m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadError e m => MonadError e (UniterT g m) where
  throwError = lift . throwError
  catchError m f = UniterT (liftCatch catchError (unUniterT m) (unUniterT . f))

runUniterT :: UniterT g m a -> UniterState g -> m (a, UniterState g)
runUniterT = runStateT . unUniterT

allocId :: Monad m => UniterT g m BoundId
allocId = UniterT $ state $ \(UniterState x y) -> (x, UniterState (succ x) y)

addEvent :: Monad m => Event g -> UniterT g m ()
addEvent ev = UniterT $ modify' $ \(UniterState x y) -> UniterState x (y :|> ev)

withEvent :: Monad m => (BoundId -> Event g) -> UniterT g m BoundId
withEvent f = do
  k <- allocId
  addEvent (f k)
  pure k

-- | Emit equality constraints on two IDs.
constrainEq :: Monad m => BoundId -> BoundId -> UniterT g m BoundId
constrainEq i j = withEvent (EventConstrainEq i j)

-- | Emit equality constraints on all IDs.
constrainAllEq :: (Monad m, Foldable t) => BoundId -> t BoundId -> UniterT g m BoundId
constrainAllEq i0 js0 = go i0 (toList js0) where
  go !i = \case
    [] -> pure i
    j:js -> do
      k <- constrainEq i j
      go k js

-- | Allocate an ID for the given 'Node'.
addNode :: Monad m => Node g -> UniterT g m BoundId
addNode n = withEvent (EventAddNode n)

-- | Allocate a fresh ID.
freshVar :: Monad m => UniterT g m BoundId
freshVar = withEvent EventFreshVar

recordEvent :: Event g -> PreGraph g -> PreGraph g
recordEvent = \case
  EventAddNode n k -> UP.insert k (PreElemNode n)
  EventConstrainEq i j k -> UP.insert k (PreElemEq i j)
  EventFreshVar k -> UP.insert k PreElemFresh

-- | Generate a 'PreGraph' (a 'Graph' with equality nodes) from the current set of events.
preGraph :: Monad m => UniterT g m (PreGraph g)
preGraph = fmap (foldr recordEvent UP.empty) (UniterT (gets usEvents))
