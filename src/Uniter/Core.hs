{-# LANGUAGE UndecidableInstances #-}

-- | Core definitions for the 'Unitable' typeclass and interpretations of the 'UniterM' effect.
module Uniter.Core
  ( BoundId (..)
  , Node
  , Event (..)
  , UniterState (..)
  , newUniterState
  , UniterT
  , runUniterT
  , constrainEq
  , constrainAllEq
  , addNode
  , freshVar
  , Unitable (..)
  , uniteTerm
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), mapStateT, modify')
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.State.Strict (liftCatch)
import Data.Foldable (toList)
import Data.Functor.Foldable (Base, Recursive (cata))
import Data.Hashable (Hashable)
import Data.Sequence (Seq (..))

-- | An opaque vertex ID in our graph representation
newtype BoundId = BoundId { unBoundId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, Enum, NFData)

-- | A 'Node' is a structure with all the holes filled with 'BoundId's.
type Node g = g BoundId

data Event g =
    EventAddNode !(Node g) !BoundId
  | EventConstrainEq !BoundId !BoundId !BoundId
  | EventFreshVar !BoundId

deriving instance Eq (Node g) => Eq (Event g)
deriving instance Ord (Node g) => Ord (Event g)
deriving instance Show (Node g) => Show (Event g)

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

-- | Describes the unification process for a particular expression functor 'f':
-- 'g' is the alignable functor (typically representing the TYPE of your expression 'f') -
--    this will almost always implement 'Alignable'
-- 'f' is the expression type (typically representing EXPRESSIONS in your language)
-- 'm' is the effect type (typically some reader/state/error)
class (Traversable f, Traversable g, Monad m) => Unitable g f m | f -> g m where
  -- | Inspects the expression functor, performing effects to
  -- allocate fresh unification vars, introduce equalities, and add nodes to the graph,
  -- returning the ID associated with this value.
  unite :: f (UniterT g m BoundId) -> UniterT g m BoundId

-- | Perform unification bottom-up on a 'Recursive' term.
uniteTerm :: (Recursive t, Base t ~ f, Unitable g f m) => t -> UniterT g m BoundId
uniteTerm = cata unite
