{-# LANGUAGE UndecidableInstances #-}

module Uniter.Core
  ( BoundId (..)
  , Node (..)
  , UniterM
  , uniterEmitEq
  , uniterAddNode
  , uniterFresh
  , EventF (..)
  , EventStream
  , nextStreamEvent
  , streamUniter
  , Unitable (..)
  , uniteTerm
  , EventHandler (..)
  , handleEvents
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), State, runState)
import Data.Functor.Foldable (Base, Recursive (cata))
import Data.Hashable (Hashable)
import Overeasy.Expressions.Free (Free, pattern FreeEmbed, pattern FreePure)
import Streaming (Stream)
import qualified Streaming as S
import Uniter.Halt (MonadHalt (..))

newtype BoundId = BoundId { unBoundId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, Enum, NFData)

newtype Node f = Node { unNode :: f BoundId }
deriving newtype instance Eq (f BoundId) => Eq (Node f)
deriving stock instance Show (f BoundId) => Show (Node f)

data UniterF v e f m a =
    UniterHalt !e
  | UniterAsk !(v -> a)
  | UniterLocal !(v -> v) (m a)
  | UniterEmitEq !BoundId !BoundId !(BoundId -> a)
  | UniterAddNode !(Node f) !(BoundId -> a)
  | UniterFresh !(BoundId -> a)
  deriving stock (Functor)

newtype UniterM v e f a = UniterM { unUniterM :: Free (UniterF v e f (UniterM v e f)) a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadHalt e (UniterM v e f) where
  halt = UniterM . FreeEmbed . UniterHalt

instance MonadReader v (UniterM v e f) where
  ask = UniterM (FreeEmbed (UniterAsk pure))
  local f act = UniterM (FreeEmbed (UniterLocal f (fmap FreePure act)))

uniterEmitEq :: BoundId -> BoundId -> UniterM v e f BoundId
uniterEmitEq i j = UniterM (FreeEmbed (UniterEmitEq i j pure))

uniterAddNode :: Node f -> UniterM v e f BoundId
uniterAddNode n = UniterM (FreeEmbed (UniterAddNode n pure))

uniterFresh :: UniterM v e f BoundId
uniterFresh = UniterM (FreeEmbed (UniterFresh pure))

data EventF e f a =
    EventHalt !e
  | EventAddNode !(Node f) !BoundId a
  | EventEmitEq !BoundId !BoundId !BoundId a
  | EventFresh !BoundId a
  deriving stock (Functor, Foldable, Traversable)
deriving stock instance (Eq e, Eq (f BoundId), Eq a) => Eq (EventF e f a)
deriving stock instance (Show e, Show (f BoundId), Show a) => Show (EventF e f a)

type EventStreamM e f = Stream (EventF e f)

type EventStream e f = EventStreamM e f Identity

nextStreamEvent :: EventStream e f r -> Either r (EventF e f (EventStream e f r))
nextStreamEvent = runIdentity . S.inspect

newtype E v a = E { unE :: ReaderT v (State BoundId) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader v, MonadState BoundId)

runE :: E v a -> v -> BoundId -> (a, BoundId)
runE e = runState . runReaderT (unE e)

interpESM :: v -> UniterM v e f r -> EventStreamM e f (E v) r
interpESM v = subInterpESM v . unUniterM

subInterpESM :: v -> Free (UniterF v e f (UniterM v e f)) a -> EventStreamM e f (E v) a
subInterpESM v = \case
  FreePure a -> pure a
  FreeEmbed u ->
    case u of
      UniterHalt e -> S.yields (EventHalt e)
      UniterAsk k -> subInterpESM v (k v)
      UniterLocal f act -> interpESM (f v) act >>= subInterpESM v
      UniterEmitEq i j k -> state (\b -> (b, succ b)) >>= \y -> S.wrap (EventEmitEq i j y (subInterpESM v (k y)))
      UniterAddNode n k -> state (\b -> (b, succ b)) >>= \y -> S.wrap (EventAddNode n y (subInterpESM v (k y)))
      UniterFresh k -> state (\b -> (b, succ b)) >>= \y -> S.wrap (EventFresh y (subInterpESM v (k y)))

evalStream :: EventStreamM e f (E v) r -> v -> BoundId -> EventStream e f (r, BoundId)
evalStream s0 v i0 = go i0 s0 where
  go i s =
    let (e, j) = runE (S.inspect s) v i
    in case e of
      Left r -> pure (r, j)
      Right f -> S.wrap (fmap (go j) f)

streamUniter :: UniterM v e f r -> v -> BoundId -> EventStream e f (r, BoundId)
streamUniter u v = evalStream (interpESM v u) v

class Unitable v e g f | f -> v e g where
  unite :: f (UniterM v e g BoundId) -> UniterM v e g BoundId

uniteTerm :: (Recursive t, Base t ~ f, Unitable v e g f) => t -> UniterM v e g BoundId
uniteTerm = cata unite

class MonadHalt e m => EventHandler e f m | m -> e f where
  handleAddNode :: Node f -> BoundId -> m ()
  handleEmitEq :: BoundId -> BoundId -> BoundId -> m ()
  handleFresh :: BoundId -> m ()

handleEvents :: EventHandler e f m => EventStream e f r -> m r
handleEvents es =
  case nextStreamEvent es of
    Left r -> pure r
    Right ef ->
      case ef of
        EventHalt e -> halt e
        EventAddNode n i tl -> handleAddNode n i *> handleEvents tl
        EventEmitEq i j y tl -> handleEmitEq i j y *> handleEvents tl
        EventFresh i tl -> handleFresh i *> handleEvents tl
