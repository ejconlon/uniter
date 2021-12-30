{-# LANGUAGE UndecidableInstances #-}

module Uniter.Core
  ( BoundId (..)
  , FreeName (..)
  , Node (..)
  , FreeEnv (..)
  , emptyFreeEnv
  , UniterError (..)
  , UniterM
  , uniterThrowError
  , uniterLookupFree
  , uniterIndexFree
  , uniterAssignFree
  , uniterEmitEq
  , uniterAddNode
  , uniterFresh
  , EventF (..)
  , EventStream
  , nextStreamEvent
  , runUniter
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), State, runState)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import Overeasy.Expressions.Free (Free, pattern FreeEmbed, pattern FreePure)
import Streaming (Stream)
import qualified Streaming as S

newtype BoundId = BoundId { unBoundId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, Enum, NFData)

newtype FreeName = FreeName { unFreeName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, NFData, IsString)

newtype Node f = Node { unNode :: f BoundId }
deriving newtype instance Eq (f BoundId) => Eq (Node f)
deriving stock instance Show (f BoundId) => Show (Node f)

newtype FreeEnv = FreeEnv { unFreeEnv :: Map FreeName BoundId }
  deriving stock (Show)
  deriving newtype (Eq)

emptyFreeEnv :: FreeEnv
emptyFreeEnv = FreeEnv Map.empty

data UniterError e =
    UniterErrorFail !String
  | UniterErrorMissingFree !FreeName
  | UniterErrorEmbed !e
  deriving stock (Eq, Show)

data UniterF e f m a =
    UniterThrowError !(UniterError e)
  | UniterLookupFree !FreeName !(Maybe BoundId -> a)
  | UniterAssignFree !FreeName !BoundId (m a)
  | UniterEmitEq !BoundId !BoundId !(BoundId -> a)
  | UniterAddNode !(Node f) !(BoundId -> a)
  | UniterFresh !(BoundId -> a)
  deriving stock (Functor)

newtype UniterM e f a = UniterM { unUniterM :: Free (UniterF e f (UniterM e f)) a }
  deriving newtype (Functor, Applicative, Monad)

uniterThrowError :: UniterError e -> UniterM e f a
uniterThrowError = UniterM . FreeEmbed . UniterThrowError

uniterLookupFree :: FreeName -> UniterM e f (Maybe BoundId)
uniterLookupFree x = UniterM (FreeEmbed (UniterLookupFree x pure))

uniterIndexFree :: FreeName -> UniterM e f BoundId
uniterIndexFree n = uniterLookupFree n >>= maybe (uniterThrowError (UniterErrorMissingFree n)) pure

uniterAssignFree :: FreeName -> BoundId -> UniterM e f a -> UniterM e f a
uniterAssignFree x i act = UniterM (FreeEmbed (UniterAssignFree x i (fmap pure act)))

uniterEmitEq :: BoundId -> BoundId -> UniterM e f BoundId
uniterEmitEq i j = UniterM (FreeEmbed (UniterEmitEq i j pure))

uniterAddNode :: Node f -> UniterM e f BoundId
uniterAddNode n = UniterM (FreeEmbed (UniterAddNode n pure))

uniterFresh :: UniterM e f BoundId
uniterFresh = UniterM (FreeEmbed (UniterFresh pure))

instance MonadFail (UniterM e f) where
  fail = uniterThrowError . UniterErrorFail

data EventF e f a =
    EventError !(UniterError e)
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

newtype E a = E { unE :: ReaderT FreeEnv (State BoundId) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader FreeEnv, MonadState BoundId)

runE :: E a -> FreeEnv -> BoundId -> (a, BoundId)
runE e = runState . runReaderT (unE e)

interpESM :: UniterM e f r -> EventStreamM e f E r
interpESM = subInterpESM . unUniterM

subInterpESM :: Free (UniterF e f (UniterM e f)) a -> EventStreamM e f E a
subInterpESM = \case
  FreePure a -> pure a
  FreeEmbed u ->
    case u of
      UniterThrowError e -> S.yields (EventError e)
      UniterLookupFree i k -> asks (Map.lookup i . unFreeEnv) >>= subInterpESM . k
      UniterAssignFree x i act -> local (\(FreeEnv m) -> FreeEnv (Map.insert x i m)) (interpESM act >>= subInterpESM)
      UniterEmitEq i j k -> state (\b -> (b, succ b)) >>= \y -> S.wrap (EventEmitEq i j y (subInterpESM (k y)))
      UniterAddNode n k -> state (\b -> (b, succ b)) >>= \i -> S.wrap (EventAddNode n i (subInterpESM (k i)))
      UniterFresh k -> state (\b -> (b, succ b)) >>= \i -> S.wrap (EventFresh i (subInterpESM (k i)))

runUniter :: UniterM e f r -> FreeEnv -> BoundId -> EventStream e f (r, BoundId)
runUniter u env bid = S.hoist (\x -> Identity (fst (runE x env bid))) (interpESM u >>= \r -> get >>= \bid' -> pure (r, bid'))
