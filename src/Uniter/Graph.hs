{-# LANGUAGE UndecidableInstances #-}

module Uniter.Graph
  ( Join (..)
  , BoundEnv (..)
  , emptyBoundEnv
  , resolveVar
  , resolveNode
  , GraphM
  , graphResolveVar
  , graphResolveNode
  , graphInsertTerm
  , graphInsertNode
  , AppM (..)
  , runAppM
  , appUniter
  , appGraph
  ) where

import Control.Monad ((>=>))
import Control.Monad.Except (Except, MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT)
import Control.Monad.State.Strict (MonadState (..), State, StateT, gets, modify', runState)
import Data.Functor (($>))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Uniter.Core (BoundId, EventF (..), EventStream, FreeEnv, Node (..), UniterError, UniterM, nextStreamEvent,
                    streamUniter)

data Join f =
    JoinRoot !(Node f)
  | JoinLeaf !BoundId
  | JoinEq !BoundId !BoundId
  | JoinFresh
deriving stock instance Eq (f BoundId) => Eq (Join f)
deriving stock instance Show (f BoundId) => Show (Join f)

newtype BoundEnv f = BoundEnv { unBoundEnv :: Map BoundId (Join f) }
deriving newtype instance Eq (f BoundId) => Eq (BoundEnv f)
deriving stock instance Show (f BoundId) => Show (BoundEnv f)

emptyBoundEnv :: BoundEnv f
emptyBoundEnv = BoundEnv Map.empty

resolveVar :: (Corecursive t, Base t ~ f, Traversable f) => BoundId -> BoundEnv f -> Either BoundId t
resolveVar v b@(BoundEnv m) =
  case Map.lookup v m of
    Nothing -> Left v
    Just j ->
      case j of
        JoinRoot x -> resolveNode x b
        JoinLeaf x -> resolveVar x b
        _ -> Left v

resolveNode :: (Corecursive t, Base t ~ f, Traversable f) => Node f -> BoundEnv f -> Either BoundId t
resolveNode n b = fmap embed (traverse (`resolveVar` b) (unNode n))

data GraphState f = GraphState
  { gsUnique :: !BoundId
  , gsBoundEnv :: !(BoundEnv f)
  }

type GraphM f = State (GraphState f)

graphResolveVar :: (Corecursive t, Base t ~ f, Traversable f) => BoundId -> GraphM f (Either BoundId t)
graphResolveVar v = fmap (resolveVar v) (gets gsBoundEnv)

graphResolveNode :: (Corecursive t, Base t ~ f, Traversable f) => Node f -> GraphM f (Either BoundId t)
graphResolveNode n = fmap (resolveNode n) (gets gsBoundEnv)

graphInsertTerm :: (Recursive t, Base t ~ f, Traversable f) => t -> GraphM f BoundId
graphInsertTerm = cata (sequence >=> graphInsertNode . Node)

graphInsertNode :: Node f -> GraphM f BoundId
graphInsertNode n = state $ \(GraphState uniq (BoundEnv m)) ->
  let m' = Map.insert uniq (JoinRoot n) m
  in (uniq, GraphState (succ uniq) (BoundEnv m'))

newtype AppM e f a = AppM { unAppM :: ReaderT FreeEnv (StateT (GraphState f) (Except (UniterError e))) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader FreeEnv, MonadState (GraphState f), MonadError (UniterError e))

runAppM :: AppM e f a -> FreeEnv -> GraphState f -> Either (UniterError e) (a, GraphState f)
runAppM = undefined

streamUniterA :: UniterM e f a -> AppM e f (EventStream e f (a, BoundId))
streamUniterA u = do
  env <- ask
  uniq <- gets gsUnique
  pure (streamUniter u env uniq)

modifyBoundEnvA :: (Map BoundId (Join f) -> Map BoundId (Join f)) -> AppM e f ()
modifyBoundEnvA f = modify' (\st -> st { gsBoundEnv = BoundEnv (f (unBoundEnv (gsBoundEnv st))) })

addNodeA :: Node f -> BoundId -> AppM e f ()
addNodeA n i = modifyBoundEnvA (Map.insert i (JoinRoot n))

emitEqA :: BoundId -> BoundId -> BoundId -> AppM e f ()
emitEqA i j y = modifyBoundEnvA (Map.insert y (JoinEq i j))

freshA :: BoundId -> AppM e f ()
freshA i = modifyBoundEnvA (Map.insert i JoinFresh)

appUniter :: UniterM e f a -> AppM e f a
appUniter = streamUniterA >=> go where
  go es =
    case nextStreamEvent es of
      Left (r, uniq) -> modify' (\st -> st { gsUnique = uniq }) $> r
      Right ef ->
        case ef of
          EventError e -> throwError e
          EventAddNode n i tl -> addNodeA n i *> go tl
          EventEmitEq i j y tl -> emitEqA i j y *> go tl
          EventFresh i tl -> freshA i *> go tl

appGraph :: GraphM f a -> AppM e f a
appGraph = state . runState
