{-# LANGUAGE UndecidableInstances #-}

module Uniter.Graph
  ( Elem (..)
  , BoundEnv (..)
  , emptyBoundEnv
  , resolveVar
  , resolveNode
  , handleElems
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
import Data.Foldable (traverse_)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Uniter.Core (BoundId, EventHandler (..), EventStream, Node (..), UniterM, handleEvents, streamUniter)
import Uniter.Halt (MonadHalt (halt))

data Elem f =
    ElemNode !(Node f)
  | ElemEq !BoundId !BoundId
  | ElemFresh
deriving stock instance Eq (f BoundId) => Eq (Elem f)
deriving stock instance Show (f BoundId) => Show (Elem f)

newtype BoundEnv f = BoundEnv { unBoundEnv :: Map BoundId (Elem f) }
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
        ElemNode x -> resolveNode x b
        _ -> Left v

resolveNode :: (Corecursive t, Base t ~ f, Traversable f) => Node f -> BoundEnv f -> Either BoundId t
resolveNode n b = fmap embed (traverse (`resolveVar` b) (unNode n))

handleElems :: EventHandler e f m => BoundEnv f -> m ()
handleElems = traverse_ go . Map.toList . unBoundEnv where
  go (y, x) =
    case x of
      ElemNode n -> handleAddNode n y
      ElemEq i j -> handleEmitEq i j y
      ElemFresh -> handleFresh y

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
  let m' = Map.insert uniq (ElemNode n) m
  in (uniq, GraphState (succ uniq) (BoundEnv m'))

newtype AppM v e f a = AppM { unAppM :: ReaderT v (StateT (GraphState f) (Except e)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader v, MonadState (GraphState f), MonadError e)

instance MonadHalt e (AppM v e f) where
  halt = throwError

runAppM :: AppM v e f a -> v -> GraphState f -> Either e (a, GraphState f)
runAppM = undefined

streamUniterA :: UniterM v e f a -> AppM v e f (EventStream e f (a, BoundId))
streamUniterA u = do
  env <- ask
  uniq <- gets gsUnique
  pure (streamUniter u env uniq)

modifyBoundEnvA :: (Map BoundId (Elem f) -> Map BoundId (Elem f)) -> AppM v e f ()
modifyBoundEnvA f = modify' (\st -> st { gsBoundEnv = BoundEnv (f (unBoundEnv (gsBoundEnv st))) })

instance EventHandler e f (AppM v e f) where
  handleAddNode n i = modifyBoundEnvA (Map.insert i (ElemNode n))
  handleEmitEq i j y = modifyBoundEnvA (Map.insert y (ElemEq i j))
  handleFresh i = modifyBoundEnvA (Map.insert i ElemFresh)

appUniter :: UniterM v e f a -> AppM v e f a
appUniter u = do
  es <- streamUniterA u
  (r, uniq) <- handleEvents es
  modify' (\st -> st { gsUnique = uniq })
  pure r

appGraph :: GraphM f a -> AppM v e f a
appGraph = state . runState
