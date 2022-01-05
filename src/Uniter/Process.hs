{-# LANGUAGE UndecidableInstances #-}

module Uniter.Process where

import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState, StateT, gets, runStateT, state)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.Strict (runWriterT, tell)
import Data.Functor (($>))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Lens.Micro (lens)
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Uniter.Align (Alignable (..))
import Uniter.Core (BoundId (..), EventHandler (..), Node (..))
import Uniter.Halt (MonadHalt (halt))
import Uniter.UnionMap (UnionMap, UnionMapAddVal (..), UnionMapLens,
                        UnionMapLookupVal (UnionMapLookupValMissing, UnionMapLookupValOk), addUnionMapLM, emptyUnionMap,
                        lookupUnionMapLM, memberUnionMap)

data ProcessError e =
    ProcessErrorDuplicate !BoundId
  | ProcessErrorMissing !BoundId
  | ProcessErrorEmbed !e
  deriving stock (Eq, Show, Typeable)

instance (Show e, Typeable e) => Exception (ProcessError e)

data Defn f =
    DefnNode !(Node f)
  | DefnFresh
deriving stock instance Eq (f BoundId) => Eq (Defn f)
deriving stock instance Show (f BoundId) => Show (Defn f)

data ProcessState f = ProcessState
  { psUnique :: !BoundId
  , psUnionMap :: !(UnionMap BoundId (Defn f))
  }
deriving stock instance Eq (f BoundId) => Eq (ProcessState f)
deriving stock instance Show (f BoundId) => Show (ProcessState f)

psUnionMapL :: UnionMapLens (ProcessState f) BoundId (Defn f)
psUnionMapL = lens psUnionMap (\ps um -> ps { psUnionMap = um })

newProcessState :: BoundId -> ProcessState f
newProcessState uniq = ProcessState uniq emptyUnionMap

newtype ProcessM e f a = ProcessM
  { unProcessM :: StateT (ProcessState f) (Except (ProcessError e) ) a
  } deriving newtype (Functor, Applicative, Monad, MonadState (ProcessState f))

instance MonadHalt (ProcessError e) (ProcessM e f) where
  halt = ProcessM . throwError

runProcessM :: ProcessM e f a -> ProcessState f -> Either (ProcessError e) (a, ProcessState f)
runProcessM p = runExcept . runStateT (unProcessM p)

guardNewP :: BoundId -> ProcessM e f ()
guardNewP i = do
  isMem <- gets (memberUnionMap i . psUnionMap)
  when isMem (halt (ProcessErrorDuplicate i))

lookupP :: BoundId -> ProcessM e f (BoundId, Defn f)
lookupP i = do
  val <- lookupUnionMapLM psUnionMapL i
  case val of
    UnionMapLookupValMissing x -> halt (ProcessErrorMissing x)
    UnionMapLookupValOk r d _ -> pure (r, d)

mergeP :: IntLikeSet BoundId -> ProcessM e f ()
mergeP = undefined

mergeAsP :: IntLikeSet BoundId -> Defn f -> ProcessM e f ()
mergeAsP = undefined

freshP :: ProcessM e f BoundId
freshP = state (\st -> let uniq = psUnique st in (uniq, st { psUnique = succ uniq }))

data Triple = Triple !BoundId !BoundId !BoundId
  deriving stock (Eq, Show)

emitRecP :: Alignable e f => Seq Triple -> ProcessM e f ()
emitRecP = \case
    Empty -> pure ()
    Triple i j y :<| triples -> do
      guardNewP y
      (ri, di) <- lookupP i
      (rj, dj) <- lookupP j
      let rootTrip = ILS.fromList [ri, rj, y]
      newTriples <-
        case (di, dj) of
          (DefnFresh, _) -> mergeP rootTrip $> triples
          (_, DefnFresh) -> mergeP rootTrip $> triples
          (DefnNode (Node ni), DefnNode (Node nj)) -> do
            case align ni nj of
              Left e -> halt (ProcessErrorEmbed e)
              Right g -> do
                (ny, addlTriples) <- runWriterT $ for g $ \(a, b) -> do
                  c <- lift freshP
                  let triple = Triple a b c
                  tell (Seq.singleton triple)
                  pure c
                mergeAsP rootTrip (DefnNode (Node ny))
                pure (addlTriples <> triples)
      emitRecP newTriples

defineP :: Defn f -> BoundId -> ProcessM e f ()
defineP d i = do
  guardNewP i
  val <- addUnionMapLM psUnionMapL i d
  case val of
    UnionMapAddValAdded -> pure ()
    UnionMapAddValDuplicate -> halt (ProcessErrorDuplicate i)

instance Alignable e f => EventHandler (ProcessError e) f (ProcessM e f) where
  handleAddNode = defineP . DefnNode
  handleEmitEq i j y = emitRecP (Seq.singleton (Triple i j y))
  handleFresh = defineP DefnFresh
