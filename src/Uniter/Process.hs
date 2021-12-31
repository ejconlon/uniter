{-# LANGUAGE UndecidableInstances #-}

module Uniter.Process where

import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState, StateT, gets, modify', runStateT, state)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.Strict (runWriterT, tell)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Overeasy.EquivFind (EquivFind, efAddInc, efFindRoot, efMember, efUnsafeMerge)
import Uniter.Align (Alignable (..))
import Uniter.Core (BoundId (..), EventHandler (..), Node (..), UniterError)

data ProcessError e =
    ProcessErrorUniter !(UniterError e)
  | ProcessErrorDuplicate !BoundId
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
  , psEquiv :: !(EquivFind BoundId)
  , psNodes :: Map BoundId (Defn f)
  }
deriving stock instance Eq (f BoundId) => Eq (ProcessState f)
deriving stock instance Show (f BoundId) => Show (ProcessState f)

newtype ProcessM e f a = ProcessM
  { unProcessM :: StateT (ProcessState f) (Except (ProcessError e) ) a
  } deriving newtype (Functor, Applicative, Monad, MonadError (ProcessError e), MonadState (ProcessState f))

runProcessM :: ProcessM e f a -> ProcessState f -> Either (ProcessError e) (a, ProcessState f)
runProcessM p = runExcept . runStateT (unProcessM p)

guardNewP :: BoundId -> ProcessM e f ()
guardNewP i = do
  isMem <- gets (efMember i . psEquiv)
  when isMem (throwError (ProcessErrorDuplicate i))

lookupP :: BoundId -> ProcessM e f (BoundId, Defn f)
lookupP i = do
  mayRoot <- gets (efFindRoot i . psEquiv)
  case mayRoot of
    Nothing -> throwError (ProcessErrorMissing i)
    Just r -> gets (\st -> (r, psNodes st Map.! r))

mergeP :: BoundId -> BoundId -> ProcessM e f ()
mergeP = undefined

mergeAsP :: BoundId -> BoundId -> Defn f -> ProcessM e f ()
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
      newTriples <-
        if ri /= rj
          then case (di, dj) of
            (DefnFresh, _) -> mergeP ri rj $> triples
            (_, DefnFresh) -> mergeP ri rj $> triples
            (DefnNode (Node ni), DefnNode (Node nj)) -> do
              case align ni nj of
                Left e -> throwError (ProcessErrorEmbed e)
                Right g -> do
                  (ny, addlTriples) <- runWriterT $ for g $ \(a, b) -> do
                    c <- lift freshP
                    let triple = Triple a b c
                    tell (Seq.singleton triple)
                    pure c
                  mergeAsP ri rj (DefnNode (Node ny))
                  pure (addlTriples <> triples)
          else pure triples
      emitRecP newTriples

defineP :: Defn f -> BoundId -> ProcessM e f ()
defineP d i = do
  guardNewP i
  modify' $ \(ProcessState uniq equiv nodes) ->
    let equiv' = snd (efAddInc i equiv)
        nodes' = Map.insert i d nodes
    in ProcessState uniq equiv' nodes'

instance Alignable e f => EventHandler e f (ProcessM e f) where
  handleError = throwError . ProcessErrorUniter
  handleAddNode = defineP . DefnNode
  handleEmitEq i j y = emitRecP (Seq.singleton (Triple i j y))
  handleFresh = defineP DefnFresh
