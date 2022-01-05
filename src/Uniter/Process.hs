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
import Uniter.Align (Alignable (..))
import Uniter.Core (BoundId (..), EventHandler (..), Node (..))
import Uniter.Halt (MonadHalt (halt))
import Uniter.UnionMap (UnionMap, UnionMapAddVal (..), UnionMapLens,
                        UnionMapLookupVal (UnionMapLookupValMissing, UnionMapLookupValOk), UnionMergeMany,
                        addUnionMapLM, emptyUnionMap, lookupUnionMapLM, memberUnionMap, mergeManyUnionMapLM)

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

data Item = Item
  { itemLeft :: !BoundId
  , itemRight :: !BoundId
  , itemRoot :: !BoundId
  }
  deriving stock (Eq, Show)

alignMerge :: Alignable e f => BoundId -> UnionMergeMany e (Defn f) (Seq Item, BoundId)
alignMerge = undefined
-- case (di, dj) of
--   (DefnFresh, _) -> mergeP rootItem $> rest
--   (_, DefnFresh) -> mergeP rootItem $> rest
--   (DefnNode (Node ni), DefnNode (Node nj)) -> do
--     case align ni nj of
--       Left e -> halt (ProcessErrorEmbed e)
--       Right g -> do
--         (ny, addlTriples) <- runWriterT $ for g $ \(a, b) -> do
--           c <- lift freshP
--           let item = Item a b c
--           tell (Seq.singleton item)
--           pure c
--         mergeP  (DefnNode (Node ny))
--         pure (addlTriples <> triples)

mergeP :: Item -> ProcessM e f (Seq Item)
mergeP = undefined

freshP :: ProcessM e f BoundId
freshP = state (\st -> let uniq = psUnique st in (uniq, st { psUnique = succ uniq }))

emitRecP :: Alignable e f => Seq Item -> ProcessM e f ()
emitRecP = \case
    Empty -> pure ()
    it :<| rest -> do
      guardNewP (itemRoot it)
      newIts <- mergeP it
      emitRecP (newIts <> rest)

defineP :: Defn f -> BoundId -> ProcessM e f ()
defineP d i = do
  guardNewP i
  val <- addUnionMapLM psUnionMapL i d
  case val of
    UnionMapAddValAdded -> pure ()
    UnionMapAddValDuplicate -> halt (ProcessErrorDuplicate i)

instance Alignable e f => EventHandler (ProcessError e) f (ProcessM e f) where
  handleAddNode = defineP . DefnNode
  handleEmitEq i j y = emitRecP (Seq.singleton (Item i j y))
  handleFresh = defineP DefnFresh
