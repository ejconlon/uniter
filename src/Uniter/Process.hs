{-# LANGUAGE UndecidableInstances #-}

module Uniter.Process
  ( ProcessError (..)
  , Defn (..)
  , defnTraversal
  , ProcessState (..)
  , newProcessState
  , ProcessM
  , runProcessM
  , processCompact
  , processCanonicalize
  , processBoundEnv
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState, StateT, gets, modify', runStateT, state)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, runWriterT, tell)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Lens.Micro (Traversal', lens)
import Overeasy.IntLike.Map (IntLikeMap)
import Uniter.Align (Alignable (..))
import Uniter.Core (BoundId (..), EventHandler (..), Node (..))
import Uniter.Graph (BoundEnv (BoundEnv), Elem (ElemFresh, ElemNode))
import Uniter.Halt (MonadHalt (halt))
import Uniter.State (KeepM, runKeepM)
import Uniter.UnionMap (UnionEntry (..), UnionMap, UnionMapAddVal (..), UnionMapLens, UnionMapLookupVal (..),
                        UnionMapMergeVal (..), UnionMergeMany, addUnionMapLM, canonicalizeUnionMapLM, compactUnionMapLM,
                        emptyUnionMap, lookupUnionMapLM, mergeManyUnionMapLM, unUnionMap)

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

defnTraversal :: Traversable f => Traversal' (Defn f) BoundId
defnTraversal g = \case
  DefnFresh -> pure DefnFresh
  DefnNode (Node fb) -> fmap (DefnNode . Node) (traverse g fb)

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

newtype ProcessM e f a = ProcessM { unProcessM :: KeepM (ProcessError e) (ProcessState f) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (ProcessState f), MonadHalt (ProcessError e))

runProcessM :: ProcessM e f a -> ProcessState f -> (Either (ProcessError e) a, ProcessState f)
runProcessM = runKeepM . unProcessM

processCompact :: ProcessM e f (IntLikeMap BoundId BoundId)
processCompact = compactUnionMapLM psUnionMapL

processCanonicalize :: Traversable f => ProcessM e f (IntLikeMap BoundId BoundId)
processCanonicalize = canonicalizeUnionMapLM psUnionMapL defnTraversal

processBoundEnv :: Traversable f => ProcessM e f (IntLikeMap BoundId BoundId, BoundEnv f)
processBoundEnv = res where
  res = do
    m <- processCanonicalize
    v <- gets (go1 . psUnionMap)
    pure (m, v)
  go1 = BoundEnv . fmap go2 . unUnionMap
  go2 = \case
    UnionEntryLink _ -> error "impossible"
    UnionEntryValue d ->
      case d of
        DefnFresh -> ElemFresh
        DefnNode n -> ElemNode n

-- TODO can remove this because the checks happen inline
-- guardNewP :: BoundId -> ProcessM e f ()
-- guardNewP i = do
--   isMem <- gets (memberUnionMap i . psUnionMap)
--   when isMem (halt (ProcessErrorDuplicate i))

lookupP :: BoundId -> ProcessM e f (BoundId, Defn f)
lookupP i = do
  val <- lookupUnionMapLM psUnionMapL i
  case val of
    UnionMapLookupValMissing x -> halt (ProcessErrorMissing x)
    UnionMapLookupValOk r d _ -> pure (r, d)

data Duo a = Duo !a !a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Item = Item
  { itemChildren :: !(Duo BoundId)
  , itemRoot :: !BoundId
  } deriving stock (Eq, Show)

newtype AlignM e a = AlignM { unAlignM :: WriterT (Seq Item) (StateT BoundId (Except e)) a }
  deriving newtype (Functor, Applicative, Monad, MonadWriter (Seq Item), MonadState BoundId)

instance MonadHalt e (AlignM e) where
  halt = AlignM . throwError

runAlignM :: AlignM e a -> BoundId -> Either e (a, BoundId, Seq Item)
runAlignM al b = fmap (\((a, w), s) -> (a, s, w)) (runExcept (runStateT (runWriterT (unAlignM al)) b))

alignDefns :: Alignable e f => Defn f -> Defn f -> AlignM e (Defn f)
alignDefns da db =
  case (da, db) of
    (DefnFresh, _) -> pure db
    (_, DefnFresh) -> pure da
    (DefnNode (Node na), DefnNode (Node nb)) -> do
      case alignWith Duo na nb of
        Left e -> halt e
        Right g -> do
          h <- for g $ \duo -> do
            root <- state (\x -> (x, succ x))
            let item = Item duo root
            tell (Seq.singleton item)
            pure root
          pure (DefnNode (Node h))

alignMerge :: Alignable e f => BoundId -> UnionMergeMany Duo e (Defn f) (Seq Item, BoundId)
alignMerge b mdx (Duo di dj) = res where
  res = fmap (\(v, s, w) -> ((w, s), v)) (runAlignM body b)
  body = do
    case mdx of
      Just dx -> do
        dy <- alignDefns dx di
        alignDefns dy dj
      Nothing -> error "impossible"

mergeP :: Alignable e f => Item -> ProcessM e f (Seq Item)
mergeP (Item children root) = do
  b <- gets psUnique
  erb <- mergeManyUnionMapLM psUnionMapL (alignMerge b) root children
  case erb of
    UnionMapMergeValMissing i -> halt (ProcessErrorMissing i)
    UnionMapMergeValEmbed e -> halt (ProcessErrorEmbed e)
    UnionMapMergeValMerged _ _ (r, i) -> modify' (\st -> st { psUnique = i }) $> r

freshP :: ProcessM e f BoundId
freshP = state (\st -> let uniq = psUnique st in (uniq, st { psUnique = succ uniq }))

emitP :: Alignable e f => Item -> ProcessM e f ()
emitP it = do
  -- TODO need to define fresh here?
  defineP DefnFresh (itemRoot it)
  emitRecP (Seq.singleton it)

emitRecP :: Alignable e f => Seq Item -> ProcessM e f ()
emitRecP = \case
    Empty -> pure ()
    it :<| rest -> do
      -- TODO disabled guard because will check inline
      -- guardNewP (itemRoot it)
      newIts <- mergeP it
      -- TODO need to define fresh here?
      for_ newIts $ \newIt -> defineP DefnFresh (itemRoot newIt)
      emitRecP (newIts <> rest)

defineP :: Defn f -> BoundId -> ProcessM e f ()
defineP d i = do
  -- TODO disabled guard because will check inline
  -- guardNewP i
  val <- addUnionMapLM psUnionMapL i d
  case val of
    UnionMapAddValAdded -> pure ()
    UnionMapAddValDuplicate -> halt (ProcessErrorDuplicate i)

instance Alignable e f => EventHandler (ProcessError e) f (ProcessM e f) where
  handleAddNode = defineP . DefnNode
  handleEmitEq i j y = emitP (Item (Duo i j) y)
  handleFresh = defineP DefnFresh
