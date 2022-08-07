{-# LANGUAGE UndecidableInstances #-}

-- | Drives unification by handling events and updating a UnionMap
module Uniter.Process
  ( RebindMap
  , ProcessErr (..)
  , ProcessState (..)
  , psUnionMapL
  , newProcessState
  , ProcessT
  , runProcessT
  , compact
  , canonicalize
  , extract
  , embedUniterT
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (Except, ExceptT, MonadError (..), runExcept, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState (..), StateT, gets, modify', runState, runStateT, state)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer.Strict (MonadWriter, WriterT, runWriterT, tell)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.These (These (..))
import Data.Traversable (for)
import Data.Typeable (Typeable)
import IntLike.Map (IntLikeMap)
import Lens.Micro (lens)
import Uniter.Align (Alignable (..))
import Uniter.Core (BoundId (..), Event (..))
import Uniter.Graph (Elem (..), Graph (..), elemTraversal)
import Uniter.Monad (UniterState (..), UniterT, runUniterT)
import Uniter.UnionMap (UnionEntry (..), UnionMap (..), UnionMapAddVal (..), UnionMapLens, UnionMapLookupVal (..),
                        UnionMapMergeVal (..), UnionMergeMany, addUnionMapLM, canonicalizeUnionMapLM, compactUnionMapLM,
                        emptyUnionMap, lookupUnionMapLM, mergeManyUnionMapLM)

type RebindMap = IntLikeMap BoundId BoundId

data ProcessErr e =
    ProcessErrDuplicate !BoundId
  | ProcessErrMissing !BoundId
  | ProcessErrEmbed !e
  deriving stock (Eq, Ord, Show, Typeable)

instance (Show e, Typeable e) => Exception (ProcessErr e)

data ProcessState g = ProcessState
  { psUnique :: !BoundId
  , psUnionMap :: !(UnionMap BoundId (Elem g))
  }

deriving stock instance Eq (g BoundId) => Eq (ProcessState g)
deriving stock instance Show (g BoundId) => Show (ProcessState g)

psUnionMapL :: UnionMapLens (ProcessState g) BoundId (Elem g)
psUnionMapL = lens psUnionMap (\ps um -> ps { psUnionMap = um })

newProcessState :: BoundId -> ProcessState g
newProcessState uniq = ProcessState uniq emptyUnionMap

newtype ProcessT e g m a = ProcessT { unProcessT :: ExceptT (ProcessErr e) (StateT (ProcessState g) m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState (ProcessState g), MonadError (ProcessErr e))

instance MonadTrans (ProcessT e g) where
  lift = ProcessT . lift . lift

runProcessT :: ProcessT e g m a -> ProcessState g -> m (Either (ProcessErr e) a, ProcessState g)
runProcessT = runStateT . runExceptT . unProcessT

compactOnState :: ProcessState g -> (RebindMap, ProcessState g)
compactOnState = runState (compactUnionMapLM psUnionMapL)

canonicalizeOnState :: Traversable g => ProcessState g -> (RebindMap, ProcessState g)
canonicalizeOnState = runState (canonicalizeUnionMapLM psUnionMapL elemTraversal)

extractOnState :: Traversable g => ProcessState g -> ((RebindMap, Graph g), ProcessState g)
extractOnState ps = res where
  res =
    let (m, ps'@(ProcessState _ u)) = canonicalizeOnState ps
        g = go1 u
    in ((m, g), ps')
  go1 = Graph . fmap go2 . unUnionMap
  go2 = \case
    -- canonicalization will have eliminated all links
    UnionEntryLink _ -> error "impossible"
    UnionEntryValue d -> d

-- | Compact the union map - compresses all chains to directly reference roots for faster lookup
-- Returns a map of all rebound (non-root) ids to roots.
compact :: Monad m => ProcessT e g m RebindMap
compact = state compactOnState

-- | Canonicalize the union map - compacts and eliminates nodes identical in structure.
-- Returns a map of all rebound (non-root) ids to roots (include those removed during canonicalization).
canonicalize :: (Monad m, Traversable g) => ProcessT e g m RebindMap
canonicalize = state canonicalizeOnState

-- | Extracts a final graph from the union map, canonicalizing in the process.
extract :: (Monad m, Traversable g) => ProcessT e g m (RebindMap, Graph g)
extract = state extractOnState

lookupP :: Monad m => BoundId -> ProcessT e g m (BoundId, Elem g)
lookupP i = do
  val <- lookupUnionMapLM psUnionMapL i
  case val of
    UnionMapLookupValMissing x -> throwError (ProcessErrMissing x)
    UnionMapLookupValOk r d _ -> pure (r, d)

data Duo a = Duo !a !a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Item = Item
  { itemChildren :: !(Duo BoundId)
  , itemRoot :: !BoundId
  } deriving stock (Eq, Show)

-- Effect used entirely within 'alignMerge'
newtype AlignM e a = AlignM { unAlignM :: WriterT (Seq Item) (StateT BoundId (Except e)) a }
  deriving newtype (Functor, Applicative, Monad, MonadWriter (Seq Item), MonadState BoundId, MonadError e)

runAlignM :: AlignM e a -> BoundId -> Either e (a, BoundId, Seq Item)
runAlignM al b = fmap (\((a, w), s) -> (a, s, w)) (runExcept (runStateT (runWriterT (unAlignM al)) b))

alignElems :: Alignable e g => Elem g -> Elem g -> AlignM e (Elem g)
alignElems da db =
  case (da, db) of
    (ElemFresh, _) -> pure db
    (_, ElemFresh) -> pure da
    (ElemNode na, ElemNode nb) -> do
      case align na nb of
        Left e -> throwError e
        Right g -> do
          h <- for g $ \these -> do
            case these of
              This a -> pure a
              That b -> pure b
              These a b -> do
                let duo = Duo a b
                root <- state (\x -> (x, succ x))
                let item = Item duo root
                tell (Seq.singleton item)
                pure root
          pure (ElemNode h)

-- | Callback to be provided to a union map to merge values of the same key by aligning their structures.
alignMerge :: Alignable e g => BoundId -> UnionMergeMany Duo e (Elem g) (Seq Item, BoundId)
alignMerge b mdx (Duo di dj) = res where
  res = fmap (\(v, s, w) -> ((w, s), v)) (runAlignM body b)
  body = do
    case mdx of
      Just dx -> do
        dy <- alignElems dx di
        alignElems dy dj
      Nothing -> error "impossible"

mergeP :: (Monad m, Alignable e g) => Item -> ProcessT e g m (Seq Item)
mergeP (Item children root) = do
  b <- gets psUnique
  erb <- mergeManyUnionMapLM psUnionMapL (alignMerge b) root children
  case erb of
    UnionMapMergeValMissing i -> throwError (ProcessErrMissing i)
    UnionMapMergeValEmbed e -> throwError (ProcessErrEmbed e)
    UnionMapMergeValMerged _ _ (r, i) -> modify' (\st -> st { psUnique = i }) $> r

freshP :: Monad m => ProcessT e g m BoundId
freshP = state (\st -> let uniq = psUnique st in (uniq, st { psUnique = succ uniq }))

constrainP :: (Monad m, Alignable e g) => Item -> ProcessT e g m ()
constrainP it = do
  -- NOTE contract is that the root will not have been referenced before,
  -- so we have to add it to the graph when we see it
  defineP ElemFresh (itemRoot it)
  constrainRecP (Seq.singleton it)

constrainRecP :: (Monad m, Alignable e g) => Seq Item -> ProcessT e g m ()
constrainRecP = \case
    Empty -> pure ()
    it :<| rest -> do
      newIts <- mergeP it
      -- see note in emitP
      for_ newIts $ \newIt -> defineP ElemFresh (itemRoot newIt)
      constrainRecP (newIts <> rest)

defineP :: Monad m => Elem g -> BoundId -> ProcessT e g m ()
defineP d i = do
  val <- addUnionMapLM psUnionMapL i d
  case val of
    UnionMapAddValAdded -> pure ()
    UnionMapAddValDuplicate -> throwError (ProcessErrDuplicate i)

handleEvent :: (Monad m, Alignable e g) => Event g -> ProcessT e g m ()
handleEvent = \case
  EventAddNode n k -> defineP (ElemNode n) k
  EventConstrainEq i j k -> constrainP (Item (Duo i j) k)
  EventFreshVar k -> defineP ElemFresh k

embedUniterT :: (Monad m, Alignable e g) => UniterT g m a -> ProcessT e g m a
embedUniterT u = do
  q <- gets psUnique
  let us = UniterState q Empty
  (a, UniterState q' evs) <- lift (runUniterT u us)
  modify' (\ps -> ps { psUnique = q' })
  for_ evs handleEvent
  pure a
