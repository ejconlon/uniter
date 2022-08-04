{-# LANGUAGE UndecidableInstances #-}

-- | The core of unification - take a
module Uniter.Process
  ( ProcessErr (..)
  , Defn (..)
  , defnTraversal
  , ProcessState (..)
  , psUnionMapL
  , newProcessState
  , ProcessM
  , runProcessM
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState, StateT, gets, modify', runStateT, state)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, runWriterT, tell)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.These (These (..))
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Lens.Micro (Traversal', lens)
import Uniter.Align (Alignable (..))
import Uniter.Core (BoundId (..), EventHandler (..), Node)
import Uniter.Halt (MonadHalt (halt))
import Uniter.State (KeepM, runKeepM)
import Uniter.UnionMap (UnionMap, UnionMapAddVal (..), UnionMapLens, UnionMapLookupVal (..), UnionMapMergeVal (..),
                        UnionMergeMany, addUnionMapLM, emptyUnionMap, lookupUnionMapLM, mergeManyUnionMapLM)

data ProcessErr e =
    ProcessErrDuplicate !BoundId
  | ProcessErrMissing !BoundId
  | ProcessErrEmbed !e
  deriving stock (Eq, Ord, Show, Typeable)

instance (Show e, Typeable e) => Exception (ProcessErr e)

data Defn f =
    DefnNode !(Node f)
  | DefnFresh
deriving stock instance Eq (f BoundId) => Eq (Defn f)
deriving stock instance Show (f BoundId) => Show (Defn f)

-- | A traversal over 'Defn's - can get or replace all 'BoundId's
defnTraversal :: Traversable f => Traversal' (Defn f) BoundId
defnTraversal g = \case
  DefnFresh -> pure DefnFresh
  DefnNode fb -> fmap DefnNode (traverse g fb)

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

newtype ProcessM e f a = ProcessM { unProcessM :: KeepM (ProcessErr e) (ProcessState f) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (ProcessState f), MonadHalt (ProcessErr e))

runProcessM :: ProcessM e f a -> ProcessState f -> (Either (ProcessErr e) a, ProcessState f)
runProcessM = runKeepM . unProcessM

lookupP :: BoundId -> ProcessM e f (BoundId, Defn f)
lookupP i = do
  val <- lookupUnionMapLM psUnionMapL i
  case val of
    UnionMapLookupValMissing x -> halt (ProcessErrMissing x)
    UnionMapLookupValOk r d _ -> pure (r, d)

data Duo a = Duo !a !a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Item = Item
  { itemChildren :: !(Duo BoundId)
  , itemRoot :: !BoundId
  } deriving stock (Eq, Show)

-- Effect used entirely within 'alignMerge'
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
    (DefnNode na, DefnNode nb) -> do
      case align na nb of
        Left e -> halt e
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
          pure (DefnNode h)

-- | Callback to be provided to a union map to merge values of the same key by aligning their structures.
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
    UnionMapMergeValMissing i -> halt (ProcessErrMissing i)
    UnionMapMergeValEmbed e -> halt (ProcessErrEmbed e)
    UnionMapMergeValMerged _ _ (r, i) -> modify' (\st -> st { psUnique = i }) $> r

freshP :: ProcessM e f BoundId
freshP = state (\st -> let uniq = psUnique st in (uniq, st { psUnique = succ uniq }))

emitP :: Alignable e f => Item -> ProcessM e f ()
emitP it = do
  -- NOTE contract is that the root will not have been referenced before,
  -- so we have to add it to the graph when we see it
  defineP DefnFresh (itemRoot it)
  emitRecP (Seq.singleton it)

emitRecP :: Alignable e f => Seq Item -> ProcessM e f ()
emitRecP = \case
    Empty -> pure ()
    it :<| rest -> do
      newIts <- mergeP it
      -- see note in emitP
      for_ newIts $ \newIt -> defineP DefnFresh (itemRoot newIt)
      emitRecP (newIts <> rest)

defineP :: Defn f -> BoundId -> ProcessM e f ()
defineP d i = do
  val <- addUnionMapLM psUnionMapL i d
  case val of
    UnionMapAddValAdded -> pure ()
    UnionMapAddValDuplicate -> halt (ProcessErrDuplicate i)

instance Alignable e f => EventHandler (ProcessErr e) f (ProcessM e f) where
  handleAddNode = defineP . DefnNode
  handleEmitEq i j y = emitP (Item (Duo i j) y)
  handleFresh = defineP DefnFresh
