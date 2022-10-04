{-# LANGUAGE UndecidableInstances #-}

-- | Drives unification by handling events and updating a UnionMap
module Uniter.Process
  ( RebindMap
  , ProcessErr (..)
  , ProcessState (..)
  , psUnionMapL
  , newProcessState
  , ProcessM
  , runProcessM
  -- , compact
  -- , canonicalize
  , extract
  , embedReuniterM
  ) where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Control.Monad.Except (Except, ExceptT, MonadError (..), runExcept, runExceptT)
import Control.Monad.State.Strict (MonadState (..), State, StateT, gets, modify', runState, runStateT, state)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, runWriterT, tell)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.These (These (..))
import Data.Traversable (for)
import Data.Typeable (Typeable)
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import Lens.Micro (lens)
import Uniter.Align (Alignable (..))
import Uniter.Core (Event (..), Node, PolyTy, TmVar, TyBinder (..), UniqueId (..))
import Uniter.Graph (Elem (..), Graph (..), elemTraversal)
import Uniter.Reunitable.Monad (ReuniterErr, ReuniterM, ReuniterState (..), newReuniterEnv, newReuniterState,
                                runReuniterM)
import Uniter.UnionMap (UnionEntry (..), UnionMap (..), UnionMapAddVal (..), UnionMapLens, UnionMapLookupVal (..),
                        UnionMapMergeVal (..), UnionMergeMany, addUnionMapLM, canonicalizeUnionMapLM, compactUnionMapLM,
                        emptyUnionMap, lookupUnionMapLM, mergeManyUnionMapLM)

type RebindMap = IntLikeMap UniqueId UniqueId

data ProcessErr e g =
    ProcessErrDuplicate !UniqueId
  | ProcessErrMissing !UniqueId
  | ProcessErrReuniter !ReuniterErr
  | ProcessErrSkolemAlign !TyBinder !(Elem g)
  | ProcessErrEmbed !e
  deriving stock (Typeable)

deriving stock instance (Eq e, Eq (Node g)) => Eq (ProcessErr e g)
deriving stock instance (Ord e, Ord (Node g)) => Ord (ProcessErr e g)
deriving stock instance (Show e, Show (Node g)) => Show (ProcessErr e g)

instance (Show e, Typeable e, Show (Node g), Typeable g) => Exception (ProcessErr e g)

data ProcessState g = ProcessState
  { psUnique :: !UniqueId
  , psUnionMap :: !(UnionMap UniqueId (Elem g))
  }

deriving stock instance Eq (g UniqueId) => Eq (ProcessState g)
deriving stock instance Show (g UniqueId) => Show (ProcessState g)

psUnionMapL :: UnionMapLens (ProcessState g) UniqueId (Elem g)
psUnionMapL = lens psUnionMap (\ps um -> ps { psUnionMap = um })

newProcessState :: UniqueId -> ProcessState g
newProcessState uniq = ProcessState uniq emptyUnionMap

newtype ProcessM e g a = ProcessM { unProcessM :: ExceptT (ProcessErr e g) (State (ProcessState g)) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (ProcessState g), MonadError (ProcessErr e g))

runProcessM :: ProcessM e g a -> ProcessState g -> (Either (ProcessErr e g) a, ProcessState g)
runProcessM = runState . runExceptT . unProcessM

compactOnState :: ProcessState g -> (RebindMap, ProcessState g)
compactOnState = runState (compactUnionMapLM psUnionMapL)

canonicalizeOnState :: Traversable g => ProcessState g -> (RebindMap, ProcessState g)
canonicalizeOnState = runState (canonicalizeUnionMapLM psUnionMapL elemTraversal)

extractOnState :: Traversable g => ProcessState g -> ((RebindMap, Graph g), ProcessState g)
extractOnState ps = res where
  res =
    let (m, ps'@(ProcessState _ u)) = canonicalizeOnState ps
        g = go1 (unUnionMap u)
    in ((m, g), ps')
  go1 im = Graph (fmap (go2 im) im)
  go2 im = \case
    -- compaction will have made this a one-link jump
    UnionEntryLink k ->
      case ILM.lookup k im of
        Nothing -> error ("Missing linked key: " ++ show k)
        Just v -> go2 im v
    UnionEntryValue d -> d

-- | Compact the union map - compresses all chains to directly reference roots for faster lookup
-- Returns a map of all rebound (non-root) ids to roots.
compact :: ProcessM e g RebindMap
compact = state compactOnState

-- | Canonicalize the union map - compacts and rewrites nodes identical in structure.
-- Returns a map of all rebound (non-root) ids to roots (include those removed during canonicalization).
canonicalize :: Traversable g => ProcessM e g RebindMap
canonicalize = state canonicalizeOnState

-- | Extracts a final graph from the union map, canonicalizing in the process.
extract :: Traversable g => ProcessM e g (RebindMap, Graph g)
extract = state extractOnState

lookupP :: UniqueId -> ProcessM e g (UniqueId, Elem g)
lookupP i = do
  val <- lookupUnionMapLM psUnionMapL i
  case val of
    UnionMapLookupValMissing x -> throwError (ProcessErrMissing x)
    UnionMapLookupValOk r d _ -> pure (r, d)

data Duo a = Duo !a !a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Item = Item
  { itemChildren :: !(Duo UniqueId)
  , itemRoot :: !UniqueId
  } deriving stock (Eq, Show)

-- Effect used entirely within 'alignMerge'
newtype AlignM e g a = AlignM { unAlignM :: WriterT (Seq Item) (StateT UniqueId (Except (ProcessErr e g))) a }
  deriving newtype (Functor, Applicative, Monad, MonadWriter (Seq Item), MonadState UniqueId, MonadError (ProcessErr e g))

runAlignM :: AlignM e g a -> UniqueId -> Either (ProcessErr e g) (a, UniqueId, Seq Item)
runAlignM al b = fmap (\((a, w), s) -> (a, s, w)) (runExcept (runStateT (runWriterT (unAlignM al)) b))

alignElems :: Alignable e g => Elem g -> Elem g -> AlignM e g (Elem g)
alignElems da db =
  case (da, db) of
    (ElemMeta (TyBinder tya), ElemMeta (TyBinder tyb)) -> pure (ElemMeta (TyBinder (tya <|> tyb)))
    (ElemMeta _, _) -> pure db
    (_, ElemMeta _) -> pure da
    -- It is always a domain error to align skolem vars, because
    -- they only align with themselves! The framework will not call this unless
    -- it's aligning two distinct vars.
    (ElemSkolem tya, _) -> throwError (ProcessErrSkolemAlign tya db)
    (_, ElemSkolem tyb) -> throwError (ProcessErrSkolemAlign tyb da)
    (ElemNode na, ElemNode nb) -> do
      case align na nb of
        Left e -> throwError (ProcessErrEmbed e)
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
alignMerge :: Alignable e g => UniqueId -> UnionMergeMany Duo (ProcessErr e g) (Elem g) (Seq Item, UniqueId)
alignMerge b mdx (Duo di dj) = res where
  res = fmap (\(v, s, w) -> ((w, s), v)) (runAlignM body b)
  body = do
    case mdx of
      Just dx -> do
        dy <- alignElems dx di
        alignElems dy dj
      Nothing -> error "impossible"

mergeP :: Alignable e g => Item -> ProcessM e g (Seq Item)
mergeP (Item children root) = do
  b <- gets psUnique
  erb <- mergeManyUnionMapLM psUnionMapL (alignMerge b) root children
  case erb of
    UnionMapMergeValMissing i -> throwError (ProcessErrMissing i)
    UnionMapMergeValEmbed e -> throwError e
    UnionMapMergeValMerged _ _ (r, i) -> modify' (\st -> st { psUnique = i }) $> r

freshP :: ProcessM e g UniqueId
freshP = state (\st -> let uniq = psUnique st in (uniq, st { psUnique = succ uniq }))

constrainP :: Alignable e g => Item -> ProcessM e g ()
constrainP it = do
  -- NOTE contract is that the root will not have been referenced before,
  -- so we have to add it to the graph when we see it
  defineP (ElemMeta (TyBinder Nothing)) (itemRoot it)
  constrainRecP (Seq.singleton it)

constrainRecP :: Alignable e g => Seq Item -> ProcessM e g ()
constrainRecP = \case
    Empty -> pure ()
    it :<| rest -> do
      newIts <- mergeP it
      -- see note in emitP
      for_ newIts $ \newIt -> defineP (ElemMeta (TyBinder Nothing)) (itemRoot newIt)
      constrainRecP (newIts <> rest)

defineP :: Elem g -> UniqueId -> ProcessM e g ()
defineP d i = do
  val <- addUnionMapLM psUnionMapL i d
  case val of
    UnionMapAddValAdded -> pure ()
    UnionMapAddValDuplicate -> throwError (ProcessErrDuplicate i)

handleEvent :: Alignable e g => Event g -> ProcessM e g ()
handleEvent = \case
  EventAddNode n k -> defineP (ElemNode n) k
  EventConstrainEq i j k -> constrainP (Item (Duo i j) k)
  EventNewMetaVar tyb k -> defineP (ElemMeta tyb) k
  EventNewSkolemVar tyb k -> defineP (ElemSkolem tyb) k

embedReuniterM :: Alignable e g => Map TmVar (PolyTy g) -> ReuniterM g a -> ProcessM e g a
embedReuniterM fm m = do
  q <- gets psUnique
  let re = newReuniterEnv fm
      rs = newReuniterState q
  let (ea, ReuniterState q' evs) = runReuniterM m re rs
  case ea of
    Left e -> throwError (ProcessErrReuniter e)
    Right a -> do
      modify' (\ps -> ps { psUnique = q' })
      for_ evs handleEvent
      pure a
