module Uniter.UnionMap
  ( Changed (..)
  , maybeChanged
  , UnionEquiv (..)
  , emptyUnionEquiv
  , UnionEntry (..)
  , UnionMergeOne
  , UnionMergeMany
  , adaptUnionMergeOne
  , foldUnionMergeOne
  , foldUnionMergeMany
  , concatUnionMergeOne
  , concatUnionMergeMany
  , UnionMap (unUnionMap)
  , UnionMapLens
  , emptyUnionMap
  , sizeUnionMap
  , memberUnionMap
  , toListUnionMap
  , valuesUnionMap
  , UnionMapAddRes (..)
  , UnionMapAddVal (..)
  , addUnionMap
  , addUnionMapLM
  , addUnionMapM
  , UnionMapTraceRes (..)
  , traceUnionMap
  , UnionMapLookupRes (..)
  , UnionMapLookupVal (..)
  , lookupUnionMap
  , lookupUnionMapLM
  , lookupUnionMapM
  , equivUnionMap
  , equivUnionMapLM
  , equivUnionMapM
  , compactUnionMap
  , compactUnionMapLM
  , compactUnionMapM
  , canonicalizeUnionMap
  , canonicalizeUnionMapLM
  , canonicalizeUnionMapM
  , UnionMapUpdateRes (..)
  , UnionMapUpdateVal (..)
  , updateUnionMap
  , updateUnionMapLM
  , updateUnionMapM
  , UnionMapMergeRes (..)
  , UnionMapMergeVal (..)
  , mergeOneUnionMap
  , mergeOneUnionMapLM
  , mergeOneUnionMapM
  , mergeManyUnionMap
  , mergeManyUnionMapLM
  , mergeManyUnionMapM
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (MonadState, get, put)
import Data.Coerce (Coercible)
import Data.Foldable (fold, foldl', toList)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import qualified IntLike.Set as ILS
import Lens.Micro (Lens', Traversal', over)
import Uniter.Halt (halt)
import Uniter.State (mayStateLens, runDropM, stateLens)

safeTail :: [a] -> [a]
safeTail xs =
  case xs of
    [] -> xs
    _:ys -> ys

data Changed =
    ChangedYes
  | ChangedNo
  deriving stock (Eq, Show)

maybeChanged :: Maybe a -> Changed
maybeChanged = \case
  Nothing -> ChangedNo
  Just _ -> ChangedYes

data UnionEquiv k = UnionEquiv
  { ueFwd :: !(IntLikeMap k (IntLikeSet k))
  , ueBwd :: !(IntLikeMap k k)
  } deriving stock (Eq, Show)

emptyUnionEquiv :: UnionEquiv k
emptyUnionEquiv = UnionEquiv ILM.empty ILM.empty

data UnionEntry k v =
    UnionEntryLink !k
  | UnionEntryValue !v
  deriving stock (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

type UnionMergeOne e v r = Maybe v -> v -> Either e (r, v)
type UnionMergeMany f e v r = Maybe v -> f v -> Either e (r, v)

adaptUnionMergeOne :: (v -> f v) -> UnionMergeMany f e v r -> UnionMergeOne e v r
adaptUnionMergeOne h g mv = g mv . h

foldUnionMergeOne :: Monoid r => (v -> v -> Either e (r, v)) -> UnionMergeOne e v r
foldUnionMergeOne g mv v =
  case mv of
    Nothing -> Right (mempty, v)
    Just w -> g w v

foldUnionMergeMany :: (Foldable f, Monoid r) => Either e v -> (v -> v -> Either e (r, v)) -> UnionMergeMany f e v r
foldUnionMergeMany onE g mv fv = start where
  start =
    let vs = toList fv
    in case maybe vs (:vs) mv of
      [] -> fmap (mempty,) onE
      y:ys -> go mempty y ys
  go !r !v = \case
    [] -> Right (r, v)
    w:ws ->
      case g v w of
        Right (s, u) -> go (r <> s) u ws
        e@(Left _) -> e

concatUnionMergeOne :: Semigroup v => UnionMergeOne e v ()
concatUnionMergeOne mv v = Right ((), maybe v (<> v) mv)

concatUnionMergeMany :: (Foldable f, Monoid v) => UnionMergeMany f e v ()
concatUnionMergeMany mv vs = concatUnionMergeOne mv (fold vs)

newtype UnionMap k v = UnionMap { unUnionMap :: IntLikeMap k (UnionEntry k v) }
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

type UnionMapLens s k v = Lens' s (UnionMap k v)

emptyUnionMap :: UnionMap k v
emptyUnionMap = UnionMap ILM.empty

sizeUnionMap :: UnionMap k v -> Int
sizeUnionMap = ILM.size . unUnionMap

memberUnionMap :: Coercible k Int => k -> UnionMap k v -> Bool
memberUnionMap k = ILM.member k . unUnionMap

toListUnionMap :: Coercible k Int => UnionMap k v -> [(k, UnionEntry k v)]
toListUnionMap = ILM.toList . unUnionMap

valuesUnionMap :: Coercible k Int => UnionMap k v -> IntLikeMap k v
valuesUnionMap = foldl' go ILM.empty . toListUnionMap where
  go m (k, ue) =
    case ue of
      UnionEntryValue v -> ILM.insert k v m
      _ -> m

data UnionMapAddRes k v =
    UnionMapAddResAdded !(UnionMap k v)
  | UnionMapAddResDuplicate
  deriving stock (Eq, Show)

addUnionMap :: Coercible k Int => k -> v -> UnionMap k v -> UnionMapAddRes k v
addUnionMap k v (UnionMap m) =
  case ILM.lookup k m of
    Nothing -> UnionMapAddResAdded (UnionMap (ILM.insert k (UnionEntryValue v) m))
    Just _ -> UnionMapAddResDuplicate

data UnionMapAddVal =
    UnionMapAddValAdded
  | UnionMapAddValDuplicate
  deriving stock (Eq, Show)

addUnionMapLM :: (Coercible k Int, MonadState s m) => UnionMapLens s k v -> k -> v -> m UnionMapAddVal
addUnionMapLM l k v = mayStateLens l $ \u ->
  case addUnionMap k v u of
    UnionMapAddResAdded w -> (UnionMapAddValAdded, Just w)
    UnionMapAddResDuplicate -> (UnionMapAddValDuplicate, Nothing)

addUnionMapM :: (Coercible k Int, MonadState (UnionMap k v) m) => k -> v -> m UnionMapAddVal
addUnionMapM = addUnionMapLM id

data UnionMapTraceRes k v =
    UnionMapTraceResMissing !k
  | UnionMapTraceResFound !k !v ![k]
  deriving stock (Eq, Show)

traceUnionMap :: Coercible k Int => k -> UnionMap k v -> UnionMapTraceRes k v
traceUnionMap k (UnionMap m) = go [] k where
  go !acc j =
    case ILM.lookup j m of
      Nothing -> UnionMapTraceResMissing j
      Just link -> case link of
        UnionEntryLink kx -> go (j:acc) kx
        UnionEntryValue v -> UnionMapTraceResFound j v acc

data UnionMapLookupRes k v =
    UnionMapLookupResMissing !k
  | UnionMapLookupResFound !k !v !(Maybe (UnionMap k v))
  deriving stock (Eq, Show)

lookupUnionMap :: Coercible k Int => k -> UnionMap k v -> UnionMapLookupRes k v
lookupUnionMap k u = case traceUnionMap k u of
  UnionMapTraceResMissing kx -> UnionMapLookupResMissing kx
  UnionMapTraceResFound kr vr acc ->
    let mu = if null acc then Nothing else Just (foldl' (\(UnionMap n) kx -> UnionMap (ILM.insert kx (UnionEntryLink kr) n)) u (safeTail acc))
    in UnionMapLookupResFound kr vr mu

data UnionMapLookupVal k v =
    UnionMapLookupValMissing !k
  | UnionMapLookupValOk !k !v !Changed
  deriving stock (Eq, Show)

lookupUnionMapLM :: (Coercible k Int, MonadState s m) => UnionMapLens s k v -> k -> m (UnionMapLookupVal k v)
lookupUnionMapLM l k = mayStateLens l $ \u ->
  case lookupUnionMap k u of
    UnionMapLookupResMissing x -> (UnionMapLookupValMissing x, Nothing)
    UnionMapLookupResFound x y mw -> (UnionMapLookupValOk x y (maybeChanged mw), mw)

lookupUnionMapM :: (Coercible k Int, MonadState (UnionMap k v) m) => k -> m (UnionMapLookupVal k v)
lookupUnionMapM = lookupUnionMapLM id

equivUnionMap :: Coercible k Int => UnionMap k v -> (UnionEquiv k, Maybe (UnionMap k v))
equivUnionMap u = foldl' go (emptyUnionEquiv, Nothing) (toListUnionMap u) where
  go (UnionEquiv fwd bwd, mw) (k, ue) =
    case ue of
      UnionEntryValue _ ->
        let fwd' = ILM.alter (Just . fromMaybe ILS.empty) k fwd
        in (UnionEquiv fwd' bwd, mw)
      UnionEntryLink _ ->
        case lookupUnionMap k (fromMaybe u mw) of
          UnionMapLookupResMissing _ -> error "impossible"
          UnionMapLookupResFound r _ mw' ->
            let fwd' = ILM.alter (Just . maybe (ILS.singleton k) (ILS.insert k)) r fwd
                bwd' = ILM.insert k r bwd
            in (UnionEquiv fwd' bwd', mw')

equivUnionMapLM :: (Coercible k Int, MonadState s m) => UnionMapLens s k v -> m (UnionEquiv k)
equivUnionMapLM l = mayStateLens l equivUnionMap

equivUnionMapM :: (Coercible k Int, MonadState (UnionMap k v) m) => m (UnionEquiv k)
equivUnionMapM = equivUnionMapLM id

compactUnionMap :: Coercible k Int => UnionMap k v -> (IntLikeMap k k, UnionMap k v)
compactUnionMap u = foldl' go (ILM.empty, u) (toListUnionMap u) where
  go mw@(m, w) (k, ue) =
    if ILM.member k m
      then mw
      else case ue of
        UnionEntryValue _ -> mw
        UnionEntryLink _ ->
          case traceUnionMap k w of
            UnionMapTraceResMissing _ -> error "impossible"
            UnionMapTraceResFound r _ kacc ->
              foldl' (\(m', w') j -> (ILM.insert j r m', UnionMap (ILM.insert j (UnionEntryLink r) (unUnionMap w')))) mw kacc

compactUnionMapLM :: (Coercible k Int, MonadState s m) => UnionMapLens s k v -> m (IntLikeMap k k)
compactUnionMapLM l = stateLens l compactUnionMap

compactUnionMapM :: (Coercible k Int, MonadState (UnionMap k v) m) => m (IntLikeMap k k)
compactUnionMapM = compactUnionMapLM id

canonicalizeUnionMap :: (Coercible k Int) => Traversal' v k -> UnionMap k v -> (IntLikeMap k k, UnionMap k v)
canonicalizeUnionMap t u = res where
  res = let (m, w) = compactUnionMap u in (m, UnionMap (ILM.fromList (toListUnionMap w >>= go m)))
  go m (k, ue) =
    case ue of
      UnionEntryLink _ -> []
      UnionEntryValue fk -> [(k, UnionEntryValue (over t (\j -> ILM.findWithDefault j j m) fk))]

canonicalizeUnionMapLM :: (Coercible k Int, MonadState s m) => UnionMapLens s k v -> Traversal' v k -> m (IntLikeMap k k)
canonicalizeUnionMapLM l t = stateLens l (canonicalizeUnionMap t)

canonicalizeUnionMapM :: (Coercible k Int, MonadState (UnionMap k v) m) => Traversal' v k -> m (IntLikeMap k k)
canonicalizeUnionMapM = canonicalizeUnionMapLM id

data UnionMapUpdateRes e k v r =
    UnionMapUpdateResMissing !k
  | UnionMapUpdateResEmbed !e
  | UnionMapUpdateResAdded !v !r !(UnionMap k v)
  | UnionMapUpdateResUpdated !k !v !r !(UnionMap k v)
  deriving stock (Eq, Show)

updateUnionMap :: (Coercible k Int, Eq k) => UnionMergeOne e v r -> k -> v -> UnionMap k v -> UnionMapUpdateRes e k v r
updateUnionMap g k v u@(UnionMap m) = goLookupK where
  goLookupK = case lookupUnionMap k u of
    UnionMapLookupResMissing kx ->
      if k == kx
        then goAdd
        else UnionMapUpdateResMissing kx
    UnionMapLookupResFound kr vr mu -> goMerge kr vr (fromMaybe u mu)
  goAdd =
    case g Nothing v of
      Left e -> UnionMapUpdateResEmbed e
      Right (r, vg) -> UnionMapUpdateResAdded vg r (UnionMap (ILM.insert k (UnionEntryValue v) m))
  goMerge kr vr (UnionMap mr) =
    case g (Just vr) v of
      Left e -> UnionMapUpdateResEmbed e
      Right (r, vg) -> UnionMapUpdateResUpdated kr vg r (UnionMap (ILM.insert kr (UnionEntryValue vg) mr))

data UnionMapUpdateVal e k v r =
    UnionMapUpdateValMissing !k
  | UnionMapUpdateValEmbed !e
  | UnionMapUpdateValAdded !v !r
  | UnionMapUpdateValUpdated !k !v !r
  deriving stock (Eq, Show)

updateUnionMapLM :: (Coercible k Int, Eq k, MonadState s m) => UnionMapLens s k v -> UnionMergeOne e v r -> k -> v -> m (UnionMapUpdateVal e k v r)
updateUnionMapLM l g k v = mayStateLens l $ \u ->
  case updateUnionMap g k v u of
  UnionMapUpdateResMissing x -> (UnionMapUpdateValMissing x, Nothing)
  UnionMapUpdateResEmbed e -> (UnionMapUpdateValEmbed e, Nothing)
  UnionMapUpdateResAdded y r w -> (UnionMapUpdateValAdded y r, Just w)
  UnionMapUpdateResUpdated x y r w -> (UnionMapUpdateValUpdated x y r, Just w)

updateUnionMapM :: (Coercible k Int, Eq k, MonadState (UnionMap k v) m) => UnionMergeOne e v r -> k -> v -> m (UnionMapUpdateVal e k v r)
updateUnionMapM = updateUnionMapLM id

data UnionMapMergeRes e k v r =
    UnionMapMergeResMissing !k
  | UnionMapMergeResEmbed !e
  | UnionMapMergeResMerged !k !v !r !(UnionMap k v)
  deriving stock (Eq, Show)

mergeOneUnionMap :: (Coercible k Int, Eq k) => UnionMergeOne e v r -> k -> k -> UnionMap k v -> UnionMapMergeRes e k v r
mergeOneUnionMap g k j u = goLookupK where
  doCompactCheck kr jr w jacc = doCompact kr w (if kr == jr then safeTail jacc else jr:jacc)
  doCompact kr w acc = UnionMap (foldl' (\m x -> ILM.insert x (UnionEntryLink kr) m) (unUnionMap w) acc)
  doRoot kr gv w = UnionMap (ILM.insert kr (UnionEntryValue gv) (unUnionMap w))
  goLookupK = case lookupUnionMap k u of
    UnionMapLookupResMissing kx -> if k == kx then goAssign else UnionMapMergeResMissing kx
    UnionMapLookupResFound kr kv mw -> goLookupJ kr kv (fromMaybe u mw)
  goAssign = case traceUnionMap j u of
    UnionMapTraceResMissing jx -> UnionMapMergeResMissing jx
    UnionMapTraceResFound jr jv jacc -> goMerge k Nothing jv (doCompactCheck k jr u jacc)
  goLookupJ kr kv w = case traceUnionMap j w of
    UnionMapTraceResMissing jx -> UnionMapMergeResMissing jx
    UnionMapTraceResFound jr jv jacc -> goMerge kr (Just kv) jv (doCompactCheck kr jr w jacc)
  goMerge kr mkv jv w1 =
    case g mkv jv of
      Left e -> UnionMapMergeResEmbed e
      Right (r, gv) ->
        let w2 = doRoot kr gv w1
        in UnionMapMergeResMerged kr gv r w2

data UnionMapMergeVal e k v r =
    UnionMapMergeValMissing !k
  | UnionMapMergeValEmbed !e
  | UnionMapMergeValMerged !k !v !r
  deriving stock (Eq, Show)

mergeOneUnionMapLM :: (Coercible k Int, Eq k, MonadState s m) => UnionMapLens s k v -> UnionMergeOne e v r -> k -> k -> m (UnionMapMergeVal e k v r)
mergeOneUnionMapLM l g k j = mayStateLens l $ \u ->
  case mergeOneUnionMap g k j u of
  UnionMapMergeResMissing x -> (UnionMapMergeValMissing x, Nothing)
  UnionMapMergeResEmbed e -> (UnionMapMergeValEmbed e, Nothing)
  UnionMapMergeResMerged x y r w -> (UnionMapMergeValMerged x y r, Just w)

mergeOneUnionMapM :: (Coercible k Int, Eq k, MonadState (UnionMap k v) m) => UnionMergeOne e v r -> k -> k -> m (UnionMapMergeVal e k v r)
mergeOneUnionMapM = mergeOneUnionMapLM id

mergeManyUnionMap :: (Traversable f, Coercible k Int, Eq k) => UnionMergeMany f e v r -> k -> f k -> UnionMap k v -> UnionMapMergeRes e k v r
mergeManyUnionMap g k js u = goLookupK where
  doCompactCheck kr jr w jacc = doCompact kr w (if kr == jr then safeTail jacc else jr:jacc)
  doCompact kr w acc = UnionMap (foldl' (\m x -> ILM.insert x (UnionEntryLink kr) m) (unUnionMap w) acc)
  doRoot kr gv w = UnionMap (ILM.insert kr (UnionEntryValue gv) (unUnionMap w))
  doTraceJ kr y = do
    w <- get
    case traceUnionMap y w of
      UnionMapTraceResMissing jx -> halt jx
      UnionMapTraceResFound jr jv jacc -> do
        put (doCompactCheck kr jr w jacc)
        pure jv
  doTraceJs kr = runDropM (traverse (doTraceJ kr) js)
  goLookupK = case lookupUnionMap k u of
    UnionMapLookupResMissing kx -> if k == kx then goAssign else UnionMapMergeResMissing kx
    UnionMapLookupResFound kr kv mw -> goLookupJs kr kv (fromMaybe u mw)
  goAssign =
    case doTraceJs k u of
      Left jx -> UnionMapMergeResMissing jx
      Right (jvs, w1) -> goMerge k Nothing jvs w1
  goLookupJs kr kv w =
    case doTraceJs kr w of
      Left jx -> UnionMapMergeResMissing jx
      Right (jvs, w1) -> goMerge kr (Just kv) jvs w1
  goMerge kr mkv jvs w1 =
    case g mkv jvs of
      Left e -> UnionMapMergeResEmbed e
      Right (r, gv) ->
        let w2 = doRoot kr gv w1
        in UnionMapMergeResMerged kr gv r w2

mergeManyUnionMapLM :: (Traversable f, Coercible k Int, Eq k, MonadState s m) => UnionMapLens s k v -> UnionMergeMany f e v r -> k -> f k -> m (UnionMapMergeVal e k v r)
mergeManyUnionMapLM l g k js = mayStateLens l $ \u ->
  case mergeManyUnionMap g k js u of
    UnionMapMergeResMissing x -> (UnionMapMergeValMissing x, Nothing)
    UnionMapMergeResEmbed e -> (UnionMapMergeValEmbed e, Nothing)
    UnionMapMergeResMerged x y r w -> (UnionMapMergeValMerged x y r, Just w)

mergeManyUnionMapM :: (Traversable f, Coercible k Int, Eq k, MonadState (UnionMap k v) m) => UnionMergeMany f e v r -> k -> f k -> m (UnionMapMergeVal e k v r)
mergeManyUnionMapM = mergeManyUnionMapLM id
