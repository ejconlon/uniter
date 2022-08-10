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

import Control.Monad.Except (MonadError (..))
import Control.Monad.State.Strict (MonadState, get, put)
import Data.Foldable (fold, foldl', toList)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro (Lens', Traversal', over)
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
  { ueFwd :: !(Map k (Set k))
  , ueBwd :: !(Map k k)
  } deriving stock (Eq, Show)

emptyUnionEquiv :: UnionEquiv k
emptyUnionEquiv = UnionEquiv Map.empty Map.empty

data UnionEntry k v =
    UnionEntryLink !k
  | UnionEntryValue !v
  deriving stock (Eq, Show, Ord, Functor, Foldable, Traversable)

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

newtype UnionMap k v = UnionMap { unUnionMap :: Map k (UnionEntry k v) }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

type UnionMapLens s k v = Lens' s (UnionMap k v)

emptyUnionMap :: UnionMap k v
emptyUnionMap = UnionMap Map.empty

sizeUnionMap :: UnionMap k v -> Int
sizeUnionMap = Map.size . unUnionMap

memberUnionMap :: Ord k => k -> UnionMap k v -> Bool
memberUnionMap k = Map.member k . unUnionMap

toListUnionMap :: UnionMap k v -> [(k, UnionEntry k v)]
toListUnionMap = Map.toList . unUnionMap

valuesUnionMap :: Ord k => UnionMap k v -> Map k v
valuesUnionMap = foldl' go Map.empty . toListUnionMap where
  go m (k, ue) =
    case ue of
      UnionEntryValue v -> Map.insert k v m
      _ -> m

data UnionMapAddRes k v =
    UnionMapAddResAdded !(UnionMap k v)
  | UnionMapAddResDuplicate
  deriving stock (Eq, Show)

addUnionMap :: Ord k => k -> v -> UnionMap k v -> UnionMapAddRes k v
addUnionMap k v (UnionMap m) =
  case Map.lookup k m of
    Nothing -> UnionMapAddResAdded (UnionMap (Map.insert k (UnionEntryValue v) m))
    Just _ -> UnionMapAddResDuplicate

data UnionMapAddVal =
    UnionMapAddValAdded
  | UnionMapAddValDuplicate
  deriving stock (Eq, Show)

addUnionMapLM :: (Ord k, MonadState s m) => UnionMapLens s k v -> k -> v -> m UnionMapAddVal
addUnionMapLM l k v = mayStateLens l $ \u ->
  case addUnionMap k v u of
    UnionMapAddResAdded w -> (UnionMapAddValAdded, Just w)
    UnionMapAddResDuplicate -> (UnionMapAddValDuplicate, Nothing)

addUnionMapM :: (Ord k, MonadState (UnionMap k v) m) => k -> v -> m UnionMapAddVal
addUnionMapM = addUnionMapLM id

data UnionMapTraceRes k v =
    UnionMapTraceResMissing !k
  | UnionMapTraceResFound !k !v ![k]
  deriving stock (Eq, Show)

traceUnionMap :: Ord k => k -> UnionMap k v -> UnionMapTraceRes k v
traceUnionMap k (UnionMap m) = go [] k where
  go !acc j =
    case Map.lookup j m of
      Nothing -> UnionMapTraceResMissing j
      Just link -> case link of
        UnionEntryLink kx -> go (j:acc) kx
        UnionEntryValue v -> UnionMapTraceResFound j v acc

data UnionMapLookupRes k v =
    UnionMapLookupResMissing !k
  | UnionMapLookupResFound !k !v !(Maybe (UnionMap k v))
  deriving stock (Eq, Show)

lookupUnionMap :: Ord k => k -> UnionMap k v -> UnionMapLookupRes k v
lookupUnionMap k u = case traceUnionMap k u of
  UnionMapTraceResMissing kx -> UnionMapLookupResMissing kx
  UnionMapTraceResFound kr vr acc ->
    let mu = if null acc then Nothing else Just (foldl' (\(UnionMap n) kx -> UnionMap (Map.insert kx (UnionEntryLink kr) n)) u (safeTail acc))
    in UnionMapLookupResFound kr vr mu

data UnionMapLookupVal k v =
    UnionMapLookupValMissing !k
  | UnionMapLookupValOk !k !v !Changed
  deriving stock (Eq, Show)

lookupUnionMapLM :: (Ord k, MonadState s m) => UnionMapLens s k v -> k -> m (UnionMapLookupVal k v)
lookupUnionMapLM l k = mayStateLens l $ \u ->
  case lookupUnionMap k u of
    UnionMapLookupResMissing x -> (UnionMapLookupValMissing x, Nothing)
    UnionMapLookupResFound x y mw -> (UnionMapLookupValOk x y (maybeChanged mw), mw)

lookupUnionMapM :: (Ord k, MonadState (UnionMap k v) m) => k -> m (UnionMapLookupVal k v)
lookupUnionMapM = lookupUnionMapLM id

equivUnionMap :: Ord k => UnionMap k v -> (UnionEquiv k, Maybe (UnionMap k v))
equivUnionMap u = foldl' go (emptyUnionEquiv, Nothing) (toListUnionMap u) where
  go (UnionEquiv fwd bwd, mw) (k, ue) =
    case ue of
      UnionEntryValue _ ->
        let fwd' = Map.alter (Just . fromMaybe Set.empty) k fwd
        in (UnionEquiv fwd' bwd, mw)
      UnionEntryLink _ ->
        case lookupUnionMap k (fromMaybe u mw) of
          UnionMapLookupResMissing _ -> error "impossible"
          UnionMapLookupResFound r _ mw' ->
            let fwd' = Map.alter (Just . maybe (Set.singleton k) (Set.insert k)) r fwd
                bwd' = Map.insert k r bwd
            in (UnionEquiv fwd' bwd', mw')

equivUnionMapLM :: (Ord k, MonadState s m) => UnionMapLens s k v -> m (UnionEquiv k)
equivUnionMapLM l = mayStateLens l equivUnionMap

equivUnionMapM :: (Ord k, MonadState (UnionMap k v) m) => m (UnionEquiv k)
equivUnionMapM = equivUnionMapLM id

compactUnionMap :: Ord k => UnionMap k v -> (Map k k, UnionMap k v)
compactUnionMap u = foldl' go (Map.empty, u) (toListUnionMap u) where
  go mw@(m, w) (k, ue) =
    if Map.member k m
      then mw
      else case ue of
        UnionEntryValue _ -> mw
        UnionEntryLink _ ->
          case traceUnionMap k w of
            UnionMapTraceResMissing _ -> error "impossible"
            UnionMapTraceResFound r _ kacc ->
              foldl' (\(m', w') j -> (Map.insert j r m', UnionMap (Map.insert j (UnionEntryLink r) (unUnionMap w')))) mw kacc

compactUnionMapLM :: (Ord k, MonadState s m) => UnionMapLens s k v -> m (Map k k)
compactUnionMapLM l = stateLens l compactUnionMap

compactUnionMapM :: (Ord k, MonadState (UnionMap k v) m) => m (Map k k)
compactUnionMapM = compactUnionMapLM id

canonicalizeUnionMap :: (Ord k) => Traversal' v k -> UnionMap k v -> (Map k k, UnionMap k v)
canonicalizeUnionMap t u = res where
  res = let (m, w) = compactUnionMap u in (m, UnionMap (Map.fromList (toListUnionMap w >>= go m)))
  go m (k, ue) =
    case ue of
      UnionEntryLink _ -> []
      UnionEntryValue fk -> [(k, UnionEntryValue (over t (\j -> Map.findWithDefault j j m) fk))]

canonicalizeUnionMapLM :: (Ord k, MonadState s m) => UnionMapLens s k v -> Traversal' v k -> m (Map k k)
canonicalizeUnionMapLM l t = stateLens l (canonicalizeUnionMap t)

canonicalizeUnionMapM :: (Ord k, MonadState (UnionMap k v) m) => Traversal' v k -> m (Map k k)
canonicalizeUnionMapM = canonicalizeUnionMapLM id

data UnionMapUpdateRes e k v r =
    UnionMapUpdateResMissing !k
  | UnionMapUpdateResEmbed !e
  | UnionMapUpdateResAdded !v !r !(UnionMap k v)
  | UnionMapUpdateResUpdated !k !v !r !(UnionMap k v)
  deriving stock (Eq, Show)

updateUnionMap :: (Ord k) => UnionMergeOne e v r -> k -> v -> UnionMap k v -> UnionMapUpdateRes e k v r
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
      Right (r, vg) -> UnionMapUpdateResAdded vg r (UnionMap (Map.insert k (UnionEntryValue v) m))
  goMerge kr vr (UnionMap mr) =
    case g (Just vr) v of
      Left e -> UnionMapUpdateResEmbed e
      Right (r, vg) -> UnionMapUpdateResUpdated kr vg r (UnionMap (Map.insert kr (UnionEntryValue vg) mr))

data UnionMapUpdateVal e k v r =
    UnionMapUpdateValMissing !k
  | UnionMapUpdateValEmbed !e
  | UnionMapUpdateValAdded !v !r
  | UnionMapUpdateValUpdated !k !v !r
  deriving stock (Eq, Show)

updateUnionMapLM :: (Ord k, MonadState s m) => UnionMapLens s k v -> UnionMergeOne e v r -> k -> v -> m (UnionMapUpdateVal e k v r)
updateUnionMapLM l g k v = mayStateLens l $ \u ->
  case updateUnionMap g k v u of
  UnionMapUpdateResMissing x -> (UnionMapUpdateValMissing x, Nothing)
  UnionMapUpdateResEmbed e -> (UnionMapUpdateValEmbed e, Nothing)
  UnionMapUpdateResAdded y r w -> (UnionMapUpdateValAdded y r, Just w)
  UnionMapUpdateResUpdated x y r w -> (UnionMapUpdateValUpdated x y r, Just w)

updateUnionMapM :: (Ord k, MonadState (UnionMap k v) m) => UnionMergeOne e v r -> k -> v -> m (UnionMapUpdateVal e k v r)
updateUnionMapM = updateUnionMapLM id

data UnionMapMergeRes e k v r =
    UnionMapMergeResMissing !k
  | UnionMapMergeResEmbed !e
  | UnionMapMergeResMerged !k !v !r !(UnionMap k v)
  deriving stock (Eq, Show)

mergeOneUnionMap :: (Ord k) => UnionMergeOne e v r -> k -> k -> UnionMap k v -> UnionMapMergeRes e k v r
mergeOneUnionMap g k j u = goLookupK where
  doCompactCheck kr jr w jacc = doCompact kr w (if kr == jr then safeTail jacc else jr:jacc)
  doCompact kr w acc = UnionMap (foldl' (\m x -> Map.insert x (UnionEntryLink kr) m) (unUnionMap w) acc)
  doRoot kr gv w = UnionMap (Map.insert kr (UnionEntryValue gv) (unUnionMap w))
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

mergeOneUnionMapLM :: (Ord k, MonadState s m) => UnionMapLens s k v -> UnionMergeOne e v r -> k -> k -> m (UnionMapMergeVal e k v r)
mergeOneUnionMapLM l g k j = mayStateLens l $ \u ->
  case mergeOneUnionMap g k j u of
  UnionMapMergeResMissing x -> (UnionMapMergeValMissing x, Nothing)
  UnionMapMergeResEmbed e -> (UnionMapMergeValEmbed e, Nothing)
  UnionMapMergeResMerged x y r w -> (UnionMapMergeValMerged x y r, Just w)

mergeOneUnionMapM :: (Ord k, MonadState (UnionMap k v) m) => UnionMergeOne e v r -> k -> k -> m (UnionMapMergeVal e k v r)
mergeOneUnionMapM = mergeOneUnionMapLM id

mergeManyUnionMap :: (Traversable f, Ord k) => UnionMergeMany f e v r -> k -> f k -> UnionMap k v -> UnionMapMergeRes e k v r
mergeManyUnionMap g k js u = goLookupK where
  doCompactCheck kr jr w jacc = doCompact kr w (if kr == jr then safeTail jacc else jr:jacc)
  doCompact kr w acc = UnionMap (foldl' (\m x -> Map.insert x (UnionEntryLink kr) m) (unUnionMap w) acc)
  doRoot kr gv w = UnionMap (Map.insert kr (UnionEntryValue gv) (unUnionMap w))
  doTraceJ kr y = do
    w <- get
    case traceUnionMap y w of
      UnionMapTraceResMissing jx -> throwError jx
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

mergeManyUnionMapLM :: (Traversable f, Ord k, MonadState s m) => UnionMapLens s k v -> UnionMergeMany f e v r -> k -> f k -> m (UnionMapMergeVal e k v r)
mergeManyUnionMapLM l g k js = mayStateLens l $ \u ->
  case mergeManyUnionMap g k js u of
    UnionMapMergeResMissing x -> (UnionMapMergeValMissing x, Nothing)
    UnionMapMergeResEmbed e -> (UnionMapMergeValEmbed e, Nothing)
    UnionMapMergeResMerged x y r w -> (UnionMapMergeValMerged x y r, Just w)

mergeManyUnionMapM :: (Traversable f, Ord k, MonadState (UnionMap k v) m) => UnionMergeMany f e v r -> k -> f k -> m (UnionMapMergeVal e k v r)
mergeManyUnionMapM = mergeManyUnionMapLM id
