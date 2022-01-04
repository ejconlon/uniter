module Uniter.UnionMap
  ( UnionEntry (..)
  , UnionMergeOne
  , UnionMergeMany
  , foldUnionMergeMany
  , semigroupUnionMergeOne
  , semigroupUnionMergeMany
  , UnionMap
  , emptyUnionMap
  , sizeUnionMap
  , memberUnionMap
  , toListUnionMap
  , valuesUnionMap
  , UnionMapAddRes (..)
  , addUnionMap
  , UnionMapAddVal (..)
  , addUnionMapS
  , UnionMapLookupRes (..)
  , lookupUnionMap
  , UnionMapLookupVal (..)
  , lookupUnionMapS
  , equivUnionMap
  , equivUnionMapS
  , UnionMapUpdateRes (..)
  , updateUnionMap
  , UnionMapUpdateVal (..)
  , updateUnionMapS
  , UnionMapMergeRes (..)
  , mergeOneUnionMap
  , UnionMapMergeVal (..)
  , mergeOneUnionMapS
  -- , mergeManyUnionMap
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, state)
import Data.Coerce (Coercible)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import GHC.Generics (Generic)
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS

safeTail :: [a] -> [a]
safeTail xs =
  case xs of
    [] -> xs
    _:ys -> ys

data UnionEntry k v =
    UnionEntryLink !k
  | UnionEntryValue !v
  deriving stock (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

type UnionMergeOne e v = v -> v -> Either e v
type UnionMergeMany e v = v -> [v] -> Either e v

foldUnionMergeMany :: UnionMergeOne e v -> UnionMergeMany e v
foldUnionMergeMany f = go where
  go v = \case
    [] -> Right v
    w:ws ->
      case f v w of
        Right u -> go u ws
        e@(Left _) -> e

semigroupUnionMergeOne :: Semigroup v => UnionMergeOne e v
semigroupUnionMergeOne v w = Right (v <> w)

semigroupUnionMergeMany :: Semigroup v => UnionMergeMany e v
semigroupUnionMergeMany v vs = Right (sconcat (v :| vs))

newtype UnionMap k v = UnionMap { unUnionMap :: IntLikeMap k (UnionEntry k v) }
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

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
  | UnionMapAddResExists
  deriving stock (Eq, Show)

addUnionMap :: Coercible k Int => k -> v -> UnionMap k v -> UnionMapAddRes k v
addUnionMap k v (UnionMap m) =
  case ILM.lookup k m of
    Nothing -> UnionMapAddResAdded (UnionMap (ILM.insert k (UnionEntryValue v) m))
    Just _ -> UnionMapAddResExists

data UnionMapAddVal =
    UnionMapAddValAdded
  | UnionMapAddValExists
  deriving stock (Eq, Show)

addUnionMapS :: Coercible k Int => k -> v -> State (UnionMap k v) UnionMapAddVal
addUnionMapS k v = state $ \u ->
  case addUnionMap k v u of
    UnionMapAddResAdded w -> (UnionMapAddValAdded, w)
    UnionMapAddResExists -> (UnionMapAddValExists, u)

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
        UnionEntryValue v -> UnionMapTraceResFound k v acc

data UnionMapLookupRes k v =
    UnionMapLookupResMissing !k
  | UnionMapLookupResOk !k !v !(Maybe (UnionMap k v))
  deriving stock (Eq, Show)

lookupUnionMap :: Coercible k Int => k -> UnionMap k v -> UnionMapLookupRes k v
lookupUnionMap k u = case traceUnionMap k u of
  UnionMapTraceResMissing kx -> UnionMapLookupResMissing kx
  UnionMapTraceResFound kr vr acc ->
    let tlAcc = safeTail acc
        mu = if null tlAcc then Nothing else Just (foldl' (\(UnionMap n) kx -> UnionMap (ILM.insert kx (UnionEntryLink kr) n)) u tlAcc)
    in UnionMapLookupResOk kr vr mu

data UnionMapLookupVal k v =
    UnionMapLookupValMissing !k
  | UnionMapLookupValOk !k !v
  deriving stock (Eq, Show)

lookupUnionMapS :: Coercible k Int => k -> State (UnionMap k v) (UnionMapLookupVal k v)
lookupUnionMapS k = state $ \u ->
  case lookupUnionMap k u of
    UnionMapLookupResMissing x -> (UnionMapLookupValMissing x, u)
    UnionMapLookupResOk x y mw -> (UnionMapLookupValOk x y, fromMaybe u mw)

equivUnionMap :: Coercible k Int => UnionMap k v -> ((IntLikeMap k (IntLikeSet k), IntLikeMap k k), UnionMap k v)
equivUnionMap u = foldl' go ((ILM.empty, ILM.empty), u) (toListUnionMap u) where
  go ((fwd, bwd), w) (k, ue) =
    case ue of
      UnionEntryValue _ ->
        let fwd' = ILM.alter (Just . fromMaybe ILS.empty) k fwd
        in ((fwd', bwd), w)
      UnionEntryLink _ ->
        case lookupUnionMap k w of
          UnionMapLookupResMissing _ -> error "impossible"
          UnionMapLookupResOk r _ mw ->
            let fwd' = ILM.alter (Just . maybe (ILS.singleton k) (ILS.insert k)) r fwd
                bwd' = ILM.insert k r bwd
                w' = fromMaybe w mw
            in ((fwd', bwd'), w')

equivUnionMapS :: Coercible k Int => State (UnionMap k v) (IntLikeMap k (IntLikeSet k), IntLikeMap k k)
equivUnionMapS = state equivUnionMap


data UnionMapUpdateRes e k v =
    UnionMapUpdateResMissing !k
  | UnionMapUpdateResEmbed !e
  | UnionMapUpdateResAdded !(UnionMap k v)
  | UnionMapUpdateResUpdated !k !v !(UnionMap k v)
  deriving stock (Eq, Show)

updateUnionMap :: (Coercible k Int, Eq k) => UnionMergeOne e v -> k -> v -> UnionMap k v -> UnionMapUpdateRes e k v
updateUnionMap g k v u@(UnionMap m) = go1 where
  go1 = case lookupUnionMap k u of
    UnionMapLookupResMissing kx ->
      if k == kx
        then UnionMapUpdateResAdded (UnionMap (ILM.insert k (UnionEntryValue v) m))
        else UnionMapUpdateResMissing kx
    UnionMapLookupResOk kr vr mu -> go2 kr vr (fromMaybe u mu)
  go2 kr vr (UnionMap mr) =
    case g vr v of
      Left e -> UnionMapUpdateResEmbed e
      Right vg -> UnionMapUpdateResUpdated kr vg (UnionMap (ILM.insert kr (UnionEntryValue vg) mr))

data UnionMapUpdateVal e k v =
    UnionMapUpdateValMissing !k
  | UnionMapUpdateValEmbed !e
  | UnionMapUpdateValAdded
  | UnionMapUpdateValUpdated !k !v
  deriving stock (Eq, Show)

updateUnionMapS :: (Coercible k Int, Eq k) => UnionMergeOne e v -> k -> v -> State (UnionMap k v) (UnionMapUpdateVal e k v)
updateUnionMapS g k v = state $ \u ->
  case updateUnionMap g k v u of
  UnionMapUpdateResMissing x -> (UnionMapUpdateValMissing x, u)
  UnionMapUpdateResEmbed e -> (UnionMapUpdateValEmbed e, u)
  UnionMapUpdateResAdded w -> (UnionMapUpdateValAdded, w)
  UnionMapUpdateResUpdated x y w -> (UnionMapUpdateValUpdated x y, w)

data UnionMapMergeRes e k v =
    UnionMapMergeResMissing !k
  | UnionMapMergeResEmbed !e
  | UnionMapMergeResOk !k !v !(UnionMap k v)
  deriving stock (Eq, Show)

mergeOneUnionMap :: (Coercible k Int, Eq k) => UnionMergeOne e v -> k -> k -> UnionMap k v -> UnionMapMergeRes e k v
mergeOneUnionMap g k j u@(UnionMap m) = goLookupK where
  goLookupK = case traceUnionMap k u of
    UnionMapTraceResMissing kx ->
      if k == kx
        then goAssign
        else UnionMapMergeResMissing kx
    UnionMapTraceResFound kr kv kacc ->
      -- tail to drop last link we don't need to update
      goLookupJ kr kv (safeTail kacc)
  goAssign = case traceUnionMap j u of
    UnionMapTraceResMissing jx -> UnionMapMergeResMissing jx
    UnionMapTraceResFound jr jv jacc ->
      let gu = UnionMap (ILM.insert k (UnionEntryValue jv) m)
      in goUpdate k jv (jr:jacc) gu
  goLookupJ kr kv kacc = case traceUnionMap j u of
    UnionMapTraceResMissing jx -> UnionMapMergeResMissing jx
    UnionMapTraceResFound jr jv jacc -> goMerge kr kv kacc jr jv jacc
  goMerge kr kv kacc jr jv jacc =
    if kr == jr
      then goUpdate kr kv kacc u
      else case g kv jv of
        Left e -> UnionMapMergeResEmbed e
        Right gv ->
          let gu = UnionMap (ILM.insert kr (UnionEntryValue gv) m)
          in goUpdate kr gv (jr:(kacc ++ jacc)) gu
  goUpdate kr gv acc gu =
    let fu = foldl' (\(UnionMap n) kx -> UnionMap (ILM.insert kx (UnionEntryLink kr) n)) gu acc
    in UnionMapMergeResOk kr gv fu

data UnionMapMergeVal e k v =
    UnionMapMergeValMissing !k
  | UnionMapMergeValEmbed !e
  | UnionMapMergeValOk !k !v
  deriving stock (Eq, Show)

mergeOneUnionMapS :: (Coercible k Int, Eq k) => UnionMergeOne e v -> k -> k -> State (UnionMap k v) (UnionMapMergeVal e k v)
mergeOneUnionMapS g k j = state $ \u ->
  case mergeOneUnionMap g k j u of
  UnionMapMergeResMissing x -> (UnionMapMergeValMissing x, u)
  UnionMapMergeResEmbed e -> (UnionMapMergeValEmbed e, u)
  UnionMapMergeResOk x y w -> (UnionMapMergeValOk x y, w)

-- mergeManyUnionMap :: (Coercible k Int, Eq k) => UnionMergeMany e v -> k -> [k] -> UnionMap k v -> UnionMapMergeRes e k v
-- mergeManyUnionMap = undefined
