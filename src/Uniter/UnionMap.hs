module Uniter.UnionMap
  ( UnionMergeOne
  , UnionMergeMany
  , foldUnionMergeMany
  , semigroupUnionMergeOne
  , semigroupUnionMergeMany
  , UnionMap
  , emptyUnionMap
  , sizeUnionMap
  , memberUnionMap
  , addUnionMap
  , UnionMapLookupRes (..)
  , lookupUnionMap
  , UnionMapUpdateRes (..)
  , updateUnionMap
  , UnionMapMergeRes (..)
  , mergeOneUnionMap
  , mergeManyUnionMap
  ) where

import Control.DeepSeq (NFData)
import Data.Coerce (Coercible)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import GHC.Generics (Generic)
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM

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

addUnionMap :: Coercible k Int => k -> v -> UnionMap k v -> Maybe (UnionMap k v)
addUnionMap k v (UnionMap m) =
  case ILM.lookup k m of
    Nothing -> Just (UnionMap (ILM.insert k (UnionEntryValue v) m))
    Just _ -> Nothing

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
  | UnionMapLookupResUnchanged !k !v
  | UnionMapLookupResChanged !k !v !(UnionMap k v)
  deriving stock (Eq, Show)

lookupUnionMap :: Coercible k Int => k -> UnionMap k v -> UnionMapLookupRes k v
lookupUnionMap k u = case traceUnionMap k u of
  UnionMapTraceResMissing kx -> UnionMapLookupResMissing kx
  UnionMapTraceResFound kr vr acc ->
    case acc of
      _:_:_ ->
        let ur = foldl' (\(UnionMap n) kx -> UnionMap (ILM.insert kx (UnionEntryLink kr) n)) u (tail acc)
        in UnionMapLookupResChanged kr vr ur
      _ -> UnionMapLookupResUnchanged kr vr

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
    UnionMapLookupResUnchanged kr vr -> go2 kr vr u
    UnionMapLookupResChanged kr vr ur -> go2 kr vr ur
  go2 kr vr (UnionMap mr) =
    case g vr v of
      Left e -> UnionMapUpdateResEmbed e
      Right vg -> UnionMapUpdateResUpdated kr vg (UnionMap (ILM.insert kr (UnionEntryValue vg) mr))

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
      goLookupJ kr kv (tail kacc)
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

mergeManyUnionMap :: (Coercible k Int, Eq k) => UnionMergeMany e v -> k -> [k] -> UnionMap k v -> UnionMapMergeRes e k v
mergeManyUnionMap = undefined
