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
  , lookupUnionMap
  , updateUnionMap
  , UnionMapEditRes (..)
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

data UnionMapLookupRes k v =
    UnionMapLookupResMissing !k
  | UnionMapLookupResUnchanged !k !v
  | UnionMapLookupResChanged !k !v !(UnionMap k v)
  deriving stock (Eq, Show)

lookupUnionMap :: Coercible k Int => k -> UnionMap k v -> UnionMapLookupRes k v
lookupUnionMap k u@(UnionMap m) = go [] k where
  go !acc j =
    case ILM.lookup j m of
      Nothing -> UnionMapLookupResMissing j
      Just link -> case link of
        UnionEntryLink kx -> go (j:acc) kx
        UnionEntryValue v ->
          case acc of
            _:_:_ ->
              let ur = foldl' (\(UnionMap n) ky -> UnionMap (ILM.insert ky (UnionEntryLink j) n)) u acc
              in UnionMapLookupResChanged k v ur
            _ -> UnionMapLookupResUnchanged j v

data UnionMapEditRes e k v =
    UnionMapEditResMissing !k
  | UnionMapEditResEmbed !e
  | UnionMapEditResUnchanged !k !v
  | UnionMapEditResChanged !k !v !(UnionMap k v)
  deriving stock (Eq, Show)

updateUnionMap :: (Coercible k Int, Eq k) => UnionMergeOne e v -> k -> v -> UnionMap k v -> UnionMapEditRes e k v
updateUnionMap g k v u@(UnionMap m) = go1 where
  go1 = case lookupUnionMap k u of
    UnionMapLookupResMissing kx ->
      if k == kx
        then UnionMapEditResChanged k v (UnionMap (ILM.insert k (UnionEntryValue v) m))
        else UnionMapEditResMissing kx
    UnionMapLookupResUnchanged kr vr -> go2 kr vr u
    UnionMapLookupResChanged kr vr ur -> go2 kr vr ur
  go2 kr vr (UnionMap mr) =
    case g vr v of
      Left e -> UnionMapEditResEmbed e
      Right vg -> UnionMapEditResChanged kr vg (UnionMap (ILM.insert kr (UnionEntryValue vg) mr))

mergeOneUnionMap :: UnionMergeOne e v -> k -> k -> UnionMapEditRes e k v
mergeOneUnionMap = undefined

mergeManyUnionMap :: UnionMergeMany e v -> k -> [k] -> UnionMapEditRes e k v
mergeManyUnionMap = undefined
