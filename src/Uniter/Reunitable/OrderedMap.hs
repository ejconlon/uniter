module Uniter.Reunitable.OrderedMap
  ( Index (..)
  , OrderedMap
  , empty
  , fromList
  , toList
  , snoc
  , unsnoc
  , lookup
  ) where

import Data.Map.Strict (Map)
import Data.Sequence (Seq (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Foldable (foldl')
import qualified Data.Foldable as F
import Prelude hiding (lookup)
import Uniter.Reunitable.Core (Level (..), Pair (..), Index (..), pairToTuple)
import qualified Data.Set as Set

-- | A map where mutation is done through snocing and unsnocing
data OrderedMap k v = OrderedMap
  { osMap :: !(Map k (Seq (Pair Level v)))
  , osSeq :: !(Seq (Pair k v))
  } deriving stock (Eq, Show, Functor)

empty :: OrderedMap k v
empty = OrderedMap Map.empty Seq.empty

fromList :: Ord k => [(k, v)] -> OrderedMap k v
fromList = foldl' (uncurry . snoc) empty

toList :: OrderedMap k v -> [(k, v)]
toList (OrderedMap _ s) = fmap pairToTuple (F.toList s)

snoc :: Ord k => OrderedMap k v -> k -> v -> OrderedMap k v
snoc (OrderedMap m s) k v =
  let !cl = Level (Seq.length s)
      !m' = Map.insertWith (<>) k (Seq.singleton (Pair cl v)) m
      !s' = s :|> Pair k v
  in OrderedMap m' s'

alterUnsnoc :: Maybe (Seq x) -> Maybe (Seq x)
alterUnsnoc = \case
  Nothing -> Nothing
  Just xs -> case xs of
    Empty -> Nothing
    Empty :|> _ -> Nothing
    xs' :|> _ -> Just xs'

unsnoc :: Ord k => OrderedMap k v -> Maybe (OrderedMap k v, v)
unsnoc (OrderedMap m s) =
  case s of
    Empty -> Nothing
    s' :|> Pair k v ->
      let !m' = Map.alter alterUnsnoc k m
          !o' = OrderedMap m' s'
      in Just (o', v)

lookup :: Ord k => k -> OrderedMap k v -> Maybe (Index, v)
lookup k (OrderedMap m s) =
  flip fmap (Map.lookup k m) $ \case
    _ :|> Pair vl v ->
      let !cl = Level (Seq.length s)
          !i = Index (unLevel cl - unLevel vl)
      in (i, v)
    Empty -> error "impossible"

order :: Ord k => OrderedMap k v -> Seq (Pair k v)
order (OrderedMap _ s0) = go Empty Set.empty s0 where
  go !out !seen = \case
    Empty -> out
    s :|> p@(Pair k _) ->
      if Set.member k seen
        then go out seen s
        else go (p :<| out) (Set.insert k seen) s
