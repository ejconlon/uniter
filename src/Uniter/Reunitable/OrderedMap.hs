module Uniter.Reunitable.OrderedMap
  ( OrderedMap
  , empty
  , level
  , fromList
  , toList
  , snoc
  , snocAll
  , unsnoc
  , lookup
  , order
  ) where

import Data.Foldable (foldl')
import qualified Data.Foldable as F
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Prelude hiding (lookup)
import Uniter.Reunitable.Core (Index (..), Level (..), Pair (..), pairToTuple)

-- | A map where mutation is done through snocing and unsnocing
data OrderedMap k v = OrderedMap
  { osMap :: !(Map k (Seq (Pair Level v)))
  , osSeq :: !(Seq (Pair k v))
  } deriving stock (Eq, Show, Functor)

empty :: OrderedMap k v
empty = OrderedMap Map.empty Seq.empty

level :: OrderedMap k v -> Level
level (OrderedMap _ s) = Level (Seq.length s)

fromList :: Ord k => [(k, v)] -> OrderedMap k v
fromList = foldl' (uncurry . snoc) empty

toList :: OrderedMap k v -> [(k, v)]
toList (OrderedMap _ s) = fmap pairToTuple (F.toList s)

snoc :: Ord k => OrderedMap k v -> k -> v -> OrderedMap k v
snoc (OrderedMap m s) k v =
  let !cl = Level (Seq.length s)
      !m' = Map.insertWith (flip (<>)) k (Seq.singleton (Pair cl v)) m
      !s' = s :|> Pair k v
  in OrderedMap m' s'

snocAll :: Ord k => OrderedMap k v -> Seq (Pair k v) -> OrderedMap k v
snocAll (OrderedMap m s) ps =
  let !cl0 = Level (Seq.length s)
      (!m', _) = foldl' (\(n, cl) (Pair k v) -> (Map.insertWith (flip (<>)) k (Seq.singleton (Pair cl v)) n, succ cl)) (m, cl0) ps
      !s' = s <> ps
  in OrderedMap m' s'

alterUnsnoc :: Maybe (Seq x) -> Maybe (Seq x)
alterUnsnoc = \case
  Nothing -> Nothing
  Just xs -> case xs of
    Empty -> Nothing
    Empty :|> _ -> Nothing
    xs' :|> _ -> Just xs'

unsnoc :: Ord k => OrderedMap k v -> Maybe (OrderedMap k v, k, v)
unsnoc (OrderedMap m s) =
  case s of
    Empty -> Nothing
    s' :|> Pair k v ->
      let !m' = Map.alter alterUnsnoc k m
          !o' = OrderedMap m' s'
      in Just (o', k, v)

lookup :: Ord k => k -> OrderedMap k v -> Maybe (Index, v)
lookup k (OrderedMap m s) =
  flip fmap (Map.lookup k m) $ \case
    _ :|> Pair vl v ->
      let !cl = Level (Seq.length s)
          !i = Index (unLevel cl - unLevel vl - 1)
      in (i, v)
    Empty -> error "impossible"

order :: Ord k => OrderedMap k v -> Seq (Index, k, v)
order (OrderedMap _ s0) = go 0 Empty Set.empty s0 where
  go !ix !out !seen = \case
    Empty -> out
    s :|> (Pair k v) ->
      if Set.member k seen
        then go (ix + 1) out seen s
        else go (ix + 1) ((Index ix, k, v) :<| out) (Set.insert k seen) s
