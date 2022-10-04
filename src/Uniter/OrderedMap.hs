module Uniter.OrderedMap
  ( OrderedMap
  , empty
  , level
  , fromList
  , toList
  , traverseAsList
  , snoc
  , snocAll
  , order
  , lookupByKey
  , lookupByIndex
  ) where

import Data.Foldable (foldl')
import qualified Data.Foldable as F
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Prelude hiding (lookup)
import Uniter.Core (Index (..), Level (..))

-- | A map where mutation is done through snocing
-- Meant for looking up string keys 'k' and getting a de Bruijn index back.
data OrderedMap k v = OrderedMap
  { osMap :: !(Map k (Level, v))
  , osSeq :: !(Seq (Maybe k, v))
  } deriving stock (Eq, Show, Functor)

empty :: OrderedMap k v
empty = OrderedMap Map.empty Seq.empty

level :: OrderedMap k v -> Level
level (OrderedMap _ s) = Level (Seq.length s)

fromList :: Ord k => [(Maybe k, v)] -> OrderedMap k v
fromList = foldl' (uncurry . snoc) empty

toList :: OrderedMap k v -> [(Maybe k, v)]
toList (OrderedMap _ s) = F.toList s

traverseAsList :: (Applicative m, Ord k) => (v -> m w) -> OrderedMap k v -> m (OrderedMap k w)
traverseAsList f = fmap fromList . traverse (traverse f) . toList

snoc :: Ord k => OrderedMap k v -> Maybe k -> v -> OrderedMap k v
snoc (OrderedMap m s) mk v =
  let !cl = Level (Seq.length s)
      !m' = maybe m (\k -> Map.insert k (cl, v) m) mk
      !s' = s :|> (mk, v)
  in OrderedMap m' s'

snocAll :: Ord k => OrderedMap k v -> Seq (Maybe k, v) -> OrderedMap k v
snocAll (OrderedMap m s) ps =
  let !cl0 = Level (Seq.length s)
      (!m', _) = foldl' (\(n, cl) (mk, v) -> (maybe m (\k -> Map.insert k (cl, v) n) mk, succ cl)) (m, cl0) ps
      !s' = s <> ps
  in OrderedMap m' s'

order :: Ord k => OrderedMap k v -> Seq (Index, Maybe k, v)
order (OrderedMap _ s0) = go 0 Empty Set.empty s0 where
  go !i !out !seen = \case
    Empty -> out
    s :|> sm ->
      case sm of
        (k, v) | not (Set.member k seen) ->
          go (i + 1) ((Index i, k, v) :<| out) (Set.insert k seen) s
        _ ->
          go (i + 1) out seen s

lookupByKey :: Ord k => k -> OrderedMap k v -> Maybe (Index, v)
lookupByKey k (OrderedMap m s) =
  flip fmap (Map.lookup k m) $ \(vl, v) ->
    let !cl = Level (Seq.length s)
        !i = Index (unLevel cl - unLevel vl - 1)
    in (i, v)

lookupByIndex :: Index -> OrderedMap k v -> Maybe (Maybe k, v)
lookupByIndex (Index i) (OrderedMap _ s) = Seq.lookup (Seq.length s - i - 1) s
