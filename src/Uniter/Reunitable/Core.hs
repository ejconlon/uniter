module Uniter.Reunitable.Core
  ( Index (..)
  , Level (..)
  , Pair (..)
  , pairToTuple
  , pairFromTuple
  ) where

-- | DeBruijn index
newtype Index = Index { unIndex :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- | DeBruijn level
newtype Level = Level { unLevel :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- Strict tuple
data Pair k v = Pair
  { pairKey :: !k
  , pairVal :: !v
  } deriving stock (Eq, Show, Functor)

pairToTuple :: Pair k v -> (k, v)
pairToTuple (Pair k v) = (k, v)

pairFromTuple :: (k, v) -> Pair k v
pairFromTuple (k, v) = Pair k v
