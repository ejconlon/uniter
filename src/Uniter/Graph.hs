{-# LANGUAGE UndecidableInstances #-}

module Uniter.Graph where

import Control.Monad.State.Strict (State)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Overeasy.Expressions.Free (Free, pattern FreeEmbed, pattern FreePure)
import Uniter.Core (BoundId, Node (..))

-- data Pair a b = Pair !a !b
--   deriving stock (Eq, Show, Ord, Generic)
--   deriving anyclass (Hashable, NFData)

-- newtype Part f = Part { unPart :: Free f BoundId }
-- deriving newtype instance Eq (f (Free f BoundId)) => Eq (Part f)
-- deriving stock instance Show (f (Free f BoundId)) => Show (Part f)

-- makePart :: (Recursive t, Base t ~ f) => t -> Part f
-- makePart = Part . go where
--   go = FreeEmbed . fmap go . project

data Join f =
    JoinRoot !(Node f)
  | JoinLeaf !BoundId
  | JoinEq !BoundId !BoundId
deriving stock instance Eq (f BoundId) => Eq (Join f)
deriving stock instance Show (f BoundId) => Show (Join f)

newtype BoundEnv f = BoundEnv { unBoundEnv :: Map BoundId (Join f) }
deriving newtype instance Eq (f BoundId) => Eq (BoundEnv f)
deriving stock instance Show (f BoundId) => Show (BoundEnv f)

resolveVar :: (Corecursive t, Base t ~ f, Traversable f) => BoundId -> BoundEnv f -> Either BoundId t
resolveVar v b@(BoundEnv m) =
  case Map.lookup v m of
    Nothing -> Left v
    Just j ->
      case j of
        JoinRoot x -> resolveNode x b
        JoinLeaf x -> resolveVar x b
        JoinEq _ _ -> Left v

resolveNode :: (Corecursive t, Base t ~ f, Traversable f) => Node f -> BoundEnv f -> Either BoundId t
resolveNode n b = fmap embed (traverse (`resolveVar` b) (unNode n))

-- lookupTerm :: (Recursive t, Base t ~ f, Traversable f) => t -> BoundEnv f -> Either (Node f) BoundId
-- lookupTerm = undefined

data GraphState f = GraphState
  { gsUnique :: !BoundId
  , gsBoundEnv :: !(BoundEnv f)
  }

type GraphM f = State (GraphState f)

-- insertTerm :: (Recursive t, Base t ~ f, Traversable f) => t -> GraphM f BoundId
-- insertTerm = undefined
