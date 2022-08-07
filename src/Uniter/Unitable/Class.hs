module Uniter.Unitable.Class
  ( Unitable (..)
  , uniteTerm
  ) where

import Data.Functor.Foldable (Base, Recursive (..))
import Uniter.Core (BoundId)
import Uniter.Monad (UniterT)

-- | Describes the unification process for a particular expression functor 'f':
-- 'f' is the expression type (typically representing EXPRESSIONS in your language)
-- 'g' is the alignable functor (typically representing the TYPE of your expression 'f') -
--    this will almost always implement 'Alignable'
-- 'm' is the effect type (typically some reader/state/error)
class (Traversable f, Traversable g, Monad m) => Unitable f g m | f -> g m where
  -- | Inspects the expression functor, performing effects to
  -- allocate fresh unification vars, introduce equalities, and add nodes to the graph,
  -- returning the ID associated with this value.
  unite :: f (UniterT g m BoundId) -> UniterT g m BoundId

-- | Perform unification bottom-up on a 'Recursive' term.
uniteTerm :: (Recursive t, Base t ~ f, Unitable f g m) => t -> UniterT g m BoundId
uniteTerm = cata unite
