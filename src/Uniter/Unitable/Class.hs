module Uniter.Unitable.Class
  ( MonadUniter (..)
  , Unitable (..)
  , uniteTerm
  ) where

import Data.Functor.Foldable (Base, Recursive (..))
import Uniter.Core (BoundId, TmVar, dummySpecTm)
import Uniter.Reunitable.Monad (ReuniterM, addBaseTy, bindTmVar, constrainEq, freshVar, resolveTmVar)

class (Traversable g, Monad m) => MonadUniter g m | m -> g where
  -- | Allocate an ID for the given base type.
  uniterAddBaseTy :: g BoundId -> m BoundId

  -- | Allocate a fresh ID.
  uniterFreshVar :: m BoundId

  -- | Emit equality constraints on two IDs.
  uniterConstrainEq :: BoundId -> BoundId -> m BoundId

  -- | Bind the type of the given term variable in the given scope.
  uniterBindTmVar :: TmVar -> BoundId -> m a -> m a

  -- | Lookup the type metavar of the given term variable in the given scope
  uniterResolveTmVar :: TmVar -> m BoundId

instance Traversable g => MonadUniter g (ReuniterM g) where
  uniterAddBaseTy = addBaseTy
  uniterFreshVar = freshVar
  uniterConstrainEq = constrainEq
  uniterBindTmVar = bindTmVar
  uniterResolveTmVar v = fmap fst (resolveTmVar v dummySpecTm (const dummySpecTm))

-- | Describes the unification process for a particular expression functor 'f':
-- 'f' is the expression type (typically representing EXPRESSIONS in your language)
-- 'g' is the alignable functor (typically representing the TYPE of your expression 'f') -
--    this will almost always implement 'Alignable'
-- 'm' is the effect type (typically some reader/state/error)
class (Traversable f, Traversable g) => Unitable f g | f -> g where
  -- | Inspects the expression functor, performing effects to
  -- allocate fresh unification vars, introduce equalities, and add nodes to the graph,
  -- returning the ID associated with this value.
  unite :: MonadUniter g m => f (m BoundId) -> m BoundId

-- | Perform unification bottom-up on a 'Recursive' term.
uniteTerm :: (Recursive t, Base t ~ f, Unitable f g, MonadUniter g m) => t -> m BoundId
uniteTerm = cata unite
