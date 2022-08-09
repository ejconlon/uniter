module Uniter.Reunitable.Class
  ( MonadReuniter (..)
  , Reunitable (..)
  ) where

import Data.Bitraversable (Bitraversable)
import Data.Foldable (toList)
import Data.Functor.Foldable (Base, Recursive)
import Data.Kind (Type)
import Data.Void (Void)
import Uniter.Core (BoundId)
import Uniter.Reunitable.Core (Index, SpecTm, TmVar)
import Uniter.Reunitable.Monad (ReuniterM, addNode, addTerm, bindTmVar, constrainEq, freshVar, resolveTmVar)

-- | (There's really only one instance of this but we need to encapsulate the monad.)
class (Base u ~ g, Recursive u, Traversable g, Monad m) => MonadReuniter (u :: Type) (g :: Type -> Type) (m :: Type -> Type) | m -> u g where
  -- | Allocate an ID for the given 'Node'.
  reuniterAddNode :: g BoundId -> m BoundId

  -- | Allocate an ID for the given term (recursing bottom-up on individual nodes)
  reuniterAddTerm :: u -> m BoundId

  -- | Allocate a fresh ID.
  reuniterFreshVar :: m BoundId

  -- | Emit equality constraints on two IDs.
  reuniterConstrainEq :: BoundId -> BoundId -> m BoundId

  -- | Bind the type of the given term variable in the given scope.
  reuniterBindTmVar :: TmVar -> BoundId -> m a -> m a

  -- | Lookup the type and conversion of the given term variable in the current scope.
  reuniterResolveTmVar :: TmVar -> SpecTm h BoundId -> (Index -> SpecTm h BoundId) -> m (BoundId, SpecTm h BoundId)

  -- | Emit equality constraints on all IDs.
  reuniterConstrainAllEq :: Foldable t => BoundId -> t BoundId -> m BoundId
  reuniterConstrainAllEq i0 js0 = go i0 (toList js0) where
    go !i = \case
      [] -> pure i
      j:js -> do
        k <- reuniterConstrainEq i j
        go k js

  -- | Bind the type of the givne term variable to a fresh metavar in the given scope.
  reuniterBindFreshTmVar :: TmVar -> m a -> m a
  reuniterBindFreshTmVar tmv m = reuniterFreshVar >>= \b -> reuniterBindTmVar tmv b m

instance (Base u ~ g, Recursive u, Traversable g) => MonadReuniter u g (ReuniterM Void u g) where
  reuniterAddNode = addNode
  reuniterAddTerm = addTerm
  reuniterFreshVar = freshVar
  reuniterConstrainEq = constrainEq
  reuniterResolveTmVar = resolveTmVar
  reuniterBindTmVar = bindTmVar

-- f, g, and h are base functors of some recursive structure
class (Traversable f, Traversable g, Bitraversable h, Base u ~ g, Recursive u) => Reunitable f h u g | f -> h u g where
  reunite :: MonadReuniter u g m => f (m (BoundId, SpecTm h BoundId)) -> m (BoundId, SpecTm h BoundId)
