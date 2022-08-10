module Uniter.Reunitable.Class
  ( MonadReuniter (..)
  , Reunitable (..)
  , reuniteTerm
  ) where

import Data.Bitraversable (Bitraversable)
import Data.Foldable (toList)
import Data.Functor.Foldable (Base, Recursive (..))
import Data.Kind (Type)
import Uniter.Core (BoundId, GenTy, Index, SpecTm, TmVar)
import Uniter.Reunitable.Monad (ReuniterM, addBaseTy, addGenTy, bindTmVar, constrainEq, freshVar, resolveTmVar)

-- | (There's really only one instance of this but we need to encapsulate the monad.)
class (Traversable g, Monad m) => MonadReuniter (g :: Type -> Type) (m :: Type -> Type) | m -> g where
  -- | Allocate an ID for the given base type.
  reuniterAddBaseTy :: g BoundId -> m BoundId

  -- | Allocate an ID for the given generalized type (recursing bottom-up on individual nodes and resolving vars as needed)
  reuniterAddGenTy :: GenTy g -> m BoundId

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

  -- | Bind the type of the given term variable to a fresh metavar in the given scope.
  reuniterBindFreshTmVar :: TmVar -> m a -> m a
  reuniterBindFreshTmVar tmv m = reuniterFreshVar >>= \b -> reuniterBindTmVar tmv b m

instance Traversable g => MonadReuniter g (ReuniterM g) where
  reuniterAddBaseTy = addBaseTy
  reuniterAddGenTy = addGenTy
  reuniterFreshVar = freshVar
  reuniterConstrainEq = constrainEq
  reuniterResolveTmVar = resolveTmVar
  reuniterBindTmVar = bindTmVar

-- f, g, and h are base functors of some recursive structure
class (Traversable f, Traversable g, Bitraversable h) => Reunitable f h g | f -> h g where
  reunite :: MonadReuniter g m => f (m (BoundId, SpecTm h BoundId)) -> m (BoundId, SpecTm h BoundId)

reuniteTerm :: (Base t ~ f, Recursive t, Reunitable f h g, MonadReuniter g m) => t -> m (BoundId, SpecTm h BoundId)
reuniteTerm = cata reunite
