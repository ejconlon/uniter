module Uniter.Reunitable.Class
  ( MonadReuniter (..)
  , Reunitable (..)
  , reuniteTerm
  ) where

import Data.Bitraversable (Bitraversable)
import Data.Foldable (toList)
import Data.Functor.Foldable (Base, Recursive (..))
import Data.Kind (Type)
import Uniter.Core (Index, Node, SpecInit, SpecTm, SrcQuant, TmVar, TyVar, UniqueId, ContextTy)
import Uniter.Reunitable.Monad (ReuniterM, addBaseTy, bindTmVar, constrainEq, freshMetaVar, resolveTmVar)

-- | (There's really only one instance of this but we need to encapsulate the monad.)
class (Traversable g, Monad m) => MonadReuniter (g :: Type -> Type) (m :: Type -> Type) | m -> g where
  -- | Allocate an ID for the given base type.
  reuniterAddBaseTy :: Node g -> m UniqueId

  -- | Allocate a fresh ID.
  reuniterFreshVar :: Maybe TyVar -> m UniqueId

  -- | Emit equality constraints on two IDs.
  reuniterConstrainEq :: UniqueId -> UniqueId -> m UniqueId

  -- | Bind the type of the given term variable to a fresh metavar or a known polytype in the given scope
  -- Note that the polytype must be CLOSED.
  reuniterBindTmVar :: TmVar -> Maybe (SrcQuant g) -> m a -> m (ContextTy g, a)

  -- | Lookup the type and conversion of the given term variable in the current scope.
  reuniterResolveTmVar :: TmVar -> SpecInit h g -> (Index -> SpecInit h g) -> m (UniqueId, SpecInit h g)

  -- | Bind the type of the given term variable to a fresh metavar in the given scope.
  reuniterBindFreshTmVar :: TmVar -> m a -> m (ContextTy g, a)
  reuniterBindFreshTmVar v = reuniterBindTmVar v Nothing

  -- | Bind the type of the given term variable to a known polytype in the given scope
  -- Note that the polytype must be CLOSED.
  reuniterBindPolyTmVar :: TmVar -> SrcQuant g -> m a -> m (ContextTy g, a)
  reuniterBindPolyTmVar v = reuniterBindTmVar v . Just

  -- | Emit equality constraints on all IDs.
  reuniterConstrainAllEq :: Foldable t => UniqueId -> t UniqueId -> m UniqueId
  reuniterConstrainAllEq i0 js0 = go i0 (toList js0) where
    go !i = \case
      [] -> pure i
      j:js -> do
        k <- reuniterConstrainEq i j
        go k js

instance Traversable g => MonadReuniter g (ReuniterM g) where
  reuniterAddBaseTy = addBaseTy
  reuniterFreshVar = freshMetaVar
  reuniterConstrainEq = constrainEq
  reuniterResolveTmVar = resolveTmVar
  reuniterBindTmVar = bindTmVar

-- f, g, and h are base functors of some recursive structure
class (Traversable f, Traversable g, Bitraversable h) => Reunitable f h g | f -> h g where
  reunite :: MonadReuniter g m => f (m (UniqueId, SpecTm h g UniqueId UniqueId)) -> m (UniqueId, SpecInit h g)

reuniteTerm :: (Base t ~ f, Recursive t, Reunitable f h g, MonadReuniter g m) => t -> m (UniqueId, SpecInit h g)
reuniteTerm = cata reunite
