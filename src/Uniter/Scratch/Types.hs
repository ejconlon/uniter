{-# LANGUAGE UndecidableInstances #-}

module Uniter.Scratch.Types where

import Data.Text (Text)
import Data.String (IsString)
import Data.Kind (Type)
import Data.Sequence (Seq (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))

newtype UniqId = UniqId { unUniqId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

newtype TmName = TmName { unTmName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype TyName = TyName { unTyName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data TmVar =
    TmVarNamed !TmName
  | TmVarUniq !UniqId
  deriving stock (Eq, Ord, Show)

data TyVar =
    TyVarNamed !TyName
  | TyVarUniq !UniqId
  deriving stock (Eq, Ord, Show)

data ForAll (a :: Type) = ForAll
  { forAllBinders :: !(Seq TyVar)
  , forAllBody :: !a
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

noForAll :: a -> ForAll a
noForAll = ForAll Empty

-- Types should be functors (where the hole is in the free var position)
-- They should then have a base bifunctor (a la recursion-schemes) that gets
-- tied in the following definitions.

-- | A monotype ('tau' type)
newtype MonoTy (g :: Type -> Type -> Type) (a :: Type) =
  MonoTy { unMonoTy :: g a (MonoTy g a) }

deriving stock instance (Eq a, Eq (g a (MonoTy g a))) => Eq (MonoTy g a)
deriving stock instance (Ord a, Ord (g a (MonoTy g a))) => Ord (MonoTy g a)
deriving stock instance (Show a, Show (g a (MonoTy g a))) => Show (MonoTy g a)

instance Bifunctor g => Functor (MonoTy g) where
  fmap f = go where go = MonoTy . bimap f go . unMonoTy

instance Bifoldable g => Foldable (MonoTy g) where
  foldr f = go where go z = bifoldr f (flip go) z . unMonoTy

instance Bitraversable g => Traversable (MonoTy g) where
  traverse f = go where go = fmap MonoTy . bitraverse f go . unMonoTy

-- | A polytype ('sigma' type)
data PolyTy (g :: Type -> Type -> Type) (a :: Type) =
    PolyTyForAll !(ForAll (PeelTy g a))
  | PolyTyPeel !(PeelTy g a)

deriving stock instance (Eq a, Eq (g a (PolyTy g a))) => Eq (PolyTy g a)
deriving stock instance (Ord a, Ord (g a (PolyTy g a))) => Ord (PolyTy g a)
deriving stock instance (Show a, Show (g a (PolyTy g a))) => Show (PolyTy g a)

instance Bifunctor g => Functor (PolyTy g) where
  fmap f = goPoly where
    goPoly = \case
      PolyTyForAll fa -> PolyTyForAll (goForAll fa)
      PolyTyPeel p -> PolyTyPeel (goPeel p)
    goForAll = fmap goPeel
    goPeel = fmap f

instance Bifoldable g => Foldable (PolyTy g) where
  foldr f = goPoly where
    goPoly z = \case
      PolyTyForAll fa -> goForAll z fa
      PolyTyPeel p -> goPeel z p
    goForAll = foldr (flip goPeel)
    goPeel = foldr f

instance Bitraversable g => Traversable (PolyTy g) where
  traverse f = goPoly where
    goPoly = \case
      PolyTyForAll fa -> fmap PolyTyForAll (goForAll fa)
      PolyTyPeel p -> fmap PolyTyPeel (goPeel p)
    goForAll = traverse goPeel
    goPeel = traverse f

-- | A polytype with the first layer peeled off ('rho' type)
newtype PeelTy (g :: Type -> Type -> Type) (a :: Type) =
  PeelTy { unPeelTy :: g a (PolyTy g a) }

deriving stock instance (Eq a, Eq (g a (PolyTy g a))) => Eq (PeelTy g a)
deriving stock instance (Ord a, Ord (g a (PolyTy g a))) => Ord (PeelTy g a)
deriving stock instance (Show a, Show (g a (PolyTy g a))) => Show (PeelTy g a)

instance Bifunctor g => Functor (PeelTy g) where
  fmap f = goPeel where
    goPeel = PeelTy . bimap f goPoly . unPeelTy
    goPoly = fmap f

instance Bifoldable g => Foldable (PeelTy g) where
  foldr f = goPeel where
    goPeel z = bifoldr f goPoly z . unPeelTy
    goPoly = flip (foldr f)

instance Bitraversable g => Traversable (PeelTy g) where
  traverse f = go where go = fmap PeelTy . bitraverse f (traverse f) . unPeelTy

mkPolyTy :: Seq TyVar -> PeelTy g a -> PolyTy g a
mkPolyTy bs h =
  case bs of
    Empty -> PolyTyPeel h
    _ -> PolyTyForAll (ForAll bs h)

wrapPolyTy :: ForAll (PeelTy g a) -> PolyTy g a
wrapPolyTy (ForAll bs h) = mkPolyTy bs h
