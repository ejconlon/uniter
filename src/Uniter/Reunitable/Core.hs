{-# LANGUAGE UndecidableInstances #-}

module Uniter.Reunitable.Core
  ( Index (..)
  , Level (..)
  , TyVar (..)
  , TmVar (..)
  , Var (..)
  , ForAll (..)
  , Pair (..)
  , pairToTuple
  , tupleToPair
  , SpecTm (..)
  , SpecTmF (..)
  , embedSpecTm
  , bindSpecTm
  ) where
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (toList)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)

-- | DeBruijn index
newtype Index = Index { unIndex :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Enum, Ord)

-- | DeBruijn level
newtype Level = Level { unLevel :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Enum, Ord)

newtype TyVar = TyVar { unTyVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype TmVar = TmVar { unTmVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data Var =
    VarTy !TyVar
  | VarTm !TmVar
  deriving stock (Eq, Ord, Show)

data ForAll a = ForAll
  { forAllBinders :: !(Seq TyVar)
  , forAllBody :: !a
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Strict tuple
data Pair k v = Pair
  { pairKey :: !k
  , pairVal :: !v
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor Pair where
  bimap f g (Pair k v) = Pair (f k) (g v)

pairToTuple :: Pair k v -> (k, v)
pairToTuple (Pair k v) = (k, v)

tupleToPair :: (k, v) -> Pair k v
tupleToPair (k, v) = Pair k v

-- data SpecTmF (h :: Type -> Type) (a :: Type) (r :: Type) =
--     SpecTmEmbedF !(h r)
--   | SpecTmSpecF !(Seq (Pair TyVar a)) !r
--   deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- instance Functor h => Bifunctor (SpecTmF h) where
--   bimap f g = go where
--     go = \case
--       SpecTmEmbedF hr -> SpecTmEmbedF (fmap g hr)
--       SpecTmSpecF ps r -> SpecTmSpecF (fmap (fmap f) ps) (g r)

-- instance Foldable h => Bifoldable (SpecTmF h) where
--   bifoldr f g = go where
--     go z = \case
--       SpecTmEmbedF hr -> foldr g z hr
--       SpecTmSpecF ps r -> foldr f (g r z) (fmap pairVal (toList ps))

-- instance Traversable h => Bitraversable (SpecTmF h) where
--   bitraverse f g = go where
--     go = \case
--       SpecTmEmbedF hr -> fmap SpecTmEmbedF (traverse g hr)
--       SpecTmSpecF ps r -> SpecTmSpecF <$> traverse (traverse f) ps <*> g r

-- -- | A "specializing term" - contains the term constructors of 'h' but also
-- -- a constructor for type specialization (binds free type vars)
-- -- The second type parameter is the thing bound - usually this will
-- -- come out of unification as a metavariable ('BoundId') but should be
-- -- traversed to replace with actual types.
-- newtype SpecTm (h :: Type -> Type) (a :: Type) =
--   SpecTm { unSpecTm :: SpecTmF h a (SpecTm h a) }

-- instance Functor h => Functor (SpecTm h) where
--   fmap f = go where
--     go (SpecTm x) = SpecTm (bimap f go x)

-- instance Foldable h => Foldable (SpecTm h) where
--   foldr f = go where
--     go z (SpecTm x) = bifoldr f (flip go) z x

-- instance Traversable h => Traversable (SpecTm h) where
--   traverse f = go where
--     go (SpecTm x) = fmap SpecTm (bitraverse f go x)

-- deriving stock instance (Eq a, Eq (h (SpecTm h a))) => Eq (SpecTm h a)
-- deriving stock instance (Ord a, Ord (h (SpecTm h a))) => Ord (SpecTm h a)
-- deriving stock instance (Show a, Show (h (SpecTm h a))) => Show (SpecTm h a)

-- type instance Base (SpecTm h a) = SpecTmF h a

-- instance Functor h => Recursive (SpecTm h a) where
--   project = unSpecTm

-- instance Functor h => Corecursive (SpecTm h a) where
--   embed = SpecTm

-- embedSpecTm :: h (SpecTm h a) -> SpecTm h a
-- embedSpecTm = SpecTm . SpecTmEmbedF

-- bindSpecTm :: Seq (Pair TyVar a) -> SpecTm h a -> SpecTm h a
-- bindSpecTm ps s@(SpecTm x) = SpecTm $ case x of
--   SpecTmEmbedF _ -> SpecTmSpecF ps s
--   SpecTmSpecF ps' s' -> SpecTmSpecF (ps <> ps') s'

data SpecTmF (h :: Type -> Type -> Type) (a :: Type) (r :: Type) =
    SpecTmEmbedF !(h a r)
  | SpecTmSpecF !(Seq (Pair TyVar a)) !r
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor h => Bifunctor (SpecTmF h) where
  bimap f g = go where
    go = \case
      SpecTmEmbedF hr -> SpecTmEmbedF (bimap f g hr)
      SpecTmSpecF ps r -> SpecTmSpecF (fmap (fmap f) ps) (g r)

instance Bifoldable h => Bifoldable (SpecTmF h) where
  bifoldr f g = go where
    go z = \case
      SpecTmEmbedF hr -> bifoldr f g z hr
      SpecTmSpecF ps r -> foldr f (g r z) (fmap pairVal (toList ps))

instance Bitraversable h => Bitraversable (SpecTmF h) where
  bitraverse f g = go where
    go = \case
      SpecTmEmbedF hr -> fmap SpecTmEmbedF (bitraverse f g hr)
      SpecTmSpecF ps r -> SpecTmSpecF <$> traverse (traverse f) ps <*> g r

-- | A "specializing term" - contains the term constructors of 'h' but also
-- a constructor for type specialization (binds free type vars)
-- The second type parameter is the thing bound - usually this will
-- come out of unification as a metavariable ('BoundId') but should be
-- traversed to replace with actual types.
newtype SpecTm (h :: Type -> Type -> Type) (a :: Type) =
  SpecTm { unSpecTm :: SpecTmF h a (SpecTm h a) }

instance Bifunctor h => Functor (SpecTm h) where
  fmap f = go where
    go (SpecTm x) = SpecTm (bimap f go x)

instance Bifoldable h => Foldable (SpecTm h) where
  foldr f = go where
    go z (SpecTm x) = bifoldr f (flip go) z x

instance Bitraversable h => Traversable (SpecTm h) where
  traverse f = go where
    go (SpecTm x) = fmap SpecTm (bitraverse f go x)

deriving stock instance (Eq a, Eq (h a (SpecTm h a))) => Eq (SpecTm h a)
deriving stock instance (Ord a, Ord (h a (SpecTm h a))) => Ord (SpecTm h a)
deriving stock instance (Show a, Show (h a (SpecTm h a))) => Show (SpecTm h a)

type instance Base (SpecTm h a) = SpecTmF h a

instance Functor (h a) => Recursive (SpecTm h a) where
  project = unSpecTm

instance Functor (h a) => Corecursive (SpecTm h a) where
  embed = SpecTm

embedSpecTm :: h a (SpecTm h a) -> SpecTm h a
embedSpecTm = SpecTm . SpecTmEmbedF

bindSpecTm :: Seq (Pair TyVar a) -> SpecTm h a -> SpecTm h a
bindSpecTm ps s@(SpecTm x) = SpecTm $ case x of
  SpecTmEmbedF _ -> SpecTmSpecF ps s
  SpecTmSpecF ps' s' -> SpecTmSpecF (ps <> ps') s'
