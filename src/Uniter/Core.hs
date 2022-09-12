{-# LANGUAGE UndecidableInstances #-}

module Uniter.Core
  ( UniqueId (..)
  , Node
  , Event (..)
  , Index (..)
  , Level (..)
  , TyVar (..)
  , TmVar (..)
  , Var (..)
  , ForAll (..)
  , Quant (..)
  , bareQuant
  , forAllQuant
  , Pair (..)
  , pairToTuple
  , tupleToPair
  , SpecTm (..)
  , SpecTmF (..)
  , recSpecTm
  , embedSpecTm
  , bindSpecTm
  , BoundTy (..)
  , BoundTyF (..)
  , recBoundTy
  , varBoundTy
  , embedBoundTy
  , SrcQuant
  , recSrcQuant
  , GenBinder (..)
  , GenQuant
  , recGenQuant
  , DummyAnnTm (..)
  , dummySpecTm
  ) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (toList)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Text (Text)

-- | A unique ID for generating distinct synthetic vars
-- Num instance is for literal parsing.
newtype UniqueId = UniqueId { unUniqueId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

-- | A 'Node' is a structure with all the holes filled with 'UniqueId's.
type Node g = g UniqueId

-- | An 'Event' can be processed
data Event g =
    EventAddNode !(Node g) !UniqueId
  | EventConstrainEq !UniqueId !UniqueId !UniqueId
  | EventNewMetaVar !(Maybe TyVar) !UniqueId
  | EventNewSkolemVar !TyVar !UniqueId

deriving instance Eq (Node g) => Eq (Event g)
deriving instance Ord (Node g) => Ord (Event g)
deriving instance Show (Node g) => Show (Event g)

-- | DeBruijn index
-- Num instance is for literal conversion
newtype Index = Index { unIndex :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Enum, Ord, Num)

-- | DeBruijn level
-- Num instance is for literal conversion
newtype Level = Level { unLevel :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Enum, Ord, Num)

-- | A type variable (or binder)
-- IsString instance is for literal conversion
newtype TyVar = TyVar { unTyVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | A term variable (or binder)
-- IsString instance is for literal conversion
newtype TmVar = TmVar { unTmVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | A term or type variable (or binder)
data Var =
    VarTy !TyVar
  | VarTm !TmVar
  deriving stock (Eq, Ord, Show)

-- | Something with universally quantified type variables
data ForAll b a = ForAll
  { forAllBinders :: !(Seq b)
  , forAllBody :: !a
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | An optionally-quantified type
data Quant (b :: Type) (a :: Type) =
    QuantBare !a
  | QuantForAll !(ForAll b a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

bareQuant :: (Recursive u, Base u ~ g) => u -> Quant b (BoundTy i g)
bareQuant = QuantBare . recBoundTy

forAllQuant :: Seq b -> BoundTy i g -> Quant b (BoundTy i g)
forAllQuant vs bt = QuantForAll (ForAll vs bt)

-- | A strict tuple
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

-- | The base functor for 'SpecTm'
data SpecTmF (h :: Type -> Type -> Type) (a :: Type) (r :: Type) =
    SpecTmEmbedF !(h a r)
  | SpecTmSpecF !(Seq a) !r
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor h => Bifunctor (SpecTmF h) where
  bimap f g = go where
    go = \case
      SpecTmEmbedF hr -> SpecTmEmbedF (bimap f g hr)
      SpecTmSpecF ps r -> SpecTmSpecF (fmap f ps) (g r)

instance Bifoldable h => Bifoldable (SpecTmF h) where
  bifoldr f g = go where
    go z = \case
      SpecTmEmbedF hr -> bifoldr f g z hr
      SpecTmSpecF ps r -> foldr f (g r z) (toList ps)

instance Bitraversable h => Bitraversable (SpecTmF h) where
  bitraverse f g = go where
    go = \case
      SpecTmEmbedF hr -> fmap SpecTmEmbedF (bitraverse f g hr)
      SpecTmSpecF ps r -> SpecTmSpecF <$> traverse f ps <*> g r

-- | A "specializing term" - contains the term constructors of 'h' but also
-- a constructor for type specialization (binds free type vars)
-- The second type parameter is the thing bound - usually this will
-- come out of unification as a metavariable ('SynVar') but will eventually be
-- traversed to replace with actual types.
-- 'h' here is usually the base functor of your final expression type.
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

-- | Recursively embeds a term with no specialization.
recSpecTm :: (Recursive v, Base v ~ h a) => v -> SpecTm h a
recSpecTm = cata embedSpecTm

-- | Embeds a meaningful constructor into the specialized term representation.
embedSpecTm :: h a (SpecTm h a) -> SpecTm h a
embedSpecTm = SpecTm . SpecTmEmbedF

bindSpecTmF :: Seq a -> SpecTm h a -> SpecTm h a
bindSpecTmF ps x = if Seq.null ps then x else SpecTm (SpecTmSpecF ps x)

-- | Binds the given ty vars, coalescing as it goes.
bindSpecTm :: Seq a -> SpecTm h a -> SpecTm h a
bindSpecTm ps s@(SpecTm x) = case x of
  SpecTmEmbedF _ -> bindSpecTmF ps s
  SpecTmSpecF ps' s' -> bindSpecTmF (ps <> ps') s'

-- | A "bound" type - includes all the given type constructors plus one
-- for type variables.
data BoundTyF (i :: Type) (g :: Type -> Type) (r :: Type) =
    BoundTyVarF !i
  | BoundTyEmbedF !(g r)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- shiftBoundTyF :: Functor g => (r -> r) -> Int -> BoundTyF Index g r -> BoundTyF Index g r
-- shiftBoundTyF f j = if j == 0 then id else go where
--   go gt =
--     case gt of
--       BoundTyVarF (Index i) -> BoundTyVarF (Index (i + j))
--       BoundTyEmbedF gr -> BoundTyEmbedF (fmap f gr)

newtype BoundTy (i :: Type) (g :: Type -> Type) = BoundTy { unBoundTy :: BoundTyF i g (BoundTy i g) }

deriving stock instance (Eq i, Eq (g (BoundTy i g))) => (Eq (BoundTy i g))
deriving stock instance (Ord i, Ord (g (BoundTy i g))) => (Ord (BoundTy i g))
deriving stock instance (Show i, Show (g (BoundTy i g))) => (Show (BoundTy i g))

-- shiftBoundTy :: Functor g => Int -> BoundTy Index g -> BoundTy Index g
-- shiftBoundTy j = go where
--   go = BoundTy . shiftBoundTyF go j . unBoundTy

varBoundTy :: i -> BoundTy i g
varBoundTy = BoundTy . BoundTyVarF

recBoundTy :: (Recursive u, Base u ~ g) => u -> BoundTy i g
recBoundTy = cata embedBoundTy

embedBoundTy :: g (BoundTy i g) -> BoundTy i g
embedBoundTy = BoundTy . BoundTyEmbedF

type instance Base (BoundTy i g) = BoundTyF i g

instance Functor g => Recursive (BoundTy i g) where
  project = unBoundTy

instance Functor g => Corecursive (BoundTy i g) where
  embed = BoundTy

type SrcQuant g = Quant TyVar (BoundTy Index g)

recSrcQuant :: (Recursive u, Base u ~ g) => u -> SrcQuant g
recSrcQuant = QuantBare . recBoundTy

data GenBinder = GenBinder
  { gbUnique :: !UniqueId
  , gbTyVar :: !(Maybe TyVar)
  } deriving stock (Eq, Ord, Show)

type GenQuant g = Quant GenBinder (BoundTy Index g)

recGenQuant :: (Recursive u, Base u ~ g) => u -> GenQuant g
recGenQuant = QuantBare . recBoundTy

data DummyAnnTm a r = DummyAnnTm
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor DummyAnnTm where
  bimap _ _ _ = DummyAnnTm

instance Bifoldable DummyAnnTm where
  bifoldr _ _ z _ = z

instance Bitraversable DummyAnnTm where
  bitraverse _ _ _ = pure DummyAnnTm

dummySpecTm :: SpecTm DummyAnnTm a
dummySpecTm = embedSpecTm DummyAnnTm
