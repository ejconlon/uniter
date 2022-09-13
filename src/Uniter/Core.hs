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
  , Pair (..)
  , pairToTuple
  , tupleToPair
  , BoundTy (..)
  , BoundTyF (..)
  , recBoundTy
  , varBoundTy
  , embedBoundTy
  , bareQuantTy
  , forAllQuantTy
  , SrcQuant
  , recSrcQuant
  , GenQuant
  , recGenQuant
  , demoteQuant
  , SpecTm (..)
  , SpecTmF (..)
  , recSpecTm
  , embedSpecTm
  , bindSpecTm
  , SpecInit
  , SpecFinal
  , DummyAnnTm
  , dummySpecTm
  ) where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)

-- | A unique ID for generating distinct synthetic vars
-- Num instance is for literal conversion.
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
  -- | EventNewPolyVar !(Seq UniqueId) !UniqueId

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

-- | An optionally-quantified thing - for types it's a forall, for terms it's a type lambda
data Quant (b :: Type) (a :: Type) =
    QuantBare !a
  | QuantForAll !(ForAll b a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

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

-- | A "bound" type - includes all the given type constructors plus one
-- for type variables.
data BoundTyF (g :: Type -> Type) (i :: Type) (r :: Type) =
    BoundTyVarF !i
  | BoundTyEmbedF !(g r)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype BoundTy (g :: Type -> Type) (i :: Type) = BoundTy { unBoundTy :: BoundTyF g i (BoundTy g i) }

deriving stock instance (Eq i, Eq (g (BoundTy g i))) => (Eq (BoundTy g i))
deriving stock instance (Ord i, Ord (g (BoundTy g i))) => (Ord (BoundTy g i))
deriving stock instance (Show i, Show (g (BoundTy g i))) => (Show (BoundTy g i))

varBoundTy :: i -> BoundTy g i
varBoundTy = BoundTy . BoundTyVarF

recBoundTy :: (Recursive u, Base u ~ g) => u -> BoundTy g i
recBoundTy = cata embedBoundTy

embedBoundTy :: g (BoundTy g i) -> BoundTy g i
embedBoundTy = BoundTy . BoundTyEmbedF

type instance Base (BoundTy g i) = BoundTyF g i

instance Functor g => Recursive (BoundTy g i) where
  project = unBoundTy

instance Functor g => Corecursive (BoundTy g i) where
  embed = BoundTy

bareQuantTy :: (Recursive u, Base u ~ g) => u -> Quant b (BoundTy g i)
bareQuantTy = QuantBare . recBoundTy

forAllQuantTy :: Seq b -> BoundTy i g -> Quant b (BoundTy i g)
forAllQuantTy vs bt = QuantForAll (ForAll vs bt)

-- | A type signature for a term variable in the environment
type SrcQuant g = Quant TyVar (BoundTy g Index)

recSrcQuant :: (Recursive u, Base u ~ g) => u -> SrcQuant g
recSrcQuant = QuantBare . recBoundTy

-- | A polytype
type GenQuant g = Quant (Maybe TyVar) (BoundTy g Index)

recGenQuant :: (Recursive u, Base u ~ g) => u -> GenQuant g
recGenQuant = QuantBare . recBoundTy

-- | Convert a SrcQuant into a GenQuant (a weaker type; we already have all the optional info)
demoteQuant :: SrcQuant g -> GenQuant g
demoteQuant = \case
  QuantBare bt -> QuantBare bt
  QuantForAll (ForAll xs bt) -> QuantForAll (ForAll (fmap Just xs) bt)

-- | The base functor for 'SpecTm'
data SpecTmF (h :: Type -> Type -> Type) (g :: Type -> Type) (a :: Type) (i :: Type) (r :: Type) =
    SpecTmSpecF !(GenQuant g) !(Seq i) !r
  | SpecTmEmbedF !(h a r)
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance (Eq a, Eq i, Eq r, Eq (g (BoundTy g Index)), Eq (h a r)) => Eq (SpecTmF h g a i r)
deriving stock instance (Ord a, Ord i, Ord r, Ord (g (BoundTy g Index)), Ord (h a r)) => Ord (SpecTmF h g a i r)
deriving stock instance (Show a, Show i, Show r, Show (g (BoundTy g Index)), Show (h a r)) => Show (SpecTmF h g a i r)

-- | A "specializing term" - contains the term constructors of 'h' but also
-- a constructor for type specialization (binds free type vars)
-- 'h' is the base Bifunctor of your final expression type.
-- 'g' is the base Functor of the type type.
-- 'a' is the type annotation
-- 'i' is what specializes terms
-- These last two will come out of unification as a metavariables ('UniqueId') but will
-- eventually be traversed to replace with actual types.
newtype SpecTm (h :: Type -> Type -> Type) (g :: Type -> Type) (a :: Type) (i :: Type) =
  SpecTm { unSpecTm :: SpecTmF h g a i (SpecTm h g a i) }

deriving stock instance (Eq a, Eq i, Eq (g (BoundTy g Index)), Eq (h a (SpecTm h g a i))) => Eq (SpecTm h g a i)
deriving stock instance (Ord a, Ord i, Ord (g (BoundTy g Index)), Ord (h a (SpecTm h g a i))) => Ord (SpecTm h g a i)
deriving stock instance (Show a, Show i, Show (g (BoundTy g Index)), Show (h a (SpecTm h g a i))) => Show (SpecTm h g a i)

type instance Base (SpecTm h g a i) = SpecTmF h g a i

instance Functor (h a) => Recursive (SpecTm h g a i) where
  project = unSpecTm

instance Functor (h a) => Corecursive (SpecTm h g a i) where
  embed = SpecTm

-- | Recursively embeds a term with no specialization.
recSpecTm :: (Recursive v, Base v ~ h a) => v -> SpecTm h g a i
recSpecTm = cata embedSpecTm

-- | Embeds a meaningful constructor into the specialized term representation.
embedSpecTm :: h a (SpecTm h g a i) -> SpecTm h g a i
embedSpecTm = SpecTm . SpecTmEmbedF

-- | Specializes a term.
bindSpecTm :: GenQuant g -> Seq i -> SpecTm h g a i -> SpecTm h g a i
bindSpecTm a is s = SpecTm (SpecTmSpecF a is s)

-- | The initial reconstructed term - unsolved annotations and specializations
type SpecInit h g = SpecTm h g UniqueId UniqueId

-- | The final reconstructed term - annotated with polytypes and specializing with bound tys
type SpecFinal h g = Quant (Maybe TyVar) (SpecTm h g (GenQuant g) (BoundTy g Index))

data DummyAnnTm a r = DummyAnnTm
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

dummySpecTm :: SpecTm DummyAnnTm g a i
dummySpecTm = embedSpecTm DummyAnnTm
