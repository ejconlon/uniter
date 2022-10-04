{-# LANGUAGE UndecidableInstances #-}

-- | Core definitions
module Uniter.Core
  ( UniqueId (..)
  , Node
  , Index (..)
  , Level (..)
  , TyVar (..)
  , TmVar (..)
  , Var (..)
  , TyBinder (..)
  -- , TyBinding (..)
  -- , mkTyBinding
  , Event (..)
  , ForAll (..)
  , Quant (..)
  , BoundTy (..)
  , BoundTyF (..)
  , monoToBoundTy
  , varBoundTy
  , embedBoundTy
  , bareQuantTy
  , forAllQuantTy
  , PolyTy
  , monoToPolyTy
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

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.String (IsString (..))
import Data.Text (Text)

-- | A unique ID for generating distinct synthetic vars
-- Num instance is for literal conversion.
newtype UniqueId = UniqueId { unUniqueId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

-- | A 'Node' is a structure with all the holes filled with 'UniqueId's.
type Node g = g UniqueId

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

-- | An optional binder for a type
-- 'Nothing' means it cannot bind any vars, but still shifts indices.
newtype TyBinder = TyBinder { unTyBinder :: Maybe TyVar }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance IsString TyBinder where
  fromString = TyBinder . Just . fromString

-- data TyBinding a = TyBinding
--   { tbVar :: !TyVar
--   , tbVal :: !a
--   } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- mkTyBinding :: Applicative m => TyBinder -> m a -> m (Maybe (TyBinding a))
-- mkTyBinding tyb act = maybe (pure Nothing) (\tyv -> fmap (Just . TyBinding tyv) act) (unTyBinder tyb)

-- | An 'Event' can be processed to yield solution graphs
data Event g =
    EventAddNode !(Node g) !UniqueId
  | EventConstrainEq !UniqueId !UniqueId !UniqueId
  | EventNewMetaVar !TyBinder !UniqueId
  | EventNewSkolemVar !TyBinder !UniqueId
  -- | EventNewPolyVar !(Seq UniqueId) !UniqueId

deriving instance Eq (Node g) => Eq (Event g)
deriving instance Ord (Node g) => Ord (Event g)
deriving instance Show (Node g) => Show (Event g)

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

-- | A "bound" type - includes all the given type constructors plus one
-- for type variables.
data BoundTyF (g :: Type -> Type) (i :: Type) (r :: Type) =
    BoundTyVarF !i
    -- ^ A type variable
  | BoundTyEmbedF !(g r)
    -- ^ Just a normal type embedded here
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype BoundTy (g :: Type -> Type) (i :: Type) = BoundTy { unBoundTy :: BoundTyF g i (BoundTy g i) }

deriving stock instance (Eq i, Eq (g (BoundTy g i))) => (Eq (BoundTy g i))
deriving stock instance (Ord i, Ord (g (BoundTy g i))) => (Ord (BoundTy g i))
deriving stock instance (Show i, Show (g (BoundTy g i))) => (Show (BoundTy g i))

varBoundTy :: i -> BoundTy g i
varBoundTy = BoundTy . BoundTyVarF

monoToBoundTy :: (Recursive u, Base u ~ g) => u -> BoundTy g i
monoToBoundTy = cata embedBoundTy

embedBoundTy :: g (BoundTy g i) -> BoundTy g i
embedBoundTy = BoundTy . BoundTyEmbedF

type instance Base (BoundTy g i) = BoundTyF g i

instance Functor g => Recursive (BoundTy g i) where
  project = unBoundTy

instance Functor g => Corecursive (BoundTy g i) where
  embed = BoundTy

bareQuantTy :: (Recursive u, Base u ~ g) => u -> Quant b (BoundTy g i)
bareQuantTy = QuantBare . monoToBoundTy

forAllQuantTy :: Seq b -> BoundTy i g -> Quant b (BoundTy i g)
forAllQuantTy vs bt = QuantForAll (ForAll vs bt)

-- | A polytype
type PolyTy g = Quant TyBinder (BoundTy g Index)

monoToPolyTy :: (Recursive u, Base u ~ g) => u -> PolyTy g
monoToPolyTy = QuantBare . monoToBoundTy

-- | The base functor for 'SpecTm'
data SpecTmF (h :: Type -> Type -> Type) (g :: Type -> Type) (a :: Type) (i :: Type) (r :: Type) =
    SpecTmSpecF !(PolyTy g) !(Seq i) !r
    -- ^ A specialized term - (polytype, instantiations for all vars, term)
  | SpecTmEmbedF !(h a r)
    -- ^ Just a normal term embedded here
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
bindSpecTm :: PolyTy g -> Seq i -> SpecTm h g a i -> SpecTm h g a i
bindSpecTm a is s = SpecTm (SpecTmSpecF a is s)

-- | The initial reconstructed term - unsolved annotations and specializations
type SpecInit h g = SpecTm h g UniqueId UniqueId

-- | The final reconstructed term - annotated with polytypes and specializing with bound tys
type SpecFinal h g = Quant TyBinder (SpecTm h g (PolyTy g) (BoundTy g Index))

data DummyAnnTm a r = DummyAnnTm
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

dummySpecTm :: SpecTm DummyAnnTm g a i
dummySpecTm = embedSpecTm DummyAnnTm
