{-# LANGUAGE UndecidableInstances #-}

module Uniter.Core
  ( UniqueId (..)
  , MetaVar (..)
  , SkolemVar (..)
  , SynVar (..)
  , synVarUniqueId
  , Node
  , Event (..)
  , Index (..)
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
  , GenTy (..)
  , GenTyF (..)
  , embedGenTy
  , varGenTy
  , closedGenTy
  , Quant (..)
  , forAllQuant
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
newtype UniqueId = UniqueId { unUniqueId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

-- | A meta variable representing an unknown type
newtype MetaVar = MetaVar { unMetaVar :: UniqueId }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

-- | A Skolem variable representing a type bound by a forall
data SkolemVar = SkolemVar
  { svUnique :: !UniqueId
  , svName :: !TyVar
  } deriving stock (Eq, Ord, Show)

-- | A "synthetic" variable representing one of the two var types
data SynVar =
    SynVarMeta !MetaVar
  | SynVarSkolem !SkolemVar
  deriving stock (Eq, Ord, Show)

synVarUniqueId :: SynVar -> UniqueId
synVarUniqueId = \case
  SynVarMeta (MetaVar u) -> u
  SynVarSkolem (SkolemVar u _) -> u

-- | A 'Node' is a structure with all the holes filled with 'SynVar's.
type Node g = g SynVar

data Event g =
    EventAddNode !(Node g) !SynVar
  | EventConstrainEq !SynVar !SynVar !SynVar
  | EventFreshVar !SynVar

deriving instance Eq (Node g) => Eq (Event g)
deriving instance Ord (Node g) => Ord (Event g)
deriving instance Show (Node g) => Show (Event g)

-- | DeBruijn index
newtype Index = Index { unIndex :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Enum, Ord)

-- | DeBruijn level
newtype Level = Level { unLevel :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Enum, Ord)

-- | A type variable (or binder)
newtype TyVar = TyVar { unTyVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | A term variable (or binder)
newtype TmVar = TmVar { unTmVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | A term or type variable (or binder)
data Var =
    VarTy !TyVar
  | VarTm !TmVar
  deriving stock (Eq, Ord, Show)

-- | Something with universally quantified type variables
data ForAll a = ForAll
  { forAllBinders :: !(Seq TyVar)
  , forAllBody :: !a
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

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

-- | Embeds a meaningful constructor into the specialized term representation.
embedSpecTm :: h a (SpecTm h a) -> SpecTm h a
embedSpecTm = SpecTm . SpecTmEmbedF

bindSpecTmF :: Seq (Pair TyVar a) -> SpecTm h a -> SpecTm h a
bindSpecTmF ps x = if Seq.null ps then x else SpecTm (SpecTmSpecF ps x)

-- | Binds the given ty vars, coalescing as it goes.
bindSpecTm :: Seq (Pair TyVar a) -> SpecTm h a -> SpecTm h a
bindSpecTm ps s@(SpecTm x) = case x of
  SpecTmEmbedF _ -> bindSpecTmF ps s
  SpecTmSpecF ps' s' -> bindSpecTmF (ps <> ps') s'

-- | A "generalized" type - includes all the given type constructors plus one
-- for type variables.
data GenTyF (g :: Type -> Type) (r :: Type) =
    GenTyVarF !TyVar
  | GenTyEmbedF !(g r)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype GenTy (g :: Type -> Type) = GenTy { unGenTy :: GenTyF g (GenTy g) }

deriving stock instance Eq (g (GenTy g)) => (Eq (GenTy g))
deriving stock instance Ord (g (GenTy g)) => (Ord (GenTy g))
deriving stock instance Show (g (GenTy g)) => (Show (GenTy g))

embedGenTy :: g (GenTy g) -> GenTy g
embedGenTy = GenTy . GenTyEmbedF

varGenTy :: TyVar -> GenTy g
varGenTy = GenTy . GenTyVarF

closedGenTy :: (Base u ~ g, Recursive u) => u -> GenTy g
closedGenTy = cata embedGenTy

type instance Base (GenTy g) = GenTyF g

instance Functor g => Recursive (GenTy g) where
  project = unGenTy

instance Functor g => Corecursive (GenTy g) where
  embed = GenTy

-- | A generalized type with optional quantification on the outside.
data Quant (g :: Type -> Type) =
    QuantBare !(GenTy g)
  | QuantForAll !(ForAll (GenTy g))

deriving stock instance Eq (g (GenTy g)) => (Eq (Quant g))
deriving stock instance Ord (g (GenTy g)) => (Ord (Quant g))
deriving stock instance Show (g (GenTy g)) => (Show (Quant g))

forAllQuant :: Seq TyVar -> GenTy g -> Quant g
forAllQuant tyvs gt = if Seq.null tyvs then QuantBare gt else QuantForAll (ForAll tyvs gt)

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
