{-# LANGUAGE UndecidableInstances #-}

module Uniter where

import Data.Coerce (Coercible)
import Data.Foldable (Foldable (foldMap'))
import Data.Hashable (Hashable)
import Data.Set (Set)
import qualified Data.Set as Set
import Overeasy.Expressions.Free (Free, pattern FreeEmbed, pattern FreePure)
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS

newtype Var = Var { unVar :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable)

class Monoid (n a) => SingletonMonoid n a where
  singletonMonoid :: a -> n a

instance Coercible v Int => SingletonMonoid IntLikeSet v where
  singletonMonoid = ILS.singleton

instance Ord a => SingletonMonoid Set a where
  singletonMonoid = Set.singleton

instance SingletonMonoid [] a where
  singletonMonoid = pure

-- Free monad with cached free variables
-- No longer a monad but good to keep around
data VarTree n f a =
    VarTreePure !a
  | VarTreeEmbed !(n a) !(f (VarTree n f a))

deriving stock instance (Eq a, Eq (n a), Eq (f (VarTree n f a))) => Eq (VarTree n f a)
deriving stock instance (Show a, Show (n a), Show (f (VarTree n f a))) => Show (VarTree n f a)

instance (Functor f, Functor n) => Functor (VarTree n f) where
  fmap f = go where
    go = \case
      VarTreePure a -> VarTreePure (f a)
      VarTreeEmbed na fv -> VarTreeEmbed (fmap f na) (fmap go fv)

modVarTree :: Functor f => (n a -> z a) -> VarTree n f a -> VarTree z f a
modVarTree f = go where
  go = \case
    VarTreePure a -> VarTreePure a
    VarTreeEmbed na fv -> VarTreeEmbed (f na) (fmap go fv)

class BindingTree g n where
  bindingTreeFree :: SingletonMonoid n a => g a -> n a

instance BindingTree (VarTree n f) n where
  bindingTreeFree = \case
    VarTreePure a -> singletonMonoid a
    VarTreeEmbed na _ -> na

class Functor f => BindingTreeLayer f n where
  bindingTreeLayer :: BindingTree g n => f (g a) -> n a

defaultBindingTreeLayer :: (Foldable f, BindingTree g n, SingletonMonoid n a) => f (g a) -> n a
defaultBindingTreeLayer = foldMap' bindingTreeFree

instance BindingTreeLayer f n => BindingTree (Free f) n where
  bindingTreeFree = \case
    FreePure a -> singletonMonoid a
    FreeEmbed fx -> bindingTreeLayer fx

toVarTree :: (BindingTreeLayer f n, SingletonMonoid n a) => Free f a -> VarTree n f a
toVarTree = \case
  FreePure a -> VarTreePure a
  FreeEmbed fx ->
    let fv = fmap toVarTree fx
        vs = bindingTreeLayer fv
    in VarTreeEmbed vs fv

fromVarTree :: Functor f => VarTree n f a -> Free f a
fromVarTree = \case
  VarTreePure a -> FreePure a
  VarTreeEmbed _ fv -> FreeEmbed (fmap fromVarTree fv)
