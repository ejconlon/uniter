{-# LANGUAGE TemplateHaskell #-}

module Uniter.Scratch.Lang where

import Data.Bifunctor.TH (deriveBifunctor, deriveBifoldable, deriveBitraversable)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Uniter.Scratch.Types (TmName, MonoTy, PolyTy, PeelTy, TyVar, TmVar)

-- | Constructors (base types) in our language
data XConType = XConTypeInt
  deriving stock (Eq, Ord, Show)

-- | The types of our language
-- NOTE: Does NOT define forall types, those get tied into XPolyTy
data XTy a =
    XTyVar !a
  | XTyCon !XConType
  | XTyFun (XTy a) (XTy a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''XTy
deriving stock instance (Eq a, Eq r) => Eq (XTyF a r)
deriving stock instance (Ord a, Ord r) => Ord (XTyF a r)
deriving stock instance (Show a, Show r) => Show (XTyF a r)
deriveBifunctor ''XTyF
deriveBifoldable ''XTyF
deriveBitraversable ''XTyF

type XMonoTy = MonoTy XTyF TyVar
type XPolyTy = PolyTy XTyF TyVar
type XPeelTy = PeelTy XTyF TyVar

-- | The terms of our language
data XTm a =
    XTmVar !a
  | XTmInt !Int
  | XTmApp (XTm a) (XTm a)
  | XTmLam !TmName (XTm a)
  | XTmAnnLam !TmName !XPolyTy (XTm a)
  | XTmLet !TmName (XTm a) (XTm a)
  | XTmAnn (XTm a) !XPolyTy
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''XTm
deriving stock instance (Eq a, Eq r) => Eq (XTmF a r)
deriving stock instance (Ord a, Ord r) => Ord (XTmF a r)
deriving stock instance (Show a, Show r) => Show (XTmF a r)
deriveBifunctor ''XTmF
deriveBifoldable ''XTmF
deriveBitraversable ''XTmF

-- | A System-F-ified version of our language
data YTm a =
    YTmVar !a
  | YTmInt !Int
  | YTmApp (YTm a) (YTm a)
  | YTmTyApp (YTm a) !XPolyTy
  | YTmLam !TmVar !XPolyTy (YTm a)
  | YTmTyLam !TyVar !XPolyTy (YTm a)
  | YTmLet !TmVar !XPolyTy (YTm a) (YTm a)
  | YTmAnn (YTm a) !XPolyTy
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
