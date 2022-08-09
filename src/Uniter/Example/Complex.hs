{-# LANGUAGE TemplateHaskell #-}

module Uniter.Example.Complex
  ( main
  ) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.These (These (..))
import Uniter (Alignable (..), UnalignableErr (..))
import Uniter.Reunitable.Class (MonadReuniter (..), Reunitable (..))
import Uniter.Reunitable.Core (Index, Quant, TmVar, embedSpecTm)

data Ty =
    TyInt
  | TyFun Ty Ty
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Ty

deriving stock instance Eq r => Eq (TyF r)
deriving stock instance Ord r => Ord (TyF r)
deriving stock instance Show r => Show (TyF r)

data Exp =
    ExpFree !TmVar
  | ExpInt !Int
  | ExpAdd Exp Exp
  | ExpIfZero Exp Exp Exp
  | ExpApp Exp Exp
  | ExpAbs !TmVar !(Maybe (Quant TyF)) !Exp
  | ExpLet !TmVar !(Maybe (Quant TyF)) !Exp !Exp
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Exp

deriving stock instance Eq r => Eq (ExpF r)
deriving stock instance Ord r => Ord (ExpF r)
deriving stock instance Show r => Show (ExpF r)

data AnnExp ty =
    AnnExpBound !Index
  | AnnExpFree !TmVar
  | AnnExpInt !Int
  | AnnExpAdd (AnnExp ty) (AnnExp ty)
  | AnnExpIfZero (AnnExp ty) (AnnExp ty) (AnnExp ty)
  | AnnExpApp (AnnExp ty) (AnnExp ty)
  | AnnExpAbs !TmVar !ty (AnnExp ty)
  | AnnExpLet !TmVar !ty (AnnExp ty) (AnnExp ty)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''AnnExp

deriving stock instance (Eq ty, Eq r) => Eq (AnnExpF ty r)
deriving stock instance (Ord ty, Ord r) => Ord (AnnExpF ty r)
deriving stock instance (Show ty, Show r) => Show (AnnExpF ty r)

instance Bifunctor AnnExpF where
  bimap = error "TODO"

instance Bifoldable AnnExpF where
  bifoldr = error "TODO"

instance Bitraversable AnnExpF where
  bitraverse = error "TODO"

instance Alignable UnalignableErr TyF where
  align x y =
    case (x, y) of
      (TyIntF, TyIntF) -> Right TyIntF
      (TyFunF xa xb, TyFunF ya yb) -> Right (TyFunF (These xa ya) (These xb yb))
      _ -> Left UnalignableErr

instance Reunitable ExpF AnnExpF TyF where
  reunite = \case
    ExpIntF c -> do
      x <- reuniterAddBaseTy TyIntF
      pure (x, embedSpecTm (AnnExpIntF c))
    ExpAddF mi mj -> do
      (i, si) <- mi
      (j, sj) <- mj
      x <- reuniterAddBaseTy TyIntF
      _ <- reuniterConstrainEq x i
      _ <- reuniterConstrainEq x j
      pure (x, embedSpecTm (AnnExpAddF si sj))
    ExpIfZeroF mi mj mk -> do
      (i, si) <- mi
      (j, sj) <- mj
      (k, sk) <- mk
      x <- reuniterAddBaseTy TyIntF
      y <- reuniterFreshVar
      _ <- reuniterConstrainEq x i
      _ <- reuniterConstrainEq y j
      _ <- reuniterConstrainEq y k
      pure (y, embedSpecTm (AnnExpIfZeroF si sj sk))
    ExpFreeF n ->
      let onFree = embedSpecTm (AnnExpFreeF n)
          onBound = embedSpecTm . AnnExpBoundF
      in reuniterResolveTmVar n onFree onBound
    ExpAppF mi mj -> do
      (i, si) <- mi
      (j, sj) <- mj
      x <- reuniterFreshVar
      y <- reuniterAddBaseTy (TyFunF j x)
      _ <- reuniterConstrainEq y i
      pure (x, embedSpecTm (AnnExpAppF si sj))
    ExpAbsF n _mq mi -> do
      -- TODO use optional type annotation
      x <- reuniterFreshVar
      (y, sy) <- reuniterBindTmVar n x mi
      pure (y, embedSpecTm (AnnExpAbsF n y sy))
    ExpLetF n _mq mi mj -> do
      -- TODO use optional type annotation
      (i, si) <- mi
      (y, sy) <- reuniterBindTmVar n i mj
      pure (y, embedSpecTm (AnnExpLetF n i si sy))

main :: IO ()
main = pure ()
