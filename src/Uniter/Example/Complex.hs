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
import Uniter.Reunitable.Core (Index, TmVar, embedSpecTm)

main :: IO ()
main = pure ()

data Ty =
    TyInt
  | TyFun Ty Ty
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Ty

data Exp =
    ExpVar !TmVar
  | ExpInt !Int
  | ExpAdd Exp Exp
  | ExpIfZero Exp Exp Exp
  | ExpApp Exp Exp
  | ExpAbs !TmVar !Exp
  | ExpLet !TmVar !Exp !Exp
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Exp

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

instance Reunitable ExpF AnnExpF Ty TyF where
  reunite = \case
    ExpIntF c -> do
      x <- reuniterAddNode TyIntF
      pure (x, embedSpecTm (AnnExpIntF c))
    ExpAddF mi mj -> do
      (i, si) <- mi
      (j, sj) <- mj
      x <- reuniterAddNode TyIntF
      _ <- reuniterConstrainEq x i
      _ <- reuniterConstrainEq x j
      pure (x, embedSpecTm (AnnExpAddF si sj))
    ExpIfZeroF mi mj mk -> do
      (i, si) <- mi
      (j, sj) <- mj
      (k, sk) <- mk
      x <- reuniterAddNode TyIntF
      y <- reuniterFreshVar
      _ <- reuniterConstrainEq x i
      _ <- reuniterConstrainEq y j
      _ <- reuniterConstrainEq y k
      pure (y, embedSpecTm (AnnExpIfZeroF si sj sk))
    ExpVarF n ->
      let onFree = embedSpecTm (AnnExpFreeF n)
          onBound = embedSpecTm . AnnExpBoundF
      in reuniterResolveTmVar n onFree onBound
    ExpAppF mi mj -> do
      (i, si) <- mi
      (j, sj) <- mj
      x <- reuniterFreshVar
      y <- reuniterAddNode (TyFunF j x)
      _ <- reuniterConstrainEq y i
      pure (x, embedSpecTm (AnnExpAppF si sj))
    ExpAbsF n mi -> do
      (y, sy) <- reuniterBindFreshTmVar n mi
      pure (y, embedSpecTm (AnnExpAbsF n y sy))
    ExpLetF n mi mj -> do
      (i, si) <- mi
      (y, sy) <- reuniterBindTmVar n i mj
      pure (y, embedSpecTm (AnnExpLetF n i si sy))

-- data ExpUniteErr = UniteErr UnalignableErr TyF

-- newtype N a = N { unN :: ReaderT (FreeEnv Text) (Except ExpUniteErr) a }
--   deriving newtype (Functor, Applicative, Monad, MonadReader (FreeEnv Text), MonadError ExpUnitErr)

-- expTy :: Exp -> M Ty
-- expTy = undefined

-- annExp :: Exp -> M AnnExp
-- annExp = undefined
