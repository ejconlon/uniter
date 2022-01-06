{-# LANGUAGE TemplateHaskell #-}

module Test.Uniter.Exp where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)
import Uniter.Align (Alignable (..), UnalignableError (..))
import Uniter.Core (Node (..), Unitable (..), uniterAddNode, uniterEmitEq, uniterFresh)
import Uniter.FreeEnv (FreeEnv, FreeEnvMissingError (..), FreeName (..), insertFreeEnvM, lookupFreeEnvM)
import Uniter.Halt (halt)

data Exp =
    ExpConst
  | ExpUseBind !Text
  | ExpDefBind !Text Exp Exp
  | ExpTuple Exp Exp
  | ExpFirst Exp
  | ExpSecond Exp
  deriving stock (Eq, Show)

makeBaseFunctor ''Exp

data Ty =
    TyPair Ty Ty
  | TyConst
  deriving stock (Eq, Show)

makeBaseFunctor ''Ty

instance Alignable UnalignableError TyF where
  align TyConstF TyConstF = Right TyConstF
  align (TyPairF a b) (TyPairF c d) = Right (TyPairF (a, c) (b, d))
  align _ _ = Left UnalignableError

instance Unitable FreeEnv FreeEnvMissingError TyF ExpF where
  unite = \case
    ExpConstF -> uniterAddNode (Node TyConstF)
    ExpUseBindF n -> do
      b <- lookupFreeEnvM (FreeName n)
      maybe (halt (FreeEnvMissingError (FreeName n))) pure b
    ExpDefBindF n mx my -> do
      x <- mx
      insertFreeEnvM (FreeName n) x my
    ExpTupleF mx my -> do
      x <- mx
      y <- my
      uniterAddNode (Node (TyPairF x y))
    ExpFirstF mx -> do
      x <- mx
      v <- uniterFresh
      w <- uniterFresh
      y <- uniterAddNode (Node (TyPairF v w))
      uniterEmitEq x y
    ExpSecondF mx -> do
      x <- mx
      v <- uniterFresh
      w <- uniterFresh
      y <- uniterAddNode (Node (TyPairF v w))
      uniterEmitEq x y
