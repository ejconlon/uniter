{-# LANGUAGE TemplateHaskell #-}

module Uniter.Exp where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)
import Data.Void (Void)
import Uniter.Align (Alignable (..), UnalignableError (..))
import Uniter.Core (FreeName (..), Node (..), Unitable (..), uniterAddNode, uniterAssignFree, uniterEmitEq, uniterFresh,
                    uniterIndexFree)

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

instance Unitable Void TyF Exp where
  unite = \case
    ExpConst -> uniterAddNode (Node TyConstF)
    ExpUseBind n -> uniterIndexFree (FreeName n)
    ExpDefBind n x y -> do
      zx <- unite x
      uniterAssignFree (FreeName n) zx (unite y)
    ExpTuple x y -> do
      zx <- unite x
      zy <- unite y
      uniterAddNode (Node (TyPairF zx zy))
    ExpFirst x -> do
      zx <- unite x
      v <- uniterFresh
      w <- uniterFresh
      zy <- uniterAddNode (Node (TyPairF v w))
      uniterEmitEq zx zy
    ExpSecond x -> do
      zx <- unite x
      v <- uniterFresh
      w <- uniterFresh
      zy <- uniterAddNode (Node (TyPairF v w))
      uniterEmitEq zx zy
