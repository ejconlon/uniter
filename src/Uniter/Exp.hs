{-# LANGUAGE TemplateHaskell #-}

module Uniter.Exp where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)
import Uniter.Align (Alignable (..), UnalignableError (..))
import Uniter.Core (Node (..), Unitable (..), uniterAddNode, uniterEmitEq, uniterFresh)
import Uniter.Env (FreeEnv, FreeEnvMissingError (..), FreeName (..), insertFreeEnvM, lookupFreeEnvM)
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

instance Unitable FreeEnv FreeEnvMissingError TyF Exp where
  unite = \case
    ExpConst -> uniterAddNode (Node TyConstF)
    ExpUseBind n -> do
      x <- lookupFreeEnvM (FreeName n)
      maybe (halt (FreeEnvMissingError (FreeName n))) pure x
    ExpDefBind n x y -> do
      zx <- unite x
      insertFreeEnvM (FreeName n) zx (unite y)
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
