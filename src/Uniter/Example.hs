{-# LANGUAGE OverloadedStrings #-}

module Uniter.Example where

import Data.Void (Void)
import Uniter.Core (BoundId, FreeName (FreeName), Node (..), UniterM, uniterAddNode, uniterAssignFree, uniterEmitEq,
                    uniterFresh, uniterIndexFree)
import Uniter.Exp (Exp (..), TyF (..))

exampleLinear :: Exp
exampleLinear =
  let x1 = ExpDefBind "v1" ExpConst x2
      x2 = ExpDefBind "v2" (ExpTuple (ExpUseBind "v1") (ExpUseBind "v1")) x3
      x3 = ExpDefBind "v3" (ExpTuple (ExpSecond (ExpUseBind "v2")) (ExpFirst (ExpUseBind "v2"))) x4
      x4 = ExpUseBind "v3"
  in x1

exampleExponential :: Exp
exampleExponential =
  let x1 = ExpDefBind "v1" ExpConst x2
      x2 = ExpDefBind "v2" (ExpTuple (ExpUseBind "v1") (ExpUseBind "v1")) x3
      x3 = ExpDefBind "v3" (ExpTuple (ExpFirst (ExpUseBind "v2")) (ExpUseBind "v2")) x4
      x4 = ExpDefBind "v4" (ExpTuple (ExpSecond (ExpUseBind "v3")) (ExpTuple (ExpUseBind "v2") (ExpUseBind "v2"))) x5
      x5 = ExpUseBind "v4"
  in x1

expM :: Exp -> UniterM Void TyF BoundId
expM = \case
  ExpConst -> uniterAddNode (Node TyConstF)
  ExpUseBind n -> uniterIndexFree (FreeName n)
  ExpDefBind n x y -> do
    zx <- expM x
    uniterAssignFree (FreeName n) zx (expM y)
  ExpTuple x y -> do
    zx <- expM x
    zy <- expM y
    uniterAddNode (Node (TyPairF zx zy))
  ExpFirst x -> do
    zx <- expM x
    v <- uniterFresh
    w <- uniterFresh
    zy <- uniterAddNode (Node (TyPairF v w))
    uniterEmitEq zx zy
    pure zx
  ExpSecond x -> do
    zx <- expM x
    v <- uniterFresh
    w <- uniterFresh
    zy <- uniterAddNode (Node (TyPairF v w))
    uniterEmitEq zx zy
    pure zx
