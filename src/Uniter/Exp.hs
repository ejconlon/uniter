{-# LANGUAGE TemplateHaskell #-}

module Uniter.Exp where

import Data.Functor.Foldable.TH (makeBaseFunctor)

data Exp =
    ExpConst
  | ExpUseBind !String
  | ExpDefBind !String Exp Exp
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
