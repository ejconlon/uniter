{-# LANGUAGE TemplateHaskell #-}

module Uniter.Exp where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)

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
