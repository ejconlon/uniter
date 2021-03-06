{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Uniter.Example
  ( Exp (..)
  , ExpF (..)
  , Ty (..)
  , TyF (..)
  , exampleLinear
  , exampleExponential
  , initialGraphExp
  , processGraphExp
  , writeDotExp
  , main
  ) where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)
import Data.These (These (..))
import Text.Pretty.Simple (pPrint)
import Uniter.Align (Alignable (..), UnalignableError (..))
import Uniter.Core (BoundId, Unitable (..), uniterAddNode, uniterEmitEq, uniterFresh)
import Uniter.FreeEnv (FreeEnv, FreeEnvMissingError (..), FreeName (..), emptyFreeEnv, insertFreeEnvM, lookupFreeEnvM)
import Uniter.Graph (GraphState (..))
import Uniter.Halt (halt)
import Uniter.Interface (RebindMap, initialGraph, processGraph)
import Uniter.Process (ProcessError)
import Uniter.Render (renderDot)

-- | A simple expression language with constants, vars, lets, tuples, and projections.
data Exp =
    ExpConst
  | ExpUseBind !Text
  | ExpDefBind !Text Exp Exp
  | ExpTuple Exp Exp
  | ExpFirst Exp
  | ExpSecond Exp
  deriving stock (Eq, Show)

-- TH can build us an expression functor ExpF to factor our the recursion.
makeBaseFunctor ''Exp

deriving stock instance Eq a => Eq (ExpF a)
deriving stock instance Show a => Show (ExpF a)

-- | Our expressions are either constant type or pairs of types.
data Ty =
    TyPair Ty Ty
  | TyConst
  deriving stock (Eq, Show)

-- Builds a type functor TyF
makeBaseFunctor ''Ty

deriving stock instance Eq a => Eq (TyF a)
deriving stock instance Show a => Show (TyF a)

instance Alignable UnalignableError TyF where
  -- Align our type functor in the natural way
  align TyConstF TyConstF = Right TyConstF
  align (TyPairF a b) (TyPairF c d) = Right (TyPairF (These a c) (These b d))
  align _ _ = Left UnalignableError

instance Unitable FreeEnv FreeEnvMissingError TyF ExpF where
  -- Inspect our expression functor and perform unification
  unite = \case
    ExpConstF -> uniterAddNode TyConstF
    ExpUseBindF n -> do
      b <- lookupFreeEnvM (FreeName n)
      maybe (halt (FreeEnvMissingError (FreeName n))) pure b
    ExpDefBindF n mx my -> do
      x <- mx
      insertFreeEnvM (FreeName n) x my
    ExpTupleF mx my -> do
      x <- mx
      y <- my
      uniterAddNode (TyPairF x y)
    ExpFirstF mx -> do
      x <- mx
      v <- uniterFresh
      w <- uniterFresh
      y <- uniterAddNode (TyPairF v w)
      uniterEmitEq x y
    ExpSecondF mx -> do
      x <- mx
      v <- uniterFresh
      w <- uniterFresh
      y <- uniterAddNode (TyPairF v w)
      uniterEmitEq x y

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

initialGraphExp :: Exp -> (BoundId, GraphState TyF)
initialGraphExp e =
  let (ea, gs) = initialGraph e emptyFreeEnv
  in case ea of
    Left _ -> error "impossible"
    -- otherwise there's just a bug in exp unite
    Right a -> (a, gs)

processGraphExp :: GraphState TyF -> Either (ProcessError UnalignableError) (RebindMap, GraphState TyF)
processGraphExp = processGraph

writeDotExp :: FilePath -> GraphState TyF -> IO ()
writeDotExp p = writeFile p . renderDot . gsBoundEnv

main :: IO ()
main = do
  process "linear" exampleLinear
  process "exponential" exampleExponential

process :: String -> Exp -> IO ()
process n e = do
  putStrLn n
  let (_, ig) = initialGraphExp e
  pPrint ig
  writeDotExp ("dot/" ++ n ++ "-initial.dot") ig
  case processGraphExp ig of
    Left _ -> pure ()
    Right (_, pg) -> do
      pPrint pg
      writeDotExp ("dot/" ++ n ++ "-processed.dot") pg
