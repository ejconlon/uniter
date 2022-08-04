{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A simple example following "Efficient Functional Unification and Substitution"
-- by Dijkstra, Middelkoop, and Swierstra.
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
import Uniter.FreeEnv (FreeEnv, FreeEnvMissingError (..), emptyFreeEnv, insertFreeEnvM, lookupFreeEnvM)
import Uniter.Graph (GraphState (..), graphResolveVar)
import Uniter.Halt (halt)
import Uniter.Interface (RebindMap, initialGraph, processGraph)
import Uniter.Process (ProcessError)
import Uniter.Render (renderDot)
import Control.Exception (throwIO)
import Control.Monad.State.Strict (evalState)

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

instance Unitable (FreeEnv Text) (FreeEnvMissingError Text) TyF ExpF where
  -- Inspect our expression functor and perform unification
  unite = \case
    ExpConstF ->
      -- constants have constant type
      uniterAddNode TyConstF
    ExpUseBindF n -> do
      -- vars require we lookup the binding, throwing if it's not there
      b <- lookupFreeEnvM n
      maybe (halt (FreeEnvMissingError n)) pure b
    ExpDefBindF n mx my -> do
      -- first get the type of the argument of the let
      x <- mx
      -- then bind the var, and return the type of the body with it bound
      insertFreeEnvM n x my
    ExpTupleF mx my -> do
      -- find the type of both arguments
      x <- mx
      y <- my
      -- and tuple them together!
      uniterAddNode (TyPairF x y)
    ExpFirstF mx -> do
      -- find the type of the argument
      x <- mx
      -- and ensure it unifies with a tuple type
      v <- uniterFresh
      w <- uniterFresh
      y <- uniterAddNode (TyPairF v w)
      _ <- uniterEmitEq x y
      -- fst returns the type of the first element of the pair
      pure v
    ExpSecondF mx -> do
      -- just like first, find the type of the argument
      x <- mx
      -- and again ensure it unifies with a tuple type
      v <- uniterFresh
      w <- uniterFresh
      y <- uniterAddNode (TyPairF v w)
      _ <- uniterEmitEq x y
      -- BUT return something different here:
      -- snd returns the type of the second element of the pair
      pure w

-- | A small example of type (C, C)
exampleLinear :: Exp
exampleLinear =
  let x1 = ExpDefBind "v1" ExpConst x2
      x2 = ExpDefBind "v2" (ExpTuple (ExpUseBind "v1") (ExpUseBind "v1")) x3
      x3 = ExpDefBind "v3" (ExpTuple (ExpSecond (ExpUseBind "v2")) (ExpFirst (ExpUseBind "v2"))) x4
      x4 = ExpUseBind "v3"
  in x1

-- | An example that can easily grow larger: ((C, C), ((C, C), (C, C)))
exampleExponential :: Exp
exampleExponential =
  let x1 = ExpDefBind "v1" ExpConst x2
      x2 = ExpDefBind "v2" (ExpTuple (ExpUseBind "v1") (ExpUseBind "v1")) x3
      x3 = ExpDefBind "v3" (ExpTuple (ExpFirst (ExpUseBind "v2")) (ExpUseBind "v2")) x4
      x4 = ExpDefBind "v4" (ExpTuple (ExpSecond (ExpUseBind "v3")) (ExpTuple (ExpUseBind "v2") (ExpUseBind "v2"))) x5
      x5 = ExpUseBind "v4"
  in x1

initialGraphExp :: Exp -> IO (BoundId, GraphState TyF)
initialGraphExp expr =
  let (ea, gs) = initialGraph expr emptyFreeEnv
  in case ea of
    Left err -> throwIO err
    Right a -> pure (a, gs)

processGraphExp :: GraphState TyF -> Either (ProcessError UnalignableError) (RebindMap, GraphState TyF)
processGraphExp = processGraph

resolveTy :: BoundId -> GraphState TyF -> Either BoundId Ty
resolveTy = evalState . graphResolveVar

writeDotExp :: FilePath -> GraphState TyF -> IO ()
writeDotExp p = writeFile p . renderDot . gsBoundEnv

main :: IO ()
main = do
  process "linear" exampleLinear
  process "exponential" exampleExponential

-- | A complete example of how to infer the type of an expression
-- with unification through 'Unitable' and 'Alignable'.
process :: String -> Exp -> IO ()
process name expr = do
  putStrLn ("*** Processing example: " ++ show name)
  putStrLn "--- Expression:"
  pPrint expr
  (expId, ig) <- initialGraphExp expr
  writeDotExp ("dot/" ++ name ++ "-initial.dot") ig
  putStrLn ("--- Expression id: " ++ show expId)
  putStrLn "--- Initial graph:"
  pPrint ig
  case processGraphExp ig of
    Left e -> throwIO e
    Right (rebinds, pg) -> do
      writeDotExp ("dot/" ++ name ++ "-processed.dot") pg
      putStrLn "--- Rebind map:"
      pPrint rebinds
      putStrLn "--- Final graph:"
      pPrint pg
      case resolveTy expId pg of
        Left missingId -> fail ("Missing id: " ++ show missingId)
        Right ty -> do
          putStrLn "--- Final type: "
          pPrint ty
