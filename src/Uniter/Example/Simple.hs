{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A simple example following "Efficient Functional Unification and Substitution"
-- by Dijkstra, Middelkoop, and Swierstra.
module Uniter.Example.Simple
  ( Exp (..)
  , ExpF (..)
  , Ty (..)
  , TyF (..)
  , exampleLinear
  , exampleExponential
  , processVerbose
  , main
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadThrow (..))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.These (These (..))
import Text.Pretty.Simple (pPrint)
import Uniter (Alignable (..), MonadUniter (..), TmVar, UnalignableErr (..), Unitable (..), UniteSuccess (..),
               uniteResult)
import Uniter.Render (writeGraphDot, writePreGraphDot)

-- | A simple expression language with constants, vars, lets, tuples, and projections.
data Exp =
    ExpConst
  | ExpUseBind !TmVar
  | ExpDefBind !TmVar Exp Exp
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
    TyConst
  | TyPair Ty Ty
  deriving stock (Eq, Show)

-- Builds a type functor TyF
makeBaseFunctor ''Ty

deriving stock instance Eq a => Eq (TyF a)
deriving stock instance Show a => Show (TyF a)

instance Alignable UnalignableErr TyF where
  -- Align our type functor in the natural way
  align TyConstF TyConstF = Right TyConstF
  align (TyPairF a b) (TyPairF c d) = Right (TyPairF (These a c) (These b d))
  align _ _ = Left UnalignableErr

instance Unitable ExpF TyF where
  -- Inspect our expression functor and perform unification
  unite = \case
    ExpConstF ->
      -- constants have constant type
      uniterAddBaseTy TyConstF
    ExpUseBindF n -> do
      -- vars require we lookup the binding, throwing if it's not there
      uniterResolveTmVar n
    ExpDefBindF n mx my -> do
      -- first get the type of the argument of the let
      x <- mx
      -- then bind the var, and return the type of the body with it bound
      uniterBindTmVar n x my
    ExpTupleF mx my -> do
      -- find the type of both arguments
      x <- mx
      y <- my
      -- and tuple them together!
      uniterAddBaseTy (TyPairF x y)
    ExpFirstF mx -> do
      -- find the type of the argument
      x <- mx
      -- and ensure it unifies with a tuple type
      v <- uniterFreshVar Nothing
      w <- uniterFreshVar Nothing
      y <- uniterAddBaseTy (TyPairF v w)
      _ <- uniterConstrainEq x y
      -- fst returns the type of the first element of the pair
      pure v
    ExpSecondF mx -> do
      -- just like first, find the type of the argument
      x <- mx
      -- and again ensure it unifies with a tuple type
      v <- uniterFreshVar Nothing
      w <- uniterFreshVar Nothing
      y <- uniterAddBaseTy (TyPairF v w)
      _ <- uniterConstrainEq x y
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

-- | A complete example of how to infer the type of an expression
-- with unification through 'Unitable' and 'Alignable'.
processVerbose :: String -> Exp -> IO Ty
processVerbose name expr = go where
  go = do
    putStrLn ("*** Processing example: " ++ show name)
    putStrLn "--- Expression:"
    pPrint expr
    let (pg, res) = uniteResult mempty expr
    writePreGraphDot ("dot/output/" ++ name ++ "-initial.dot") pg
    case res of
      Left e -> do
        putStrLn "--- Failure"
        throwM e
      Right (UniteSuccess bid ty g) -> do
        putStrLn "--- Success"
        goVerbose bid g
        putStrLn "--- Final type: "
        pPrint ty
        pure ty
  goVerbose bid g = do
    writeGraphDot ("dot/output/" ++ name ++ "-processed.dot") g
    putStrLn ("--- Expr id: " ++ show bid)
    putStrLn "--- Final graph:"
    pPrint g

main :: IO ()
main = do
  void $ processVerbose "simple-linear" exampleLinear
  void $ processVerbose "simple-exponential" exampleExponential
