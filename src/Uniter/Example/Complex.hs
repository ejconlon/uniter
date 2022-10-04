{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Uniter.Example.Complex
  ( Ty (..)
  , TyF (..)
  , Exp (..)
  , ExpF (..)
  , AnnExp (..)
  , AnnExpF (..)
  , exampleLinear
  , exampleExponential
  , funDefs
  , InferCase (..)
  , inferCases
  , inferWithFunDefs
  , processVerbose
  , main
  ) where

import Control.Monad (void, (>=>))
import Control.Monad.Catch (MonadThrow (..))
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Foldable (for_)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.These (These (..))
import Text.Pretty.Simple (pPrint)
import Uniter.Align (Alignable (..), UnalignableErr (..))
import Uniter.Core (Index, PolyTy, Quant (..), SpecFinal, TmVar, TyBinder (..), bareQuantTy, embedBoundTy, embedSpecTm,
                    forAllQuantTy, recSpecTm, varBoundTy)
import Uniter.Render (writeGraphDot, writePreGraphDot)
import Uniter.Reunitable.Class (MonadReuniter (..), Reunitable (..))
import Uniter.Reunitable.Driver (ReuniteResult, ReuniteSuccess (..), reuniteResult)

data Ty =
    TyInt
  | TyFun Ty Ty
  | TyPair Ty Ty
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Ty

deriving stock instance Eq r => Eq (TyF r)
deriving stock instance Ord r => Ord (TyF r)
deriving stock instance Show r => Show (TyF r)

data Exp ty =
    ExpFree !TmVar
  | ExpInt !Int
  | ExpAdd (Exp ty) (Exp ty)
  | ExpIfZero (Exp ty) (Exp ty) (Exp ty)
  | ExpTuple (Exp ty) (Exp ty)
  | ExpFirst (Exp ty)
  | ExpSecond (Exp ty)
  | ExpApp (Exp ty) (Exp ty)
  | ExpAbs !TmVar !(Maybe ty) !(Exp ty)
  | ExpLet !TmVar !(Maybe ty) !(Exp ty) !(Exp ty)
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Exp

deriving stock instance (Eq r, Eq ty) => Eq (ExpF ty r)
deriving stock instance (Ord r, Ord ty) => Ord (ExpF ty r)
deriving stock instance (Show r, Show ty) => Show (ExpF ty r)

deriveBifunctor ''ExpF
deriveBifoldable ''ExpF
deriveBitraversable ''ExpF

data AnnExp ty =
    AnnExpBound !Index
  | AnnExpFree !TmVar
  | AnnExpInt !Int
  | AnnExpAdd (AnnExp ty) (AnnExp ty)
  | AnnExpIfZero (AnnExp ty) (AnnExp ty) (AnnExp ty)
  | AnnExpTuple (AnnExp ty) (AnnExp ty)
  | AnnExpFirst (AnnExp ty)
  | AnnExpSecond (AnnExp ty)
  | AnnExpApp (AnnExp ty) (AnnExp ty)
  | AnnExpAbs !TmVar !ty (AnnExp ty)
  | AnnExpLet !TmVar !ty (AnnExp ty) (AnnExp ty)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''AnnExp

deriving stock instance (Eq ty, Eq r) => Eq (AnnExpF ty r)
deriving stock instance (Ord ty, Ord r) => Ord (AnnExpF ty r)
deriving stock instance (Show ty, Show r) => Show (AnnExpF ty r)

deriveBifunctor ''AnnExpF
deriveBifoldable ''AnnExpF
deriveBitraversable ''AnnExpF

instance Alignable UnalignableErr TyF where
  align x y =
    case (x, y) of
      (TyIntF, TyIntF) -> Right TyIntF
      (TyFunF xa xb, TyFunF ya yb) -> Right (TyFunF (These xa ya) (These xb yb))
      (TyPairF xa xb, TyPairF ya yb) -> Right (TyPairF (These xa ya) (These xb yb))
      _ -> Left UnalignableErr

instance Reunitable (ExpF Ty) AnnExpF TyF where
  reunite = \case
    ExpIntF c -> do
      x <- reuniterAddNodeTy TyIntF
      pure (x, embedSpecTm (AnnExpIntF c))
    ExpAddF mi mj -> do
      (i, si) <- mi
      (j, sj) <- mj
      x <- reuniterAddNodeTy TyIntF
      _ <- reuniterConstrainEq x i
      _ <- reuniterConstrainEq x j
      pure (x, embedSpecTm (AnnExpAddF si sj))
    ExpIfZeroF mi mj mk -> do
      (i, si) <- mi
      (j, sj) <- mj
      (k, sk) <- mk
      x <- reuniterAddNodeTy TyIntF
      y <- reuniterFreshVar (TyBinder Nothing)
      _ <- reuniterConstrainEq x i
      _ <- reuniterConstrainEq y j
      _ <- reuniterConstrainEq y k
      pure (y, embedSpecTm (AnnExpIfZeroF si sj sk))
    ExpTupleF mi mj -> do
      (i, si) <- mi
      (j, sj) <- mj
      z <- reuniterAddNodeTy (TyPairF i j)
      pure (z, embedSpecTm (AnnExpTupleF si sj))
    ExpFirstF mi -> do
      (i, si) <- mi
      v <- reuniterFreshVar (TyBinder Nothing)
      w <- reuniterFreshVar (TyBinder Nothing)
      y <- reuniterAddNodeTy (TyPairF v w)
      _ <- reuniterConstrainEq i y
      pure (v, embedSpecTm (AnnExpFirstF si))
    ExpSecondF mi -> do
      (i, si) <- mi
      v <- reuniterFreshVar (TyBinder Nothing)
      w <- reuniterFreshVar (TyBinder Nothing)
      y <- reuniterAddNodeTy (TyPairF v w)
      _ <- reuniterConstrainEq i y
      pure (w, embedSpecTm (AnnExpSecondF si))
    ExpFreeF n ->
      let onFree = embedSpecTm (AnnExpFreeF n)
          onBound = embedSpecTm . AnnExpBoundF
      in reuniterResolveTmVar n onFree onBound
    ExpAppF mi mj -> do
      (i, si) <- mi
      (j, sj) <- mj
      x <- reuniterFreshVar (TyBinder Nothing)
      y <- reuniterAddNodeTy (TyFunF j x)
      _ <- reuniterConstrainEq y i
      pure (x, embedSpecTm (AnnExpAppF si sj))
    ExpAbsF n mt mi -> do
      x <- maybe (reuniterFreshVar (TyBinder Nothing)) reuniterAddMonoTy mt
      (y, sy) <- reuniterBindTmVar n x mi
      pure (y, embedSpecTm (AnnExpAbsF n y sy))
    ExpLetF n mt mi mj -> do
      (i, si) <- mi
      i' <- maybe (pure i) (reuniterAddMonoTy >=> reuniterConstrainEq i) mt
      (y, sy) <- reuniterBindTmVar n i' mj
      pure (y, embedSpecTm (AnnExpLetF n i' si sy))

-- | A small example of type (C, C)
exampleLinear :: Exp Ty
exampleLinear =
  let x1 = ExpLet "v1" Nothing (ExpInt 1) x2
      x2 = ExpLet "v2" Nothing (ExpTuple (ExpFree "v1") (ExpFree "v1")) x3
      x3 = ExpLet "v3" Nothing (ExpTuple (ExpSecond (ExpFree "v2")) (ExpFirst (ExpFree "v2"))) x4
      x4 = ExpFree "v3"
  in x1

-- | An example that can easily grow larger: ((C, C), ((C, C), (C, C)))
exampleExponential :: Exp Ty
exampleExponential =
  let x1 = ExpLet "v1" Nothing (ExpInt 1) x2
      x2 = ExpLet "v2" Nothing (ExpTuple (ExpFree "v1") (ExpFree "v1")) x3
      x3 = ExpLet "v3" Nothing (ExpTuple (ExpFirst (ExpFree "v2")) (ExpFree "v2")) x4
      x4 = ExpLet "v4" Nothing (ExpTuple (ExpSecond (ExpFree "v3")) (ExpTuple (ExpFree "v2") (ExpFree "v2"))) x5
      x5 = ExpFree "v4"
  in x1

-- | Some examples of functions (including polymorphic ones)
funDefs :: Map TmVar (PolyTy TyF)
funDefs = Map.fromList
  [ ("undefined", forAllQuantTy (Seq.fromList ["a"]) (varBoundTy 0))
  , ("id", forAllQuantTy (Seq.fromList ["a"]) (embedBoundTy (TyFunF (varBoundTy 0) (varBoundTy 0))))
  , ("const", forAllQuantTy (Seq.fromList ["a", "b"]) (embedBoundTy (TyFunF (varBoundTy 0) (embedBoundTy (TyFunF (varBoundTy 1) (varBoundTy 0))))))
  , ("zero", bareQuantTy TyInt)
  , ("succ", bareQuantTy (TyFun TyInt TyInt))
  ]

data InferCase = InferCase
  { icName :: !String
  , icTm :: !(Exp Ty)
  , icExpected :: !(Maybe (SpecFinal AnnExpF TyF, PolyTy TyF))
  } deriving stock (Eq, Show)

inferCases :: [InferCase]
inferCases =
  [ InferCase "zero" (ExpFree "zero") (Just (QuantBare (recSpecTm (AnnExpFree "zero")), bareQuantTy TyInt))
  -- , InferCase "succ" (ExpFree "succ") (Just (recSpecTm (AnnExpFree "succ"), bareQuant (TyFun TyInt TyInt)))
  -- , InferCase "succ-zero" (ExpApp (ExpFree "succ") (ExpFree "zero")) (Just (recSpecTm (AnnExpApp (AnnExpFree "succ") (AnnExpFree "zero")), bareQuant TyInt))
  -- , -- succ undefined (should be int ty with the undefined specialized with int)
  --   let recon = embedSpecTm (AnnExpAppF (recSpecTm (AnnExpFree "succ")) (bindSpecTm (Seq.singleton (bareQuant TyInt)) (recSpecTm (AnnExpFree "undefined"))))
  --   in InferCase "succ-undefined" (ExpApp (ExpFree "succ") (ExpFree "undefined")) (Just (recon, bareQuant TyInt))
  -- , -- id zero (should be int ty with id specialized with int)
  --   let recon = embedSpecTm (AnnExpAppF (bindSpecTm (Seq.singleton (bareQuant TyInt)) (recSpecTm (AnnExpFree "id"))) (recSpecTm (AnnExpFree "zero")))
  --   in InferCase "id-zero" (ExpApp (ExpFree "id") (ExpFree "zero")) (Just (recon, bareQuant TyInt))
  -- , -- id undefined (should be poly)
  --   -- TODO this is nonsense...
  --   let recon = embedSpecTm $ AnnExpAppF
  --         (bindSpecTm (Seq.singleton (forAllQuant (Seq.singleton (GenBinder 0 (Just "a"))) (varBoundTy 0))) (recSpecTm (AnnExpFree "id")))
  --         (bindSpecTm (Seq.singleton (forAllQuant (Seq.singleton (GenBinder 2 (Just "a"))) (varBoundTy 0))) (recSpecTm (AnnExpFree "undefined")))
  --       ty = forAllQuant (Seq.singleton (GenBinder 3 (Just "a"))) (varBoundTy 0)
  --   in InferCase "id-undefined" (ExpApp (ExpFree "id") (ExpFree "undefined")) (Just (recon, ty))
  ]

inferWithFunDefs :: Exp Ty -> ReuniteResult UnalignableErr AnnExpF TyF
inferWithFunDefs = snd . reuniteResult funDefs

-- | A complete example of how to infer the type of an expression
-- with unification through 'Unitable' and 'Alignable'.
processVerbose :: String -> Exp Ty -> IO (PolyTy TyF)
processVerbose name expr = go where
  go = do
    putStrLn ("*** Processing example: " ++ show name)
    putStrLn "--- Expression:"
    pPrint expr
    let (pg, res) = reuniteResult funDefs expr
    writePreGraphDot ("dot/output/" ++ name ++ "-initial.dot") pg
    case res of
      Left e -> do
        putStrLn "--- Failure"
        throwM e
      Right (ReuniteSuccess bid tm ty g) -> do
        putStrLn "--- Success"
        goVerbose bid g
        putStrLn "--- Final tm:"
        pPrint tm
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
  void $ processVerbose "complex-linear" exampleLinear
  void $ processVerbose "complex-exponential" exampleExponential
  for_ inferCases (\(InferCase name tm _) -> processVerbose ("complex-" ++ name) tm)
