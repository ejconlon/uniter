{-# LANGUAGE OverloadedStrings #-}

module Test.Uniter.Main (main) where

import Data.Bifunctor (bimap, first)
import Data.Char (chr, ord)
import Data.Semigroup (Max)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import qualified IntLike.Set as ILS
import PropUnit (TestTree, testGroup, testMain, testUnit, (===))
import Test.Uniter.State (applyS, applyTestS, runS, testS)
import Uniter.Core (ForAll (..), Index (..), Level (..), UniqueId, monoToPolyTy, recSpecTm)
import Uniter.Example.Complex (InferCase (..), inferCases, inferWithFunDefs)
import qualified Uniter.Example.Complex as Complex
import qualified Uniter.Example.Simple as Simple
import Uniter.OrderedMap (OrderedMap)
import qualified Uniter.OrderedMap as OM
import Uniter.Reunitable.Driver (ReuniteSuccess (..), quickReuniteResult)
import qualified Uniter.UnionMap as UM
import Uniter.Unitable.Driver (quickUniteResult)

newtype V = V {unV :: Int}
  deriving newtype (Eq)
  deriving stock (Show)

toV :: Char -> V
toV = V . ord

fromV :: V -> Char
fromV = chr . unV

setV :: String -> IntLikeSet V
setV = ILS.fromList . fmap toV

mapV :: [(Char, a)] -> IntLikeMap V a
mapV = ILM.fromList . fmap (first toV)

mapVV :: [(Char, Char)] -> IntLikeMap V V
mapVV = ILM.fromList . fmap (bimap toV toV)

multiMapVV :: [(Char, String)] -> IntLikeMap V (IntLikeSet V)
multiMapVV = ILM.fromList . fmap (bimap toV setV)

type UMV = UM.UnionMap V (Max Int)

emptyUMV :: UMV
emptyUMV = UM.empty

mergeOneUMV :: UM.MergeOne Void (Max Int) ()
mergeOneUMV = UM.concatMergeOne

testUmSimple :: TestTree
testUmSimple = testUnit "UM simple" $ runS emptyUMV $ do
  -- start with empty map
  testS $ \um -> UM.size um === 0
  -- add 'a'
  applyTestS (UM.addM (toV 'a') 1) $ \res um -> do
    res === UM.AddValAdded
    UM.size um === 1
    UM.trace (toV 'a') um === UM.TraceResFound (toV 'a') 1 []
    UM.values um === mapV [('a', 1)]
  -- lookup 'a'
  applyTestS (UM.lookupM (toV 'a')) $ \res _ ->
    res === UM.LookupValOk (toV 'a') 1 UM.ChangedNo
  -- try to add 'a' again
  applyTestS (UM.addM (toV 'a') 1) $ \res um -> do
    res === UM.AddValDuplicate
    UM.size um === 1
  -- add 'b' and 'c' and check them
  _ <- applyS (UM.addM (toV 'b') 2)
  _ <- applyS (UM.addM (toV 'c') 3)
  testS $ \um -> do
    UM.size um === 3
    UM.trace (toV 'b') um === UM.TraceResFound (toV 'b') 2 []
    UM.trace (toV 'c') um === UM.TraceResFound (toV 'c') 3 []
    UM.values um === mapV [('a', 1), ('b', 2), ('c', 3)]
  applyTestS UM.equivM $ \(UM.Equiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', ""), ('b', ""), ('c', "")]
    bwd === mapVV []
  -- merge 'a' and 'c'
  applyTestS (UM.mergeOneM mergeOneUMV (toV 'a') (toV 'c')) $ \res um -> do
    res === UM.MergeValMerged (toV 'a') 3 ()
    UM.size um === 3
    UM.trace (toV 'a') um === UM.TraceResFound (toV 'a') 3 []
    UM.trace (toV 'b') um === UM.TraceResFound (toV 'b') 2 []
    UM.trace (toV 'c') um === UM.TraceResFound (toV 'a') 3 [toV 'c']
    UM.values um === mapV [('a', 3), ('b', 2)]
  applyTestS UM.equivM $ \(UM.Equiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', "c"), ('b', "")]
    bwd === mapVV [('c', 'a')]
  -- try to merge again
  applyTestS (UM.mergeOneM mergeOneUMV (toV 'a') (toV 'c')) $ \res _ ->
    res === UM.MergeValMerged (toV 'a') 3 ()
  -- and the other way around
  applyTestS (UM.mergeOneM mergeOneUMV (toV 'c') (toV 'a')) $ \res _ ->
    res === UM.MergeValMerged (toV 'a') 3 ()
  -- and a non-existent merge
  applyTestS (UM.mergeOneM mergeOneUMV (toV 'b') (toV 'z')) $ \res _ ->
    res === UM.MergeValMissing (toV 'z')
  -- and creating merge
  applyTestS (UM.mergeOneM mergeOneUMV (toV 'z') (toV 'b')) $ \res _ ->
    res === UM.MergeValMerged (toV 'z') 2 ()
  applyTestS UM.equivM $ \(UM.Equiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', "c"), ('z', "b")]
    bwd === mapVV [('b', 'z'), ('c', 'a')]

testUmRec :: TestTree
testUmRec = testUnit "UM rec" $ runS emptyUMV $ do
  _ <- applyS (UM.addM (toV 'a') 1)
  _ <- applyS (UM.addM (toV 'b') 2)
  _ <- applyS (UM.addM (toV 'c') 3)
  _ <- applyS (UM.addM (toV 'c') 3)
  applyTestS (UM.mergeOneM mergeOneUMV (toV 'b') (toV 'c')) $ \res um -> do
    res === UM.MergeValMerged (toV 'b') 3 ()
    UM.size um === 3
    UM.trace (toV 'a') um === UM.TraceResFound (toV 'a') 1 []
    UM.trace (toV 'b') um === UM.TraceResFound (toV 'b') 3 []
    UM.trace (toV 'c') um === UM.TraceResFound (toV 'b') 3 [toV 'c']
    UM.values um === mapV [('a', 1), ('b', 3)]
  applyTestS UM.equivM $ \(UM.Equiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', ""), ('b', "c")]
    bwd === mapVV [('c', 'b')]
  applyTestS (UM.mergeOneM mergeOneUMV (toV 'a') (toV 'b')) $ \res um -> do
    res === UM.MergeValMerged (toV 'a') 3 ()
    UM.size um === 3
    UM.trace (toV 'a') um === UM.TraceResFound (toV 'a') 3 []
    UM.trace (toV 'b') um === UM.TraceResFound (toV 'a') 3 [toV 'b']
    UM.trace (toV 'c') um === UM.TraceResFound (toV 'a') 3 [toV 'b', toV 'c']
    UM.values um === mapV [('a', 3)]
  applyTestS UM.equivM $ \(UM.Equiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', "bc")]
    bwd === mapVV [('b', 'a'), ('c', 'a')]

testUmTail :: TestTree
testUmTail = testUnit "UM tail" $ runS emptyUMV $ do
  applyS $ do
    _ <- UM.addM (toV 'a') 1
    _ <- UM.addM (toV 'b') 2
    _ <- UM.addM (toV 'c') 3
    _ <- UM.addM (toV 'd') 4
    _ <- UM.mergeOneM mergeOneUMV (toV 'c') (toV 'd')
    _ <- UM.mergeOneM mergeOneUMV (toV 'b') (toV 'c')
    _ <- UM.mergeOneM mergeOneUMV (toV 'a') (toV 'b')
    pure ()
  testS $ \um ->
    UM.trace (toV 'd') um === UM.TraceResFound (toV 'a') 4 [toV 'b', toV 'c', toV 'd']
  applyTestS UM.equivM $ \(UM.Equiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', "bcd")]
    bwd === mapVV [('b', 'a'), ('c', 'a'), ('d', 'a')]
  testS $ \um ->
    UM.trace (toV 'd') um === UM.TraceResFound (toV 'a') 4 [toV 'd']

testUmUnit :: TestTree
testUmUnit =
  testGroup
    "UM unit"
    [ testUmSimple
    , testUmRec
    , testUmTail
    ]

testOmUnit :: TestTree
testOmUnit = testUnit "OM unit" $ do
  -- []
  let x = OM.empty :: OrderedMap Char Int
  OM.level x === Level 0
  OM.order x === Empty
  OM.toList x === []
  OM.fromList [] === x
  OM.lookupByKey 'a' x === Nothing
  OM.lookupByKey 'b' x === Nothing
  -- [(a, 1)]
  let y = OM.snoc x (Just 'a') 1
  OM.level y === Level 1
  OM.order y === Seq.fromList [(Index 0, Just 'a', 1)]
  OM.toList y === [(Just 'a', 1)]
  OM.fromList [(Just 'a', 1)] === y
  OM.lookupByKey 'a' y === Just (Index 0, 1)
  OM.lookupByKey 'b' y === Nothing
  -- [(a, 1), (b, 2)]
  let z = OM.snoc y (Just 'b') 2
  OM.level z === Level 2
  OM.order z === Seq.fromList [(Index 1, Just 'a', 1), (Index 0, Just 'b', 2)]
  OM.toList z === [(Just 'a', 1), (Just 'b', 2)]
  OM.fromList [(Just 'a', 1), (Just 'b', 2)] === z
  OM.lookupByKey 'a' z === Just (Index 1, 1)
  OM.lookupByKey 'b' z === Just (Index 0, 2)
  -- [(a, 1), (b, 2), (a, 3)]
  let w = OM.snoc z (Just 'a') 3
  OM.level w === Level 3
  OM.order w === Seq.fromList [(Index 1, Just 'b', 2), (Index 0, Just 'a', 3)]
  OM.toList w === [(Just 'a', 1), (Just 'b', 2), (Just 'a', 3)]
  OM.fromList [(Just 'a', 1), (Just 'b', 2), (Just 'a', 3)] === w
  OM.lookupByKey 'a' w === Just (Index 0, 3)
  OM.lookupByKey 'b' w === Just (Index 1, 2)
  -- [(a, 1), (b, 2), (a, 3), (a, 4)]
  let v = OM.snoc w (Just 'a') 4
  OM.level v === Level 4
  OM.order v === Seq.fromList [(Index 2, Just 'b', 2), (Index 0, Just 'a', 4)]
  OM.toList v === [(Just 'a', 1), (Just 'b', 2), (Just 'a', 3), (Just 'a', 4)]
  OM.fromList [(Just 'a', 1), (Just 'b', 2), (Just 'a', 3), (Just 'a', 4)] === v
  OM.lookupByKey 'a' v === Just (Index 0, 4)
  OM.lookupByKey 'b' v === Just (Index 2, 2)
  -- snoc all
  let v' = OM.snocAll y (Seq.fromList [(Just 'b', 2), (Just 'a', 3), (Just 'a', 4)])
  v' === v

testExampleSimple :: TestTree
testExampleSimple = testUnit "simple example" $ do
  let expLinTy = Simple.TyPair Simple.TyConst Simple.TyConst
  actualLinTy <- quickUniteResult mempty Simple.exampleLinear
  actualLinTy === expLinTy
  let expExpTy = Simple.TyPair expLinTy (Simple.TyPair expLinTy expLinTy)
  actualExpTy <- quickUniteResult mempty Simple.exampleExponential
  actualExpTy === expExpTy

testExampleComplex :: TestTree
testExampleComplex = testUnit "complex example" $ do
  let x1 = Complex.AnnExpLet "v1" (monoToPolyTy Complex.TyInt) (Complex.AnnExpInt 1) x2
      x2 = Complex.AnnExpLet "v2" (monoToPolyTy (Complex.TyPair Complex.TyInt Complex.TyInt)) (Complex.AnnExpTuple (Complex.AnnExpBound 0) (Complex.AnnExpBound 0)) x3
      x3 = Complex.AnnExpLet "v3" (monoToPolyTy (Complex.TyPair Complex.TyInt Complex.TyInt)) (Complex.AnnExpTuple (Complex.AnnExpSecond (Complex.AnnExpBound 0)) (Complex.AnnExpFirst (Complex.AnnExpBound 0))) x4
      x4 = Complex.AnnExpBound 0
  let expLinTm = x1
      expLinTy = Complex.TyPair Complex.TyInt Complex.TyInt
  (actualLinTm, actualLinTy) <- quickReuniteResult mempty (Complex.exampleLinear @UniqueId)
  actualLinTm === ForAll Seq.empty (recSpecTm expLinTm)
  actualLinTy === monoToPolyTy expLinTy

runInferCase :: InferCase -> TestTree
runInferCase (InferCase name tm expected) = testUnit name $ do
  case inferWithFunDefs tm of
    Right (ReuniteSuccess _ actualRecon actualTy _) ->
      case expected of
        Just (expectedRecon, expectedTy) -> do
          actualRecon === expectedRecon
          actualTy === expectedTy
        Nothing -> fail "expected failure but was success"
    Left failure ->
      case expected of
        Just _ -> fail ("expected success but was failure: " ++ show failure)
        Nothing -> pure ()

testComplexCases :: TestTree
testComplexCases = testGroup "complex cases" (fmap runInferCase inferCases)

main :: IO ()
main = testMain $ \_ ->
  testGroup
    "Uniter"
    [ testUmUnit
    , testOmUnit
    , testExampleSimple
    , testExampleComplex
    , testComplexCases
    ]
