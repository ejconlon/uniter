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
import Uniter.Core (Index (..), Level (..), Quant (..), recGenQuant, recSpecTm, tupleToPair)
import Uniter.Example.Complex (InferCase (..), inferCases, inferWithFunDefs)
import qualified Uniter.Example.Complex as Complex
import qualified Uniter.Example.Simple as Simple
import Uniter.OrderedMap (OrderedMap)
import qualified Uniter.OrderedMap as OM
import Uniter.Reunitable.Driver (ReuniteSuccess (..), quickReuniteResult)
import Uniter.UnionMap (Changed (..), UnionEquiv (..), UnionMap, UnionMapAddVal (..), UnionMapLookupVal (..),
                        UnionMapMergeVal (..), UnionMapTraceRes (..), UnionMergeOne, addUnionMapM, concatUnionMergeOne,
                        emptyUnionMap, equivUnionMapM, lookupUnionMapM, mergeOneUnionMapM, sizeUnionMap, traceUnionMap,
                        valuesUnionMap)
import Uniter.Unitable.Driver (quickUniteResult)

newtype V = V { unV :: Int }
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

type UMV = UnionMap V (Max Int)

emptyUMV :: UMV
emptyUMV = emptyUnionMap

mergeOneUMV :: UnionMergeOne Void (Max Int) ()
mergeOneUMV = concatUnionMergeOne

testUmSimple :: TestTree
testUmSimple = testUnit "UM simple" $ runS emptyUMV $ do
  -- start with empty map
  testS $ \um -> sizeUnionMap um === 0
  -- add 'a'
  applyTestS (addUnionMapM (toV 'a') 1) $ \res um -> do
    res === UnionMapAddValAdded
    sizeUnionMap um === 1
    traceUnionMap (toV 'a') um === UnionMapTraceResFound (toV 'a') 1 []
    valuesUnionMap um === mapV [('a', 1)]
  -- lookup 'a'
  applyTestS (lookupUnionMapM (toV 'a')) $ \res _ ->
    res === UnionMapLookupValOk (toV 'a') 1 ChangedNo
  -- try to add 'a' again
  applyTestS (addUnionMapM (toV 'a') 1) $ \res um -> do
    res === UnionMapAddValDuplicate
    sizeUnionMap um === 1
  -- add 'b' and 'c' and check them
  _ <- applyS (addUnionMapM (toV 'b') 2)
  _ <- applyS (addUnionMapM (toV 'c') 3)
  testS $ \um -> do
    sizeUnionMap um === 3
    traceUnionMap (toV 'b') um === UnionMapTraceResFound (toV 'b') 2 []
    traceUnionMap (toV 'c') um === UnionMapTraceResFound (toV 'c') 3 []
    valuesUnionMap um === mapV [('a', 1), ('b', 2), ('c', 3)]
  applyTestS equivUnionMapM $ \(UnionEquiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', ""), ('b', ""), ('c', "")]
    bwd === mapVV []
  -- merge 'a' and 'c'
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'a') (toV 'c')) $ \res um -> do
    res === UnionMapMergeValMerged (toV 'a') 3 ()
    sizeUnionMap um === 3
    traceUnionMap (toV 'a') um === UnionMapTraceResFound (toV 'a') 3 []
    traceUnionMap (toV 'b') um === UnionMapTraceResFound (toV 'b') 2 []
    traceUnionMap (toV 'c') um === UnionMapTraceResFound (toV 'a') 3 [toV 'c']
    valuesUnionMap um === mapV [('a', 3), ('b', 2)]
  applyTestS equivUnionMapM $ \(UnionEquiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', "c"), ('b', "")]
    bwd === mapVV [('c', 'a')]
  -- try to merge again
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'a') (toV 'c')) $ \res _ ->
    res === UnionMapMergeValMerged (toV 'a') 3 ()
  -- and the other way around
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'c') (toV 'a')) $ \res _ ->
    res === UnionMapMergeValMerged (toV 'a') 3 ()
  -- and a non-existent merge
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'b') (toV 'z')) $ \res _ ->
    res === UnionMapMergeValMissing (toV 'z')
  -- and creating merge
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'z') (toV 'b')) $ \res _ ->
    res === UnionMapMergeValMerged (toV 'z') 2 ()
  applyTestS equivUnionMapM $ \(UnionEquiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', "c"), ('z', "b")]
    bwd === mapVV [('b', 'z'), ('c', 'a')]

testUmRec :: TestTree
testUmRec = testUnit "UM rec" $ runS emptyUMV $ do
  _ <- applyS (addUnionMapM (toV 'a') 1)
  _ <- applyS (addUnionMapM (toV 'b') 2)
  _ <- applyS (addUnionMapM (toV 'c') 3)
  _ <- applyS (addUnionMapM (toV 'c') 3)
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'b') (toV 'c')) $ \res um -> do
    res === UnionMapMergeValMerged (toV 'b') 3 ()
    sizeUnionMap um === 3
    traceUnionMap (toV 'a') um === UnionMapTraceResFound (toV 'a') 1 []
    traceUnionMap (toV 'b') um === UnionMapTraceResFound (toV 'b') 3 []
    traceUnionMap (toV 'c') um === UnionMapTraceResFound (toV 'b') 3 [toV 'c']
    valuesUnionMap um === mapV [('a', 1), ('b', 3)]
  applyTestS equivUnionMapM $ \(UnionEquiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', ""), ('b', "c")]
    bwd === mapVV [('c', 'b')]
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'a') (toV 'b')) $ \res um -> do
    res === UnionMapMergeValMerged (toV 'a') 3 ()
    sizeUnionMap um === 3
    traceUnionMap (toV 'a') um === UnionMapTraceResFound (toV 'a') 3 []
    traceUnionMap (toV 'b') um === UnionMapTraceResFound (toV 'a') 3 [toV 'b']
    traceUnionMap (toV 'c') um === UnionMapTraceResFound (toV 'a') 3 [toV 'b', toV 'c']
    valuesUnionMap um === mapV [('a', 3)]
  applyTestS equivUnionMapM $ \(UnionEquiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', "bc")]
    bwd === mapVV [('b', 'a'), ('c', 'a')]

testUmTail :: TestTree
testUmTail = testUnit "UM tail" $ runS emptyUMV $ do
  applyS $ do
    _ <- addUnionMapM (toV 'a') 1
    _ <- addUnionMapM (toV 'b') 2
    _ <- addUnionMapM (toV 'c') 3
    _ <- addUnionMapM (toV 'd') 4
    _ <- mergeOneUnionMapM mergeOneUMV (toV 'c') (toV 'd')
    _ <- mergeOneUnionMapM mergeOneUMV (toV 'b') (toV 'c')
    _ <- mergeOneUnionMapM mergeOneUMV (toV 'a') (toV 'b')
    pure ()
  testS $ \um ->
    traceUnionMap (toV 'd') um === UnionMapTraceResFound (toV 'a') 4 [toV 'b', toV 'c', toV 'd']
  applyTestS equivUnionMapM $ \(UnionEquiv fwd bwd) _ -> do
    fwd === multiMapVV [('a', "bcd")]
    bwd === mapVV [('b', 'a'), ('c', 'a'), ('d', 'a')]
  testS $ \um ->
    traceUnionMap (toV 'd') um === UnionMapTraceResFound (toV 'a') 4 [toV 'd']

testUmUnit :: TestTree
testUmUnit = testGroup "UM unit"
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
  OM.lookup 'a' x === Nothing
  OM.lookup 'b' x === Nothing
  OM.unsnoc x === Nothing
  -- [(a, 1)]
  let y = OM.snoc x 'a' 1
  OM.level y === Level 1
  OM.order y === Seq.fromList [(Index 0, 'a', 1)]
  OM.toList y === [('a', 1)]
  OM.fromList [('a', 1)] === y
  OM.lookup 'a' y === Just (Index 0, 1)
  OM.lookup 'b' y === Nothing
  OM.unsnoc y === Just (x, 'a', 1)
  -- [(a, 1), (b, 2)]
  let z = OM.snoc y 'b' 2
  OM.level z === Level 2
  OM.order z === Seq.fromList [(Index 1, 'a', 1), (Index 0, 'b', 2)]
  OM.toList z === [('a', 1), ('b', 2)]
  OM.fromList [('a', 1), ('b', 2)] === z
  OM.lookup 'a' z === Just (Index 1, 1)
  OM.lookup 'b' z === Just (Index 0, 2)
  OM.unsnoc z === Just (y, 'b', 2)
  -- [(a, 1), (b, 2), (a, 3)]
  let w = OM.snoc z 'a' 3
  OM.level w === Level 3
  OM.order w === Seq.fromList [(Index 1, 'b', 2), (Index 0, 'a', 3)]
  OM.toList w === [('a', 1), ('b', 2), ('a', 3)]
  OM.fromList [('a', 1), ('b', 2), ('a', 3)] === w
  OM.lookup 'a' w === Just (Index 0, 3)
  OM.lookup 'b' w === Just (Index 1, 2)
  OM.unsnoc w === Just (z, 'a', 3)
  -- [(a, 1), (b, 2), (a, 3), (a, 4)]
  let v = OM.snoc w 'a' 4
  OM.level v === Level 4
  OM.order v === Seq.fromList [(Index 2, 'b', 2), (Index 0, 'a', 4)]
  OM.toList v === [('a', 1), ('b', 2), ('a', 3), ('a', 4)]
  OM.fromList [('a', 1), ('b', 2), ('a', 3), ('a', 4)] === v
  OM.lookup 'a' v === Just (Index 0, 4)
  OM.lookup 'b' v === Just (Index 2, 2)
  OM.unsnoc v === Just (w, 'a', 4)
  -- snoc all
  let v' = OM.snocAll y (Seq.fromList (fmap tupleToPair [('b', 2), ('a', 3), ('a', 4)]))
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
  let x1 = Complex.AnnExpLet "v1" (recGenQuant Complex.TyInt) (Complex.AnnExpInt 1) x2
      x2 = Complex.AnnExpLet "v2" (recGenQuant (Complex.TyPair Complex.TyInt Complex.TyInt)) (Complex.AnnExpTuple (Complex.AnnExpBound 0) (Complex.AnnExpBound 0)) x3
      x3 = Complex.AnnExpLet "v3" (recGenQuant (Complex.TyPair Complex.TyInt Complex.TyInt)) (Complex.AnnExpTuple (Complex.AnnExpSecond (Complex.AnnExpBound 0)) (Complex.AnnExpFirst (Complex.AnnExpBound 0))) x4
      x4 = Complex.AnnExpBound 0
  let expLinTm = x1
      expLinTy = Complex.TyPair Complex.TyInt Complex.TyInt
  (actualLinTm, actualLinTy) <- quickReuniteResult mempty Complex.exampleLinear
  actualLinTm === QuantBare (recSpecTm expLinTm)
  actualLinTy === recGenQuant expLinTy

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
main = testMain $ \_ -> testGroup "Uniter"
  [ testUmUnit
  , testOmUnit
  , testExampleSimple
  -- TODO reenable
  -- , testExampleComplex
  -- , testComplexCases
  ]
