module Test.Uniter.Main (main) where

import Data.Bifunctor (bimap, first)
import Data.Char (chr, ord)
import Data.Semigroup (Max)
import Data.Void (Void)
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Uniter.Assertions (testUnit, (===))
import Test.Uniter.State (applyS, applyTestS, runS, testS)
import Uniter.UnionMap (Changed (..), UnionMap, UnionMapAddVal (..), UnionMapLookupVal (..), UnionMapMergeOneVal (..),
                        UnionMapTraceRes (..), UnionMergeOne, addUnionMapM, emptyUnionMap, equivUnionMapM,
                        lookupUnionMapM, mergeOneUnionMapM, semigroupUnionMergeOne, sizeUnionMap, traceUnionMap,
                        valuesUnionMap)

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

mergeOneUMV :: UnionMergeOne Void (Max Int)
mergeOneUMV = semigroupUnionMergeOne

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
  applyTestS equivUnionMapM $ \(fwd, bwd) _ -> do
    fwd === multiMapVV [('a', ""), ('b', ""), ('c', "")]
    bwd === mapVV []
  -- merge 'a' and 'c'
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'a') (toV 'c')) $ \res um -> do
    res === UnionMapMergeOneValMerged (toV 'a') 3 ChangedYes
    sizeUnionMap um === 3
    traceUnionMap (toV 'a') um === UnionMapTraceResFound (toV 'a') 3 []
    traceUnionMap (toV 'b') um === UnionMapTraceResFound (toV 'b') 2 []
    traceUnionMap (toV 'c') um === UnionMapTraceResFound (toV 'a') 3 []
    valuesUnionMap um === mapV [('a', 3), ('b', 2)]
  applyTestS equivUnionMapM $ \(fwd, bwd) _ -> do
    fwd === multiMapVV [('a', "c"), ('b', "")]
    bwd === mapVV [('c', 'a')]
  -- try to merge again
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'a') (toV 'c')) $ \res _ ->
    res === UnionMapMergeOneValMerged (toV 'a') 3 ChangedNo
  -- and the other way around
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'c') (toV 'a')) $ \res _ ->
    res === UnionMapMergeOneValMerged (toV 'a') 3 ChangedNo
  -- and a non-existent merge
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'b') (toV 'z')) $ \res _ ->
    res === UnionMapMergeOneValMissing (toV 'z')
  -- and creating merge
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'z') (toV 'b')) $ \res _ ->
    res === UnionMapMergeOneValMerged (toV 'z') 2 ChangedYes
  applyTestS equivUnionMapM $ \(fwd, bwd) _ -> do
    fwd === multiMapVV [('a', "c"), ('z', "b")]
    bwd === mapVV [('b', 'z'), ('c', 'a')]

testUmRec :: TestTree
testUmRec = testUnit "UM rec" $ runS emptyUMV $ do
  _ <- applyS (addUnionMapM (toV 'a') 1)
  _ <- applyS (addUnionMapM (toV 'b') 2)
  _ <- applyS (addUnionMapM (toV 'c') 3)
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'b') (toV 'c')) $ \res um -> do
    res === UnionMapMergeOneValMerged (toV 'b') 3 ChangedYes
    sizeUnionMap um === 3
    traceUnionMap (toV 'a') um === UnionMapTraceResFound (toV 'a') 1 []
    traceUnionMap (toV 'b') um === UnionMapTraceResFound (toV 'b') 3 []
    traceUnionMap (toV 'c') um === UnionMapTraceResFound (toV 'b') 3 []
    valuesUnionMap um === mapV [('a', 1), ('b', 3)]
  applyTestS equivUnionMapM $ \(fwd, bwd) _ -> do
    fwd === multiMapVV [('a', ""), ('b', "c")]
    bwd === mapVV [('c', 'b')]
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'a') (toV 'b')) $ \res um -> do
    res === UnionMapMergeOneValMerged (toV 'a') 3 ChangedYes
    sizeUnionMap um === 3
    traceUnionMap (toV 'a') um === UnionMapTraceResFound (toV 'a') 3 []
    traceUnionMap (toV 'b') um === UnionMapTraceResFound (toV 'a') 3 []
    traceUnionMap (toV 'c') um === UnionMapTraceResFound (toV 'a') 3 [toV 'b']
    valuesUnionMap um === mapV [('a', 3)]
  applyTestS equivUnionMapM $ \(fwd, bwd) _ -> do
    fwd === multiMapVV [('a', "bc")]
    bwd === mapVV [('b', 'a'), ('c', 'a')]

testUmUnit :: TestTree
testUmUnit = testGroup "UM unit"
  [ testUmSimple
  , testUmRec
  ]

main :: IO ()
main = defaultMain $ testGroup "Uniter"
  [ testUmUnit
  ]
