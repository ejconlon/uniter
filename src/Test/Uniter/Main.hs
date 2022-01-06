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
import Uniter.UnionMap (Changed (..), UnionMap, UnionMapAddVal (..), UnionMapLookupVal (..), UnionMapMergeVal (..),
                        UnionMapTraceRes (..), UnionMergeOne, addUnionMapM, concatUnionMergeOne, emptyUnionMap,
                        equivUnionMapM, lookupUnionMapM, mergeOneUnionMapM, sizeUnionMap, traceUnionMap, valuesUnionMap)

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
  applyTestS equivUnionMapM $ \(fwd, bwd) _ -> do
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
  applyTestS equivUnionMapM $ \(fwd, bwd) _ -> do
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
  applyTestS equivUnionMapM $ \(fwd, bwd) _ -> do
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
  applyTestS equivUnionMapM $ \(fwd, bwd) _ -> do
    fwd === multiMapVV [('a', ""), ('b', "c")]
    bwd === mapVV [('c', 'b')]
  applyTestS (mergeOneUnionMapM mergeOneUMV (toV 'a') (toV 'b')) $ \res um -> do
    res === UnionMapMergeValMerged (toV 'a') 3 ()
    sizeUnionMap um === 3
    traceUnionMap (toV 'a') um === UnionMapTraceResFound (toV 'a') 3 []
    traceUnionMap (toV 'b') um === UnionMapTraceResFound (toV 'a') 3 [toV 'b']
    traceUnionMap (toV 'c') um === UnionMapTraceResFound (toV 'a') 3 [toV 'b', toV 'c']
    valuesUnionMap um === mapV [('a', 3)]
  applyTestS equivUnionMapM $ \(fwd, bwd) _ -> do
    fwd === multiMapVV [('a', "bc")]
    bwd === mapVV [('b', 'a'), ('c', 'a')]

testUmTail :: TestTree
testUmTail = testUnit "UM rec" $ runS emptyUMV $ do
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
  applyTestS equivUnionMapM $ \(fwd, bwd) _ -> do
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

main :: IO ()
main = defaultMain $ testGroup "Uniter"
  [ testUmUnit
  ]
