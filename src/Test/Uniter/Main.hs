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
import Uniter.UnionMap (UnionMap, UnionMapAddVal (..), UnionMapLookupVal (..), UnionMapMergeVal (..),
                        UnionMapTraceRes (..), UnionMergeOne, addUnionMapS, emptyUnionMap, equivUnionMapS,
                        lookupUnionMapS, mergeOneUnionMapS, semigroupUnionMergeOne, sizeUnionMap, traceUnionMap,
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
  applyTestS (addUnionMapS (toV 'a') 1) $ \res um -> do
    res === UnionMapAddValAdded
    sizeUnionMap um === 1
    traceUnionMap (toV 'a') um === UnionMapTraceResFound (toV 'a') 1 []
    valuesUnionMap um === mapV [('a', 1)]
  -- lookup 'a'
  applyTestS (lookupUnionMapS (toV 'a')) $ \res _ ->
    res === UnionMapLookupValOk (toV 'a') 1
  -- try to add 'a' again
  applyTestS (addUnionMapS (toV 'a') 1) $ \res um -> do
    res === UnionMapAddValExists
    sizeUnionMap um === 1
  -- add 'b' and 'c' and check them
  _ <- applyS (addUnionMapS (toV 'b') 2)
  _ <- applyS (addUnionMapS (toV 'c') 3)
  testS $ \um -> do
    sizeUnionMap um === 3
    traceUnionMap (toV 'b') um === UnionMapTraceResFound (toV 'b') 2 []
    traceUnionMap (toV 'c') um === UnionMapTraceResFound (toV 'c') 3 []
    valuesUnionMap um === mapV [('a', 1), ('b', 2), ('c', 3)]
  applyTestS equivUnionMapS $ \(fwd, bwd) _ -> do
    fwd === multiMapVV [('a', ""), ('b', ""), ('c', "")]
    bwd === mapVV []
  -- merge 'a' and 'c'
  applyTestS (mergeOneUnionMapS mergeOneUMV (toV 'a') (toV 'c')) $ \res um -> do
    res === UnionMapMergeValOk (toV 'a') 3
    sizeUnionMap um === 3
    traceUnionMap (toV 'a') um === UnionMapTraceResFound (toV 'a') 3 []
    traceUnionMap (toV 'b') um === UnionMapTraceResFound (toV 'b') 2 []
    traceUnionMap (toV 'c') um === UnionMapTraceResFound (toV 'a') 3 []
    valuesUnionMap um === mapV [('a', 3), ('b', 2)]
  applyTestS equivUnionMapS $ \(fwd, bwd) _ -> do
    fwd === multiMapVV [('a', "c"), ('b', "")]
    bwd === mapVV [('c', 'a')]
  -- try to merge again
  applyTestS (mergeOneUnionMapS mergeOneUMV (toV 'a') (toV 'c')) $ \res _ ->
    res === UnionMapMergeValOk (toV 'a') 3
  -- and the other way around
  applyTestS (mergeOneUnionMapS mergeOneUMV (toV 'c') (toV 'a')) $ \res _ ->
    res === UnionMapMergeValOk (toV 'a') 3
  -- and a non-existent merge
  applyTestS (mergeOneUnionMapS mergeOneUMV (toV 'b') (toV 'z')) $ \res _ ->
    res === UnionMapMergeValMissing (toV 'z')
  -- and creating merge
  applyTestS (mergeOneUnionMapS mergeOneUMV (toV 'z') (toV 'b')) $ \res _ ->
    res === UnionMapMergeValOk (toV 'z') 2
  applyTestS equivUnionMapS $ \(fwd, bwd) _ -> do
    fwd === multiMapVV [('a', "c"), ('z', "b")]
    bwd === mapVV [('b', 'z'), ('c', 'a')]

-- testUfRec :: TestTree
-- testUfRec = testUnit "UF rec" $ runS efNew $ do
--   _ <- applyS (efAdd (toV 'a'))
--   _ <- applyS (efAdd (toV 'b'))
--   _ <- applyS (efAdd (toV 'c'))
--   applyTestS (efMerge (toV 'b') (toV 'c')) $ \res ef -> do
--     res === Just (toV 'b', setV "c")
--     efRootsSize ef === 2
--     efLeavesSize ef === 1
--     efTotalSize ef === 3
--     ILS.fromList (efRoots ef) === setV "ab"
--     ILS.fromList (efLeaves ef) === setV "c"
--     efFwd ef === multiMapV [('a', ""), ('b', "c")]
--     efBwd ef === mapV [('c', 'b')]
--   applyTestS (efMerge (toV 'a') (toV 'c')) $ \res ef -> do
--     res === Just (toV 'a', setV "bc")
--     efRootsSize ef === 1
--     efLeavesSize ef === 2
--     efTotalSize ef === 3
--     ILS.fromList (efRoots ef) === setV "a"
--     ILS.fromList (efLeaves ef) === setV "bc"
--     efFwd ef === multiMapV [('a', "bc")]
--     efBwd ef === mapV [('b', 'a'), ('c', 'a')]

-- testUfMany :: TestTree
-- testUfMany = testUnit "UF many" $ runS efNew $ do
--   _ <- applyS (efAdd (toV 'a'))
--   _ <- applyS (efAdd (toV 'b'))
--   _ <- applyS (efAdd (toV 'c'))
--   _ <- applyS (efAdd (toV 'd'))
--   _ <- applyS (efAdd (toV 'e'))
--   applyTestS (efMergeSets [setV "cde"]) $ \res ef -> do
--     res === Just (setV "c", setV "de")
--     efRootsSize ef === 3
--     efLeavesSize ef === 2
--     efTotalSize ef === 5
--     ILS.fromList (efRoots ef) === setV "abc"
--     ILS.fromList (efLeaves ef) === setV "de"
--     efFwd ef === multiMapV [('a', ""), ('b', ""), ('c', "de")]
--     efBwd ef === mapV [('d', 'c'), ('e', 'c')]
--   applyTestS (efMergeSets [setV "abd"]) $ \res ef -> do
--     res === Just (setV "a", setV "bcde")
--     efRootsSize ef === 1
--     efLeavesSize ef === 4
--     efTotalSize ef === 5
--     ILS.fromList (efRoots ef) === setV "a"
--     ILS.fromList (efLeaves ef) === setV "bcde"
--     efFwd ef === multiMapV [('a', "bcde")]
--     efBwd ef === mapV [('b', 'a'), ('c', 'a'), ('d', 'a'), ('e', 'a')]

testUmUnit :: TestTree
testUmUnit = testGroup "UM unit"
  [ testUmSimple
  ]

main :: IO ()
main = defaultMain $ testGroup "Uniter"
  [ testUmUnit
  ]
