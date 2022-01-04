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
import Test.Uniter.State (applyS, applyTestS, gets, runS, testS)
import Uniter.UnionMap (UnionMap, UnionMapLookupVal (..), UnionMergeOne, addUnionMapS, emptyUnionMap, equivUnionMapS,
                        lookupUnionMapS, semigroupUnionMergeOne, sizeUnionMap, valuesUnionMap)

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
  testS $ \um -> do
    sizeUnionMap um === 0
  _ <- applyS (addUnionMapS (toV 'a') 1)
  testS $ \um -> do
    sizeUnionMap um === 1
  applyTestS (lookupUnionMapS (toV 'a')) $ \res _ ->
    res === UnionMapLookupValOk (toV 'a') 1
  _ <- applyS (addUnionMapS (toV 'b') 2)
  _ <- applyS (addUnionMapS (toV 'c') 3)
  testS $ \um -> do
    sizeUnionMap um === 3
  applyTestS (lookupUnionMapS (toV 'b')) $ \res _ ->
    res === UnionMapLookupValOk (toV 'b') 2
  applyTestS (lookupUnionMapS (toV 'c')) $ \res _ ->
    res === UnionMapLookupValOk (toV 'c') 3
  applyTestS (gets valuesUnionMap) $ \res _ ->
    res === mapV [('a', 1), ('b', 2), ('c', 3)]
  applyTestS equivUnionMapS $ \(fwd, bwd) _ -> do
    fwd === multiMapVV [('a', ""), ('b', ""), ('c', "")]
    bwd === mapVV []
  -- applyTestS (mergeOneUnionMap mergeOneUMV (toV 'a') (toV 'c')) $ \res ef -> do
  --   res === Just (toV 'a', setV "c")
  --   efRootsSize ef === 2
  --   efLeavesSize ef === 1
  --   efTotalSize ef === 3
  --   ILS.fromList (efRoots ef) === setV "ab"
  --   ILS.fromList (efLeaves ef) === setV "c"
  --   efFwd ef === multiMapV [('a', "c"), ('b', "")]
  --   efBwd ef === mapV [('c', 'a')]
  -- applyTestS (efMerge (toV 'c') (toV 'a')) $ \res _ -> res === Nothing
  -- applyTestS (efMerge (toV 'b') (toV 'z')) $ \res _ -> res === Nothing

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
