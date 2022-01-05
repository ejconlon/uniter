module Uniter.UnionMap
  ( Changed (..)
  , maybeChanged
  , UnionEntry (..)
  , UnionMergeOne
  , UnionMergeMany
  , foldUnionMergeMany
  , semigroupUnionMergeOne
  , semigroupUnionMergeMany
  , UnionMap
  , emptyUnionMap
  , sizeUnionMap
  , memberUnionMap
  , toListUnionMap
  , valuesUnionMap
  , UnionMapAddRes (..)
  , addUnionMap
  , UnionMapAddVal (..)
  , addUnionMapS
  , UnionMapTraceRes (..)
  , traceUnionMap
  , UnionMapLookupRes (..)
  , lookupUnionMap
  , UnionMapLookupVal (..)
  , lookupUnionMapS
  , equivUnionMap
  , equivUnionMapS
  , UnionMapUpdateRes (..)
  , updateUnionMap
  , UnionMapUpdateVal (..)
  , updateUnionMapS
  , UnionMapMergeOneRes (..)
  , mergeOneUnionMap
  , UnionMapMergeOneVal (..)
  , mergeOneUnionMapS
  , UnionMapMergeManyRes (..)
  , mergeManyUnionMap
  , UnionMapMergeManyVal (..)
  , mergeManyUnionMapS
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, state)
import Data.Coerce (Coercible)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import GHC.Generics (Generic)
import Overeasy.IntLike.Map (IntLikeMap)
import qualified Overeasy.IntLike.Map as ILM
import Overeasy.IntLike.Set (IntLikeSet)
import qualified Overeasy.IntLike.Set as ILS

safeInit :: [a] -> [a]
safeInit xs =
  case xs of
    [] -> xs
    _ -> init xs

data Changed =
    ChangedYes
  | ChangedNo
  deriving stock (Eq, Show)

maybeChanged :: Maybe a -> Changed
maybeChanged = \case
  Nothing -> ChangedNo
  Just _ -> ChangedYes

data UnionEntry k v =
    UnionEntryLink !k
  | UnionEntryValue !v
  deriving stock (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

type UnionMergeOne e v = v -> v -> Either e v
type UnionMergeMany e v = Maybe v -> NonEmpty v -> Either e v

foldUnionMergeMany :: UnionMergeOne e v -> UnionMergeMany e v
foldUnionMergeMany f mv (y :| ys) = start where
  start =
    case mv of
      Nothing -> go y ys
      Just v -> go v (y:ys)
  go v = \case
    [] -> Right v
    w:ws ->
      case f v w of
        Right u -> go u ws
        e@(Left _) -> e

semigroupUnionMergeOne :: Semigroup v => UnionMergeOne e v
semigroupUnionMergeOne v w = Right (v <> w)

semigroupUnionMergeMany :: Semigroup v => UnionMergeMany e v
semigroupUnionMergeMany mv vs = Right (sconcat (maybe vs (<| vs) mv))

newtype UnionMap k v = UnionMap { unUnionMap :: IntLikeMap k (UnionEntry k v) }
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

emptyUnionMap :: UnionMap k v
emptyUnionMap = UnionMap ILM.empty

sizeUnionMap :: UnionMap k v -> Int
sizeUnionMap = ILM.size . unUnionMap

memberUnionMap :: Coercible k Int => k -> UnionMap k v -> Bool
memberUnionMap k = ILM.member k . unUnionMap

toListUnionMap :: Coercible k Int => UnionMap k v -> [(k, UnionEntry k v)]
toListUnionMap = ILM.toList . unUnionMap

valuesUnionMap :: Coercible k Int => UnionMap k v -> IntLikeMap k v
valuesUnionMap = foldl' go ILM.empty . toListUnionMap where
  go m (k, ue) =
    case ue of
      UnionEntryValue v -> ILM.insert k v m
      _ -> m

data UnionMapAddRes k v =
    UnionMapAddResAdded !(UnionMap k v)
  | UnionMapAddResExists
  deriving stock (Eq, Show)

addUnionMap :: Coercible k Int => k -> v -> UnionMap k v -> UnionMapAddRes k v
addUnionMap k v (UnionMap m) =
  case ILM.lookup k m of
    Nothing -> UnionMapAddResAdded (UnionMap (ILM.insert k (UnionEntryValue v) m))
    Just _ -> UnionMapAddResExists

data UnionMapAddVal =
    UnionMapAddValAdded
  | UnionMapAddValExists
  deriving stock (Eq, Show)

addUnionMapS :: Coercible k Int => k -> v -> State (UnionMap k v) UnionMapAddVal
addUnionMapS k v = state $ \u ->
  case addUnionMap k v u of
    UnionMapAddResAdded w -> (UnionMapAddValAdded, w)
    UnionMapAddResExists -> (UnionMapAddValExists, u)

data UnionMapTraceRes k v =
    UnionMapTraceResMissing !k
  | UnionMapTraceResFound !k !v ![k]
  deriving stock (Eq, Show)

traceUnionMap :: Coercible k Int => k -> UnionMap k v -> UnionMapTraceRes k v
traceUnionMap k (UnionMap m) = go [] k where
  go !acc j =
    case ILM.lookup j m of
      Nothing -> UnionMapTraceResMissing j
      Just link -> case link of
        UnionEntryLink kx -> go (j:acc) kx
        UnionEntryValue v -> UnionMapTraceResFound j v (safeInit acc)

data UnionMapLookupRes k v =
    UnionMapLookupResMissing !k
  | UnionMapLookupResFound !k !v !(Maybe (UnionMap k v))
  deriving stock (Eq, Show)

lookupUnionMap :: Coercible k Int => k -> UnionMap k v -> UnionMapLookupRes k v
lookupUnionMap k u = case traceUnionMap k u of
  UnionMapTraceResMissing kx -> UnionMapLookupResMissing kx
  UnionMapTraceResFound kr vr acc ->
    let mu = if null acc then Nothing else Just (foldl' (\(UnionMap n) kx -> UnionMap (ILM.insert kx (UnionEntryLink kr) n)) u acc)
    in UnionMapLookupResFound kr vr mu

data UnionMapLookupVal k v =
    UnionMapLookupValMissing !k
  | UnionMapLookupValOk !k !v !Changed
  deriving stock (Eq, Show)

lookupUnionMapS :: Coercible k Int => k -> State (UnionMap k v) (UnionMapLookupVal k v)
lookupUnionMapS k = state $ \u ->
  case lookupUnionMap k u of
    UnionMapLookupResMissing x -> (UnionMapLookupValMissing x, u)
    UnionMapLookupResFound x y mw -> (UnionMapLookupValOk x y (maybeChanged mw), fromMaybe u mw)

equivUnionMap :: Coercible k Int => UnionMap k v -> ((IntLikeMap k (IntLikeSet k), IntLikeMap k k), UnionMap k v)
equivUnionMap u = foldl' go ((ILM.empty, ILM.empty), u) (toListUnionMap u) where
  go ((fwd, bwd), w) (k, ue) =
    case ue of
      UnionEntryValue _ ->
        let fwd' = ILM.alter (Just . fromMaybe ILS.empty) k fwd
        in ((fwd', bwd), w)
      UnionEntryLink _ ->
        case lookupUnionMap k w of
          UnionMapLookupResMissing _ -> error "impossible"
          UnionMapLookupResFound r _ mw ->
            let fwd' = ILM.alter (Just . maybe (ILS.singleton k) (ILS.insert k)) r fwd
                bwd' = ILM.insert k r bwd
                w' = fromMaybe w mw
            in ((fwd', bwd'), w')

equivUnionMapS :: Coercible k Int => State (UnionMap k v) (IntLikeMap k (IntLikeSet k), IntLikeMap k k)
equivUnionMapS = state equivUnionMap

data UnionMapUpdateRes e k v =
    UnionMapUpdateResMissing !k
  | UnionMapUpdateResEmbed !e
  | UnionMapUpdateResAdded !(UnionMap k v)
  | UnionMapUpdateResUpdated !k !v !(UnionMap k v)
  deriving stock (Eq, Show)

updateUnionMap :: (Coercible k Int, Eq k) => UnionMergeOne e v -> k -> v -> UnionMap k v -> UnionMapUpdateRes e k v
updateUnionMap g k v u@(UnionMap m) = go1 where
  go1 = case lookupUnionMap k u of
    UnionMapLookupResMissing kx ->
      if k == kx
        then UnionMapUpdateResAdded (UnionMap (ILM.insert k (UnionEntryValue v) m))
        else UnionMapUpdateResMissing kx
    UnionMapLookupResFound kr vr mu -> go2 kr vr (fromMaybe u mu)
  go2 kr vr (UnionMap mr) =
    case g vr v of
      Left e -> UnionMapUpdateResEmbed e
      Right vg -> UnionMapUpdateResUpdated kr vg (UnionMap (ILM.insert kr (UnionEntryValue vg) mr))

data UnionMapUpdateVal e k v =
    UnionMapUpdateValMissing !k
  | UnionMapUpdateValEmbed !e
  | UnionMapUpdateValAdded
  | UnionMapUpdateValUpdated !k !v
  deriving stock (Eq, Show)

updateUnionMapS :: (Coercible k Int, Eq k) => UnionMergeOne e v -> k -> v -> State (UnionMap k v) (UnionMapUpdateVal e k v)
updateUnionMapS g k v = state $ \u ->
  case updateUnionMap g k v u of
  UnionMapUpdateResMissing x -> (UnionMapUpdateValMissing x, u)
  UnionMapUpdateResEmbed e -> (UnionMapUpdateValEmbed e, u)
  UnionMapUpdateResAdded w -> (UnionMapUpdateValAdded, w)
  UnionMapUpdateResUpdated x y w -> (UnionMapUpdateValUpdated x y, w)

data UnionMapMergeOneRes e k v =
    UnionMapMergeOneResMissing !k
  | UnionMapMergeOneResEmbed !e
  | UnionMapMergeOneResMerged !k !v !(Maybe (UnionMap k v))
  deriving stock (Eq, Show)

mergeOneUnionMap :: (Coercible k Int, Eq k) => UnionMergeOne e v -> k -> k -> UnionMap k v -> UnionMapMergeOneRes e k v
mergeOneUnionMap g k j u = goLookupK where
  doCompact kr mw acc =
    if null acc
      then mw
      else Just (UnionMap (foldl' (\m x -> ILM.insert x (UnionEntryLink kr) m) (unUnionMap (fromMaybe u mw)) acc))
  goLookupK = case lookupUnionMap k u of
    UnionMapLookupResMissing kx ->
      if k == kx
        then goAssign
        else UnionMapMergeOneResMissing kx
    UnionMapLookupResFound kr kv mw -> goLookupJ kr kv mw
  goAssign = case traceUnionMap j u of
    UnionMapTraceResMissing jx -> UnionMapMergeOneResMissing jx
    UnionMapTraceResFound jr jv jacc ->
      if k == jr
        then UnionMapMergeOneResMerged k jv (doCompact k Nothing jacc)
        else goUpdate k jv (doCompact k Nothing (jr:jacc))
  goLookupJ kr kv mw = case traceUnionMap j (fromMaybe u mw) of
    UnionMapTraceResMissing jx -> UnionMapMergeOneResMissing jx
    UnionMapTraceResFound jr jv jacc ->
      if kr == jr
        then UnionMapMergeOneResMerged kr kv (doCompact kr mw jacc)
        else case g kv jv of
          Left e -> UnionMapMergeOneResEmbed e
          Right gv -> goUpdate kr gv (doCompact kr mw (jr:jacc))
  goUpdate kr gv mw =
    let y = UnionMap (ILM.insert kr (UnionEntryValue gv) (unUnionMap (fromMaybe u mw)))
    in UnionMapMergeOneResMerged kr gv (Just y)

data UnionMapMergeOneVal e k v =
    UnionMapMergeOneValMissing !k
  | UnionMapMergeOneValEmbed !e
  | UnionMapMergeOneValMerged !k !v !Changed
  deriving stock (Eq, Show)

mergeOneUnionMapS :: (Coercible k Int, Eq k) => UnionMergeOne e v -> k -> k -> State (UnionMap k v) (UnionMapMergeOneVal e k v)
mergeOneUnionMapS g k j = state $ \u ->
  case mergeOneUnionMap g k j u of
  UnionMapMergeOneResMissing x -> (UnionMapMergeOneValMissing x, u)
  UnionMapMergeOneResEmbed e -> (UnionMapMergeOneValEmbed e, u)
  UnionMapMergeOneResMerged x y mw -> (UnionMapMergeOneValMerged x y (maybeChanged mw), fromMaybe u mw)

data UnionMapMergeManyRes e k v =
    UnionMapMergeManyResMissing !k
  | UnionMapMergeManyResEmbed !e
  | UnionMapMergeManyResEmpty
  | UnionMapMergeManyResMerged !k !v !(Maybe (UnionMap k v))
  deriving stock (Eq, Show)

mergeManyUnionMap :: (Coercible k Int, Eq k) => UnionMergeMany e v -> k -> [k] -> UnionMap k v -> UnionMapMergeManyRes e k v
mergeManyUnionMap g k js u = goLookupK where
  doCompact kr mw acc =
    if null acc
      then mw
      else Just (UnionMap (foldl' (\m x -> ILM.insert x (UnionEntryLink kr) m) (unUnionMap (fromMaybe u mw)) acc))
  goLookupK = case lookupUnionMap k u of
    UnionMapLookupResMissing kx ->
      if k == kx
        then goAssign
        else UnionMapMergeManyResMissing kx
    UnionMapLookupResFound kr kv mw -> goLookupJs kr kv mw
  doTraceJsRec kr xs vs mw =
    case xs of
      [] -> Right (reverse vs, mw)  -- reverse to put in lookup order
      y:ys -> case traceUnionMap y (fromMaybe u mw) of
        UnionMapTraceResMissing jx -> Left jx
        UnionMapTraceResFound jr jv jacc ->
          if kr == jr
            then doTraceJsRec kr ys vs (doCompact kr mw jacc)
            else doTraceJsRec kr ys (jv:vs) (doCompact kr mw (jr:jacc))
  doTraceJs kr mw = doTraceJsRec kr js [] mw
  goAssign =
    case doTraceJs k Nothing of
      Left jx -> UnionMapMergeManyResMissing jx
      Right (vs, mu) ->
        case vs of
          [] -> UnionMapMergeManyResEmpty
          p:ps ->
            case g Nothing (p :| ps) of
              Left e -> UnionMapMergeManyResEmbed e
              Right gv -> goUpdate k gv mu
  goLookupJs kr kv mw =
    case doTraceJs kr mw of
      Left jx -> UnionMapMergeManyResMissing jx
      Right (vs, mu) ->
        case vs of
          [] -> goUpdate kr kv mu
          p:ps ->
            case g (Just kv) (p :| ps) of
              Left e -> UnionMapMergeManyResEmbed e
              Right gv -> goUpdate kr gv mu
  goUpdate kr gv mw =
    let y = UnionMap (ILM.insert kr (UnionEntryValue gv) (unUnionMap (fromMaybe u mw)))
    in UnionMapMergeManyResMerged kr gv (Just y)

data UnionMapMergeManyVal e k v =
    UnionMapMergeManyValMissing !k
  | UnionMapMergeManyValEmbed !e
  | UnionMapMergeManyValEmpty
  | UnionMapMergeManyValMerged !k !v !Changed
  deriving stock (Eq, Show)

mergeManyUnionMapS :: (Coercible k Int, Eq k) => UnionMergeMany e v -> k -> [k] -> State (UnionMap k v) (UnionMapMergeManyVal e k v)
mergeManyUnionMapS g k js = state $ \u ->
  case mergeManyUnionMap g k js u of
    UnionMapMergeManyResMissing x -> (UnionMapMergeManyValMissing x, u)
    UnionMapMergeManyResEmbed e -> (UnionMapMergeManyValEmbed e, u)
    UnionMapMergeManyResEmpty -> (UnionMapMergeManyValEmpty, u)
    UnionMapMergeManyResMerged x y mw -> (UnionMapMergeManyValMerged x y (maybeChanged mw), fromMaybe u mw)
