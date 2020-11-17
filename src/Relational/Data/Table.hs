module Relational.Data.Table
  ( Table (..),
  )
where

import qualified Data.Map as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import qualified Relational.Data.Bag as Bag
import Relude hiding (empty, filter, null, reduce)

newtype Table k v = Table {unTable :: Map k (MultiSet v)}

instance Ord k => Semigroup (Table k v) where
  (<>) (Table m1) (Table m2) = Table $ Map.union m1 m2

instance Ord k => Monoid (Table k v) where
  mempty = Table Map.empty

empty :: Map k v
empty = Map.empty

isEmpty :: Map k v -> Bool
isEmpty = Map.null

single :: (k, v) -> Map k v
single (k, v) = Map.singleton k v

merge :: Ord k => (Map k v, Map k v) -> Map k (v, v)
merge (m1, m2) = Map.unionWith combineTuples (duplicateValues m1) (duplicateValues m2)
  where
    duplicateValues :: Map k v -> Map k (v, v)
    duplicateValues m = fmap (\x -> (x, x)) m

    combineTuples :: (v, v) -> (v, v) -> (v, v)
    combineTuples (x1, _) (y1, _) = (x1, y1)

merge' :: Map k (v1, v2) -> (Map k v1, Map k v2)
merge' x = (fmap fst x, fmap snd x)

dom :: (Ord k) => Map k v -> MultiSet k
dom m = MultiSet.fromList $ Map.keys m

cod :: (Ord k, Ord v) => Map k v -> MultiSet v
cod t = reduce (fmap MultiSet.singleton t)

lookup :: Ord k => Map k v -> (k -> Maybe v)
lookup = flip Map.lookup

ix :: (Ord k, Ord v) => MultiSet (k, v) -> Map k (MultiSet v)
ix kvs = Map.fromList ((\(k, v) -> (k, MultiSet.fromList [v])) <$> MultiSet.elems kvs)

ix' :: (Ord k, Ord v) => Map k (MultiSet v) -> MultiSet (k, v)
ix' a = MultiSet.fromList [(k, v) | (k, vs) <- Map.assocs a, v <- MultiSet.elems vs]

indexBy :: (Ord k, Ord v) => MultiSet v -> (v -> k) -> Map k (MultiSet v)
s `indexBy` f = ix (MultiSet.map (\x -> (f x, x)) s)

reduce :: (Ord k, Ord v, Monoid v) => Map k v -> v
reduce = Bag.reduce . cod