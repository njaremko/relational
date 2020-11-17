module Relational.Data.Bag (merge, union, cartesianProduct, filter, reduce) where

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Protolude hiding (empty, filter, null, reduce)

merge :: (Monoid a, Ord a) => MultiSet (MultiSet a) -> MultiSet a
merge = MultiSet.map reduce

union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union x y = MultiSet.fromList (MultiSet.elems x <> MultiSet.elems y)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct s t = [(a, b) | a <- s, b <- t]

reduce :: Monoid m => MultiSet m -> m
reduce x = mconcat $ MultiSet.elems x

filter :: (a -> Bool) -> MultiSet a -> MultiSet a
filter = MultiSet.filter

class Monoid m => CMonoid m

instance Ord a => CMonoid (MultiSet a)