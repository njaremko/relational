-- |
-- Copyright: (c) 2020 Nathan Jaremko
-- SPDX-License-Identifier: MIT
-- Maintainer: Nathan Jaremko <nathan@jaremko.ca>
--
-- See README for more info
module Relational
  ( Bag (..),
    Pointed (..),
    Key (..),
    filterBag,
    indexBy
  )
where

-- import qualified Data.Map as Map

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Vector as V
import Protolude hiding (Map, empty, null, reduce, filter)
import qualified Protolude

class Bifunctor p => Assoc p where
  assoc :: p (p a b) c -> p a (p b c)
  unassoc :: p a (p b c) -> p (p a b) c

instance Assoc (,) where
  assoc ((a, b), c) = (a, (b, c))
  unassoc (a, (b, c)) = ((a, b), c)

instance Assoc Either where
  assoc (Left (Left a)) = Left a
  assoc (Left (Right b)) = Right (Left b)
  assoc (Right c) = Right (Right c)

  unassoc (Left a) = Left (Left a)
  unassoc (Right (Left b)) = Left (Right b)
  unassoc (Right (Right c)) = Right c

newtype Bag a = Bag {elements :: [a]} deriving (Functor, Applicative, Monad)

mergeBag :: Monoid a => Bag (Bag a) -> Bag a
mergeBag = fmap reduceBag

unionBag :: Bag a -> Bag a -> Bag a
unionBag x y = Bag (elements x <> elements y)

crossBag :: [a] -> [b] -> [(a, b)]
crossBag s t = [(a,b) | a <- s, b <- t]

reduceBag :: Monoid m => Bag m -> m
reduceBag x = mconcat $ elements x

filterBag :: (a -> Bool) -> Bag a -> Bag a
filterBag p = reduceBag . fmap (guard p)
  where
    guard p a = if p a then return a else mempty

indexBy :: Key k => Bag v -> (v -> k) -> Map k (Bag v)
s `indexBy` f = ix (fmap (\x -> (f x, x)) s)

class Monoid m => CMonoid m

instance Semigroup (Bag a) where
  (<>) = unionBag

instance Monoid (Bag a) where
  mempty = Bag []

instance CMonoid (Bag a)

class Pointed a where
  null :: a
  isNull :: a -> Bool

instance Pointed () where
  null = ()
  isNull () = True

instance (Pointed a, Pointed b) => Pointed (a, b) where
  null = (null, null)
  isNull (a, b) = isNull a && isNull b

instance Pointed (Bag a) where
  null = mempty
  isNull = Protolude.null . elements

class (Functor (Map k)) => Key k where
  data Map k :: Type -> Type
  empty :: (Pointed v) => Map k v
  isEmpty :: (Pointed v) => Map k v -> Bool
  single :: (Pointed v) => (k, v) -> Map k v
  merge :: (Map k v1, Map k v2) -> Map k (v1, v2)
  merge' :: Map k (v1, v2) -> (Map k v1, Map k v2)
  merge' x = (fmap fst x, fmap snd x)
  dom :: (Pointed v) => Map k v -> Bag k
  cod :: (Pointed v) => Map k v -> Bag v
  cod t = reduce (fmap return t)
  lookup :: Map k v -> (k -> v)
  ix :: Bag (k, v) -> Map k (Bag v)
  ix' :: Map k (Bag v) -> Bag (k, v)
  reduce :: (Pointed v, CMonoid v) => Map k v -> v
  reduce = reduceBag . cod

instance Pointed (Maybe a) where
  null = Nothing
  isNull (Just _) = False
  isNull Nothing = True

instance Key () where
  newtype Map () v = Lone v
  empty = Lone null
  isEmpty (Lone v) = isNull v
  single ((), v) = Lone v
  merge (Lone v1, Lone v2) = Lone (v1, v2)
  dom (Lone v) = Bag [() | not (isNull v)]
  cod (Lone v) = Bag [v | not (isNull v)]
  lookup (Lone v) () = v
  ix kvs = Lone (fmap snd kvs)
  ix' (Lone vs) = fmap (\v -> ((), v)) vs

instance Functor (Map ()) where
  fmap f (Lone v) = Lone (f v)

instance Key Int where
  newtype Map Int v = A (V.Vector v)
  empty = A (V.accum (\_ x -> x) mempty [])
  isEmpty (A a) = all isNull a
  single (k, v) = A (V.accum (\_ x -> x) mempty [(k, v)])
  merge (A a, A b) = A (V.zip a b)
  dom (A a) = Bag [k | (k, v) <- V.toList $ V.indexed a, not (isNull v)]
  cod (A a) = Bag [v | (_, v) <- V.toList $ V.indexed a, not (isNull v)]
  lookup (A a) k = (V.!) a k
  ix kvs = A (V.accum (\xs x -> Bag (x : elements xs)) mempty (elements kvs))
  ix' (A a) = Bag [(k, v) | (k, vs) <- V.toList $ V.indexed a, v <- elements vs]

instance Functor (Map Int) where
  fmap f (A a) = A (fmap f a)

instance (Key k, Pointed v) => Pointed (Map k v) where
  null = empty
  isNull = isEmpty

instance (Key k1, Key k2) => Key (Either k1 k2) where
  newtype Map (Either k1 k2) v = Pair (Map k1 v, Map k2 v)
  empty = Pair (empty, empty)
  isEmpty (Pair (t1, t2)) = isEmpty t1 && isEmpty t2
  single (Left k1, v) = Pair (single (k1, v), empty)
  single (Right k2, v) = Pair (empty, single (k2, v))
  merge (Pair (t1, t2), Pair (u1, u2)) = Pair (merge (t1, u1), merge (t2, u2))
  dom (Pair (t1, t2)) = fmap Left (dom t1) <> fmap Right (dom t2)
  cod (Pair (t1, t2)) = cod t1 <> cod t2
  lookup (Pair (t1, _)) (Left k1) = lookup t1 k1
  lookup (Pair (_, t2)) (Right k2) = lookup t2 k2
  ix kvs = Pair (ix (fmap (\(Left k1, v) -> (k1, v)) kvs), ix (fmap (\(Right k2, v) -> (k2, v)) kvs))
  ix' (Pair (t1, t2)) = fmap (Bifunctor.first Left) (ix' t1) <> fmap (Bifunctor.first Right) (ix' t2)

instance (Functor (Map k1), Functor (Map k2)) => Functor (Map (Either k1 k2)) where
  fmap f (Pair (t1, t2)) = Pair (fmap f t1, fmap f t2)

instance (Key k1, Key k2) => Key (k1, k2) where
  newtype Map (k1, k2) v = Comp (Map k1 (Map k2 v))
  empty = Comp empty
  isEmpty (Comp t) = isEmpty t
  single ((k1, k2), v) = Comp (single (k1, single (k2, v)))
  merge (Comp t1, Comp t2) = Comp (fmap merge (merge (t1, t2)))
  dom (Comp t) = ix' (fmap dom t)
  cod (Comp t) = reduce (fmap cod t)
  lookup (Comp t) (k1, k2) = lookup (lookup t k1) k2
  ix kvs = Comp (fmap ix (ix (fmap assoc kvs)))
  ix' (Comp t1) = fmap unassoc (ix' (fmap ix' t1))

instance (Functor (Map k1), Functor (Map k2)) => Functor (Map (k1, k2)) where
  fmap f (Comp t) = Comp (fmap (fmap f) t)


newtype Table k v = Table { unTable :: Map k (Bag v) }

union x1 x2 = do
  let a1 = merge (x1, x2)
  let a2 = fmap (<>) a1
  () 