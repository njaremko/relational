module Main (main) where

import Protolude hiding (Map, filter)
import Relational

data Customer = C {cid :: Int, name :: Text}

data Invoice = I {iid :: Int, cust :: Int, due :: Int, amount :: Int}

example :: Bag Customer -> Bag Invoice -> Bag (Bag Text, Bag Int)
example cs is =
  fmap (pair (fmap name, fmap amount))
    . cod
    $ pair (identity, filterBag ((< 5) . due))
      <$> merge (cs `indexBy` cid, is `indexBy` iid)
  where
    pair (f, g) (a, b) = (f a, g b)

main :: IO ()
main = do
  return ()
