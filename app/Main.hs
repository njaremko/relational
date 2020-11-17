module Main (main) where

import Relude hiding (Map, filter)
import Relational
-- import  Relational.Data.Table (Table)
-- import qualified Relational.Data.Table as Table
import qualified Data.Map as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

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
