module Relational.Data.Table
  ( Relation (..),
  )
where

import qualified Data.Map as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Relational.Data.Bag as Bag
import Relude hiding (empty, filter, null, reduce)

newtype Header = Header {unHeader :: Map Text Int} deriving newtype (Eq)

data Relation a = Relation Header (Vector a)

data Elem = ElemText Text | ElemInt Int

data Person = Person {personName :: Text, personEmal :: Text}

class Algebra a where
  projection :: (a -> a) -> a -> a
  union :: a -> a -> Maybe a
  cartesianProduct :: a -> a -> a
  selection :: Header -> a -> a

indexFilter ::
  (Num b, Eq b, Enum b) =>
  Vector a ->
  Vector b ->
  Vector a
indexFilter v idx = Vector.map (fst) (Vector.filter (\x -> elemV (snd x) idx) vectorMap)
  where
    vectorMap = Vector.zipWith (\a b -> (b, a)) (Vector.iterateN size (+ 1) 0) v
    size = Vector.length v
    elemV a = Vector.foldl (\acc x -> if x == a then True else acc) False

instance Algebra (Relation a) where
  selection (Header selectedHeaders) (Relation (Header headers) rows) =
    let newHeader = Map.intersection selectedHeaders headers
        newRows = indexFilter rows (Vector.fromList $ Map.elems newHeader)
     in Relation (Header newHeader) newRows

  union (Relation header1 rows1) (Relation header2 rows2) = 
      if header1 == header2
          then Just $ Relation header1 $ rows1 <> rows2
          else Nothing

  cartesianProduct 

-- example :: Relation Elem
-- example = do
--   let x1 = Relation ["id", "name", "email"] $ Vector.fromList [ElemInt 1, ElemText "John", ElemText "Smith"]
--   let x2 = projection (\(Relation [h : headers] rows) -> Relation headers rows) x1
--   x2