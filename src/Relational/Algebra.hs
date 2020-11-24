module Relational.Algebra (Algebra (..), BinaryOperation(..)) where

import Relational.Data.Attribute (Attribute)
import Relational.Data.Heading (Heading)
import Relude
import Relational.Data.Elem (Elem)
-- import Relational.BinaryOperation (BinaryOperation)

data BinaryOperation = 
    Equal | LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual

class Algebra a where
  project :: Heading -> a -> Either Text a
  union :: a -> a -> Either Text a
  naturalJoin :: a -> a -> a
  equiJoin :: (Attribute, a) -> (Attribute, a) -> Either Text a
  difference :: a -> a -> Either Text a
  cartesianProduct :: a -> a -> a
  selection :: Attribute -> Attribute -> BinaryOperation -> a -> a
  selectionWithConst :: Attribute -> Elem -> BinaryOperation -> a -> a