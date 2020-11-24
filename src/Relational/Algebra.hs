module Relational.Algebra (Algebra (..)) where

import Relational.Data.Attribute (Attribute)
import Relational.Data.Heading (Heading)
import Relude

class Algebra a where
  project :: Heading -> a -> Either Text a
  union :: a -> a -> Either Text a
  naturalJoin :: a -> a -> a
  equiJoin :: (Attribute, a) -> (Attribute, a) -> Either Text a
  difference :: a -> a -> Either Text a
  cartesianProduct :: a -> a -> a
  selection :: (a -> a) -> a -> a