module Relational.Data.Tuple (Tuple(..)) where

import Relude ( Eq, Show, Semigroup, Monoid )
import Relational.Data.Elem ( Elem )
import Data.Vector (Vector)


newtype Tuple = Tuple {unTuple :: Vector Elem} deriving newtype (Eq, Semigroup, Monoid, Show)
