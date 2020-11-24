module Relational.Data.Heading (Heading(..)) where

import Relude
import Relational.Data.Attribute (Attribute)

newtype Heading = Heading {unHeading :: Map Attribute Int} deriving newtype (Eq, Semigroup, Monoid)
