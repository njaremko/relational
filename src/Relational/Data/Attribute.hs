module Relational.Data.Attribute
  ( Attribute(..), mkAttribute
  )
where

import Relude

data Attribute = Attribute
  { relname :: Maybe Text,
    name :: Text
  }
  deriving stock (Eq, Ord, Show)

mkAttribute :: Text -> Attribute
mkAttribute name = Attribute {relname = mempty, name}