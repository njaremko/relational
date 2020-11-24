module Relational.Data.Elem (Elem(..)) where

import Relude

data Elem
  = ElemText Text
  | ElemInt Int
  | ElemBool Bool
  | ElemBlob ByteString
  | ElemVarChar Text Int
  | ElemDecimal Float
  deriving stock (Eq, Show, Ord)