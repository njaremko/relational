module Relational.Example (example1, example2, example3, example) where

import Relational.Data.Relation
import qualified Data.Map as Map
import qualified Relational.Data.Attribute as Attribute
import qualified Data.Vector as Vector
import Relude 
import Relational.Data.Elem
import qualified Relational.Algebra as Algebra

example1 :: Relation
example1 = do
  let heading1 = Heading $ Map.fromList [(Attribute.mkAttribute "id", 0), (Attribute.mkAttribute "name", 1), (Attribute.mkAttribute "email", 2)]
      tuples =
        Map.fromList $
          zip
            [0 ..]
            [ Vector.fromList [ElemInt 0, ElemText "John", ElemText "john.smith@gmail.com"],
              Vector.fromList [ElemInt 1, ElemText "Adam", ElemText "adam.smith@gmail.com"]
            ]
  Relation {heading = heading1, tuples, name = Just "people"}

example2 :: Relation
example2 = do
  let heading1 = Heading $ Map.fromList [(Attribute.mkAttribute "id", 0), (Attribute.mkAttribute "model", 1), (Attribute.mkAttribute "person_id", 2)]
      tuples =
        Map.fromList $
          zip
            [0 ..]
            [  Vector.fromList [ElemInt 0, ElemText "Ford", ElemInt 1],
              Vector.fromList [ElemInt 1, ElemText "Chevy", ElemInt 1]
            ]
  Relation {heading = heading1, tuples, name = Just "cars"}

example3 :: IO ()
example3 = do
  let e = equiJoin (Attribute.mkAttribute "id", example1) (Attribute.mkAttribute "person_id", example2)
      r = selectionWithConst
              (Attribute.Attribute {relname=Just "people", name="id"}) (ElemInt 0) Algebra.Equal
              <$> e
  traverse_ prettyPrint e
  traverse_ prettyPrint r

example :: IO ()
example = do
  let heading1 = Heading $ Map.fromList [(Attribute.mkAttribute "id", 0), (Attribute.mkAttribute "name", 1), (Attribute.mkAttribute "email", 2)]
      heading2 = Heading $ Map.fromList [(Attribute.mkAttribute "id", 0), (Attribute.mkAttribute "name", 1), (Attribute.mkAttribute "email", 2)]
      tuples =
        Map.fromList $
          zip
            [0 ..]
            [ Vector.fromList [ElemInt 0, ElemText "John", ElemText "john.smith@gmail.com"],
              Vector.fromList [ElemInt 1, ElemText "Adam", ElemText "adam.smith@gmail.com"]
            ]
      relation1 = Relation {heading = heading1, tuples, name = Just "apples"}
      relation2 = Relation {heading = heading2, tuples, name = Just "bees"}
  prettyPrint $ cartesianProduct relation1 relation2
