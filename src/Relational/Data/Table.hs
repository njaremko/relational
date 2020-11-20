module Relational.Data.Table
  ( Relation (..),
    Algebra (..),
    example,
  )
where

import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Relude hiding (empty, filter, null, reduce)

-- newtype Heading = Heading {unHeading :: Map Text Int} deriving newtype (Eq, Semigroup, Monoid)
newtype Heading = Heading {unHeading :: Map Attribute Int} deriving newtype (Eq, Semigroup, Monoid)

newtype Tuple = Tuple {unTuple :: Vector Elem} deriving newtype (Eq, Semigroup, Monoid)

newtype Name = Name {unName :: Text} deriving newtype (Eq, Semigroup, Monoid, IsString)

data Attribute = Attribute
  { relname :: Maybe Text,
    name :: Text
  }
  deriving stock (Eq, Ord, Show)

mkAttribute :: Text -> Attribute
mkAttribute name = Attribute {relname = mempty, name}

data Relation = Relation
  { name :: Maybe Text,
    heading :: Heading,
    tuples :: Map Int Tuple
  }

newtype PrimaryKeyNumeric = PrimaryKeyNumeric Int deriving newtype (Eq, Ord, Enum, Num)

newtype PrimaryKeyText = PrimaryKeyText Text deriving newtype (Eq, Ord)

data Elem
  = ElemText Text
  | ElemInt Int
  | ElemBool Bool
  | ElemBlob ByteString
  | ElemVarChar Text Int
  | ElemDecimal Float
  deriving stock (Eq, Show)

class Algebra a where
  projection :: Heading -> a -> a
  union :: a -> a -> Maybe a
  cartesianProduct :: a -> a -> a
  selection :: (a -> a) -> a -> a

--   equiJoin :: Text -> Text -> a -> a -> a

prettyPrint :: Relation -> IO ()
prettyPrint (Relation _ heading tuples) = do
  printHeader heading
  traverse_ (\(Tuple x) -> print x) tuples
  where
    printHeader :: Heading -> IO ()
    printHeader (Heading h) = do
      print $
        fmap
          ( ( \Attribute {relname, name} ->
                fromMaybe "" relname <> "." <> name
            )
              . fst
          )
          (sortWith snd $ Map.assocs h)

example :: IO ()
example = do
  let heading1 = Heading $ Map.fromList [(mkAttribute "id", 0), (mkAttribute "name", 1), (mkAttribute "email", 2)]
      heading2 = Heading $ Map.fromList [(mkAttribute "id", 0), (mkAttribute "name", 1), (mkAttribute "email", 2)]
      tuples =
        Map.fromList $
          zip
            [0 ..] 
            [ Tuple $ Vector.fromList [ElemInt 0, ElemText "John", ElemText "john.smith@gmail.com"],
              Tuple $ Vector.fromList [ElemInt 1, ElemText "Adam", ElemText "adam.smith@gmail.com"]
            ]
      relation1 = Relation {heading = heading1, tuples, name = Just "apples"}
      relation2 = Relation {heading = heading2, tuples, name = Just "bees"}
  prettyPrint $ cartesianProduct relation1 relation2

indexFilter ::
  (Num b, Eq b) =>
  Vector a ->
  Vector b ->
  Vector a
indexFilter v idx = Vector.map fst (Vector.filter (\x -> elemV (snd x) idx) vectorMap)
  where
    vectorMap = Vector.zipWith (\a b -> (b, a)) (Vector.iterateN size (+ 1) 0) v
    size = Vector.length v
    elemV a = Vector.foldl (\acc x -> (x == a) || acc) False

naturalJoin :: Relation -> Relation -> Relation
naturalJoin Relation{} Relation{} = mempty
naturalJoin a b = a

instance Semigroup Relation where
  (<>) = naturalJoin

instance Monoid Relation where
  mempty =
    Relation
      { heading = mempty,
        tuples = mempty,
        name = mempty
      }

instance Algebra Relation where
  projection (Heading selectedHeaders) (Relation name (Heading headers) rows) =
    let newHeader = Map.intersection selectedHeaders headers
        newRows = Map.map (\(Tuple elems) -> Tuple $ indexFilter elems (Vector.fromList $ Map.elems newHeader)) rows
     in Relation {heading = Heading newHeader, tuples = newRows, name}

  union Relation {heading = header1, tuples = rows1} Relation {heading = header2, tuples = rows2} =
    if header1 == header2
      then Just $ Relation {heading = header1, tuples = rows1 <> rows2, name = mempty}
      else Nothing

  cartesianProduct
    (Relation relname1 (Heading leftHeader) rel1)
    (Relation relname2 (Heading rightHeader) rel2) =
      let e1 = (\(Tuple e) -> e) <$> Map.elems rel1
          e2 = repeat $ (\(Tuple e) -> e) <$> Map.elems rel2
          tuples =
            let leftWithAllRight = \(a1, a2) -> fmap (a1 <>) a2
             in mconcat (leftWithAllRight <$> zip e1 e2)
          maxIndex = foldr max 0 $ Map.elems leftHeader -- Find size of current tuples in relation
          leftWithRelName = Map.mapKeys (\Attribute {name} -> Attribute {relname = relname1, name}) leftHeader
          newHeading =
            Heading $
              Map.union leftWithRelName
                . Map.fromList -- Make a new map of attribute index for both relations
                . flip zip [maxIndex + 1 ..] -- Add new indexes for these attribute names
                . fmap -- Add relation name to attributes, if relevant
                  ( (\Attribute {name} -> (Attribute {relname = relname2, name}))
                      . fst
                  )
                . sortWith snd -- Sort attributes by tuple ordering
                . Map.assocs
                $ rightHeader
       in Relation
            { heading = newHeading,
              tuples =
                Map.fromList
                  . zip [0 ..]
                  . fmap Tuple
                  $ tuples,
              name = mempty
            }

  selection func = func