module Relational.Data.Table
  ( Relation (..),
    Algebra (..),
    example,
  )
where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Relational.Data.Bag as Bag
import Relude hiding (empty, filter, null, reduce)

-- newtype Heading = Heading {unHeading :: Map Text Int} deriving newtype (Eq, Semigroup, Monoid)
newtype Heading = Heading {unHeading :: Map (Maybe Text) (Map Text Int)} deriving newtype (Eq, Semigroup, Monoid)

newtype Tuple = Tuple {unTuple :: Vector Elem} deriving newtype (Eq, Semigroup, Monoid)

newtype Name = Name {unName :: Text} deriving newtype (Eq, Semigroup, Monoid, IsString)

data Relation a = Relation
  { heading :: Heading,
    tuples :: Map a Tuple
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

prettyPrint :: Relation a -> IO ()
prettyPrint (Relation heading tuples) = do
  let header = fst <$> headerByVal heading
  putTextLn $ show header
  traverse_ (\(Tuple x) -> print x) tuples

example :: IO ()
example = do
  let heading1 = Heading $ Map.fromList [(Nothing, Map.fromList [("id", 0), ("name", 1), ("email", 2)])]
      heading2 = Heading $ Map.fromList [(Just "apples", Map.fromList [("id", 0), ("name", 1), ("email", 2)])]
      tuples =
        Map.fromList $
          zip
            ([0 ..] :: [PrimaryKeyNumeric])
            [ Tuple $ Vector.fromList [ElemInt 0, ElemText "John", ElemText "john.smith@gmail.com"],
              Tuple $ Vector.fromList [ElemInt 1, ElemText "Adam", ElemText "adam.smith@gmail.com"]
            ]
      relation1 = Relation {heading = heading1, tuples}
      relation2 = Relation {heading = heading2, tuples}
  prettyPrint $ cartesianProduct relation1 relation2

headerByVal :: Heading -> [(Text, Int)]
headerByVal (Heading h) = sortWith snd $ Map.assocs $ handleAliases h

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

instance Semigroup (Relation PrimaryKeyNumeric) where
  (<>) = (<>)

instance Monoid (Relation PrimaryKeyNumeric) where
  mempty =
    Relation
      { heading = mempty,
        tuples = mempty
      }

handleAliases :: Map (Maybe Text) (Map Text Int) -> Map Text Int
handleAliases as =
    Map.fromList
    . mconcat
    $ ((\ (alias, attributes)
      -> Bifunctor.first (resolveAlias alias <>)
           <$> Map.assocs attributes)
     <$> Map.assocs as)
    where
        resolveAlias :: Maybe Text -> Text
        resolveAlias Nothing = ""
        resolveAlias (Just alias) = alias <> "."

instance Algebra (Relation PrimaryKeyNumeric) where
  projection (Heading selectedHeaders) (Relation (Heading headers) rows) =
    let newHeader = Map.intersection selectedHeaders headers
        newRows = Map.map (\(Tuple elems) -> Tuple $ indexFilter elems (Vector.fromList $ Map.elems $ handleAliases newHeader)) rows
     in Relation {heading = Heading newHeader, tuples = newRows}

  union (Relation header1 rows1) (Relation header2 rows2) =
    if header1 == header2
      then Just $ Relation {heading = header1, tuples = rows1 <> rows2}
      else Nothing

  cartesianProduct
    (Relation (Heading h1) rel1)
    (Relation (Heading h2) rel2) =
      let e1 = (\(Tuple e) -> e) <$> Map.elems rel1
          e2 = repeat $ (\(Tuple e) -> e) <$> Map.elems rel2
          tuples =
            let leftWithAllRight = \(a1, a2) -> fmap (a1 <>) a2
             in mconcat (leftWithAllRight <$> zip e1 e2)
          maxIndex = foldr max 0 $ mconcat $ Map.elems h1
          leftHeader = handleAliases h1
          f = fmap fst -- Take just the attribute names
              . sortWith snd -- Sort by indexes
              . Map.assocs $ handleAliases h2 -- Get indexes of attributes in relation
          fdsa :: Map Text Int = Map.fromList $ zip f [maxIndex + 1 ..] -- Add new indexes for these attribute names
              

          asdf = Map.union leftHeader fdsa -- Make a new map of attribute index for both relations
          newHeading =
            Heading $ Map.fromList [(Nothing, asdf)]
              
       in Relation newHeading
            . Map.fromList
            . zip [0 ..]
            . fmap Tuple
            $ tuples
        

  selection func = func