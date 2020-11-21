module Relational.Data.Table
  ( Relation (..),
    Algebra (..),
    example,
    example3,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Relude hiding (empty, filter, null, reduce)

-- newtype Heading = Heading {unHeading :: Map Text Int} deriving newtype (Eq, Semigroup, Monoid)
newtype Heading = Heading {unHeading :: Map Attribute Int} deriving newtype (Eq, Semigroup, Monoid)

newtype Tuple = Tuple {unTuple :: Vector Elem} deriving newtype (Eq, Semigroup, Monoid, Show)

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
  deriving stock (Eq, Show, Ord)

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

example1 :: Relation
example1 = do
  let heading1 = Heading $ Map.fromList [(mkAttribute "id", 0), (mkAttribute "name", 1), (mkAttribute "email", 2)]
      tuples =
        Map.fromList $
          zip
            [0 ..]
            [ Tuple $ Vector.fromList [ElemInt 0, ElemText "John", ElemText "john.smith@gmail.com"],
              Tuple $ Vector.fromList [ElemInt 1, ElemText "Adam", ElemText "adam.smith@gmail.com"]
            ]
  Relation {heading = heading1, tuples, name = Just "people"}

example2 :: Relation
example2 = do
  let heading1 = Heading $ Map.fromList [(mkAttribute "id", 0), (mkAttribute "model", 1), (mkAttribute "person_id", 2)]
      tuples =
        Map.fromList $
          zip
            [0 ..]
            [ Tuple $ Vector.fromList [ElemInt 0, ElemText "Ford", ElemInt 1],
              Tuple $ Vector.fromList [ElemInt 1, ElemText "Chevy", ElemInt 1]
            ]
  Relation {heading = heading1, tuples, name = Just "cars"}

example3 :: IO ()
example3 = do
  let e = equiJoin (mkAttribute "id", example1) (mkAttribute "person_id", example2)

  traverse_ prettyPrint e

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
naturalJoin Relation {} Relation {} = mempty
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

mergeHeadings :: (Maybe Text, Heading) -> (Maybe Text, Heading) -> Heading
mergeHeadings
  (leftRelName, Heading leftHeading)
  (rightRelName, Heading rightHeading) =
    do
      let maxIndex = foldr max 0 $ Map.elems leftHeading
          leftWithRelName = Map.mapKeys (\Attribute {name} -> Attribute {relname = leftRelName, name}) leftHeading
          rightWithRelName =
            Map.fromList -- Make a new map of attribute index for both relations
              . flip zip [maxIndex + 1 ..] -- Add new indexes for these attribute names
              . fmap -- Add relation name to attributes, if relevant
                ( (\Attribute {name} -> (Attribute {relname = rightRelName, name}))
                    . fst
                )
              . sortWith snd -- Sort attributes by tuple ordering
              . Map.assocs
              $ rightHeading
      Heading $ Map.union leftWithRelName rightWithRelName

class Algebra a where
  project :: Heading -> a -> Either Text a
  union :: a -> a -> Either Text a
  join :: a -> a -> Either Text a
  equiJoin :: (Attribute, a) -> (Attribute, a) -> Either Text a
  difference :: a -> a -> Either Text a
  cartesianProduct :: a -> a -> a
  selection :: (a -> a) -> a -> a

instance Algebra Relation where
  project (Heading selectedHeaders) (Relation name (Heading headers) rows) =
    let newHeader = Map.intersection selectedHeaders headers
        newRows = Map.map (\(Tuple elems) -> Tuple $ indexFilter elems (Vector.fromList $ Map.elems newHeader)) rows
     in if Map.null newHeader
          then Left "No overlap in headings provided. Projection is not possible."
          else Right $ Relation {heading = Heading newHeader, tuples = newRows, name}

  union Relation {heading = header1, tuples = rows1} Relation {heading = header2, tuples = rows2} =
    if header1 == header2
      then Right $ Relation {heading = header1, tuples = rows1 <> rows2, name = mempty}
      else Left "Heading's are not compatible"

  cartesianProduct
    (Relation relname1 (Heading leftHeader) rel1)
    (Relation relname2 (Heading rightHeader) rel2) =
      let e1 = (\(Tuple e) -> e) <$> Map.elems rel1
          e2 = repeat $ (\(Tuple e) -> e) <$> Map.elems rel2
          tuples =
            let leftWithAllRight = \(a1, a2) -> fmap (a1 <>) a2
             in mconcat (leftWithAllRight <$> zip e1 e2)
          newHeading = mergeHeadings (relname1, Heading leftHeader) (relname2, Heading rightHeader)
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

  join rel1 rel2 = Left "Join is not possible"

  difference rel1 rel2 = Left "Difference is not possible"

  equiJoin
    (attr1, leftRel)
    (attr2, rightRel) =
      maybeToRight "EquiJoin is not possible." $ do
        a2 <- Map.lookup attr1 $ unHeading $ heading leftRel
        a3 <- Map.lookup attr2 $ unHeading $ heading rightRel
        return $ doMerge (leftRel, a2) (rightRel, a3)
      where
        handleFold :: (Elem, Tuple) -> Map Elem [Tuple] -> Map Elem [Tuple]
        handleFold (valElem, valTuple) acc =
          let x =
                maybe
                  [valTuple]
                  (valTuple :)
                  $ Map.lookup valElem acc
           in Map.insert valElem x acc

        doMerge :: (Relation, Int) -> (Relation, Int) -> Relation
        doMerge
          (Relation {name = buildName, heading = buildHeading}, buildIndex)
          (Relation {name = probeName, heading = probeHeading, tuples = probeTuples}, probeIndex) = do
            let joinMap =
                  foldr
                    handleFold
                    mempty
                    ( (\(Tuple v) -> (Vector.unsafeIndex v buildIndex, Tuple v))
                        <$> Map.elems (tuples leftRel)
                    )
            let mergedHeadings = mergeHeadings (buildName, buildHeading) (probeName, probeHeading)
                mergedTuples = mconcat $
                  catMaybes $
                    flip fmap (Map.assocs probeTuples) $ \(i, Tuple t) -> do
                      found <- Map.lookup (Vector.unsafeIndex t probeIndex) joinMap
                      return $ Map.fromList $ fmap (\(Tuple tmp) -> (i, Tuple $ tmp <> t)) found

            Relation {name = mempty, heading = mergedHeadings, tuples = mergedTuples}