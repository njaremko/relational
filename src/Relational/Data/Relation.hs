module Relational.Data.Relation
  ( Relation (..),
    Algebra (..),
    Heading (..),
    prettyPrint,
  )
where

import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Relational.Algebra (Algebra (..))
import qualified Relational.Algebra as Algebra
import Relational.Data.Attribute (Attribute)
import qualified Relational.Data.Attribute as Attribute
import Relational.Data.Elem (Elem)
import Relational.Data.Heading (Heading (Heading), unHeading)
import Relude hiding (empty, filter, null, reduce)

-- newtype Heading = Heading {unHeading :: Map Text Int} deriving newtype (Eq, Semigroup, Monoid)

newtype Name = Name {unName :: Text} deriving newtype (Eq, Semigroup, Monoid, IsString)

data Relation = Relation
  { name :: Maybe Text,
    heading :: Heading,
    tuples :: Map Int (Vector Elem)
  }

newtype PrimaryKeyNumeric = PrimaryKeyNumeric Int deriving newtype (Eq, Ord, Enum, Num)

newtype PrimaryKeyText = PrimaryKeyText Text deriving newtype (Eq, Ord)

prettyPrint :: Relation -> IO ()
prettyPrint (Relation _ heading tuples) = do
  printHeader heading
  traverse_ print tuples
  where
    printHeader :: Heading -> IO ()
    printHeader (Heading h) = do
      print $
        fmap
          ( ( \Attribute.Attribute {relname, name} ->
                fromMaybe "" relname <> "." <> name
            )
              . fst
          )
          (sortWith snd $ Map.assocs h)

instance Semigroup Relation where
  (<>) = Algebra.naturalJoin

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
      let maxIndex = Map.foldr max 0 leftHeading
          leftWithRelName = Map.mapKeys (\attr -> attr {Attribute.relname = leftRelName}) leftHeading
          rightWithRelName =
            Map.fromList -- Make a new map of attribute index for both relations
              . flip zip [maxIndex + 1 ..] -- Add new indexes for these attribute names
              . fmap -- Add relation name to attributes, if relevant
                ( (\attr -> (attr {Attribute.relname = rightRelName}))
                    . fst
                )
              . sortWith snd -- Sort attributes by tuple ordering
              . Map.assocs
              $ rightHeading
      Heading $ Map.union leftWithRelName rightWithRelName

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

instance Algebra Relation where
  project :: Heading -> Relation -> Either Text Relation
  project (Heading selectedHeaders) (Relation name (Heading headers) rows) =
    let newHeader = Map.intersection selectedHeaders headers
        newRows = Map.map (\elems -> indexFilter elems (Vector.fromList $ Map.elems newHeader)) rows
     in if Map.null newHeader
          then Left "No overlap in headings provided. Projection is not possible."
          else Right $ Relation {heading = Heading newHeader, tuples = newRows, name}

  union :: Relation -> Relation -> Either Text Relation
  union Relation {heading = header1, tuples = rows1} Relation {heading = header2, tuples = rows2} =
    if header1 == header2
      then Right $ Relation {heading = header1, tuples = rows1 <> rows2, name = mempty}
      else Left "Heading's are not compatible"

  cartesianProduct :: Relation -> Relation -> Relation
  cartesianProduct
    (Relation relname1 (Heading leftHeader) rel1)
    (Relation relname2 (Heading rightHeader) rel2) =
      let e1 = Map.elems rel1
          e2 = repeat $ Map.elems rel2
          tuples =
            let leftWithAllRight = \(a1, a2) -> fmap (a1 <>) a2
             in mconcat (leftWithAllRight <$> zip e1 e2)
          newHeading = mergeHeadings (relname1, Heading leftHeader) (relname2, Heading rightHeader)
       in Relation
            { heading = newHeading,
              tuples =
                Map.fromList
                  . zip [0 ..]
                  $ tuples,
              name = mempty
            }
  selection :: Attribute -> Attribute -> Algebra.BinaryOperation -> Relation -> Relation
  selection a1 a2 op rel = rel

  selectionWithConst :: Attribute -> Elem -> Algebra.BinaryOperation -> Relation -> Relation
  selectionWithConst a v Algebra.Equal rel@Relation {tuples, heading} =
    fromMaybe rel $ do
      found <- Map.lookup a $ unHeading heading
      return $ rel {tuples = Map.filter (\tuple -> v == Vector.unsafeIndex tuple found) tuples}
  selectionWithConst a v op rel = rel

  naturalJoin :: Relation -> Relation -> Relation
  naturalJoin rel1 rel2 = rel1

  difference :: Relation -> Relation -> Either Text Relation
  difference rel1 rel2 = Left "Difference is not possible"

  equiJoin :: (Attribute, Relation) -> (Attribute, Relation) -> Either Text Relation
  equiJoin
    (attr1, leftRel)
    (attr2, rightRel) = do
      a2 <- getAttributeIndex attr1 leftRel
      a3 <- getAttributeIndex attr2 rightRel
      doMerge (a2, leftRel) (a3, rightRel)
      where
        getAttributeIndex :: Attribute -> Relation -> Either Text Int
        getAttributeIndex attr rel =
          let errorMsg =
                "Unable to find heading '" <> Attribute.name attr
                  <> maybe
                    " in relation."
                    (\n -> "' in relation '" <> n <> "'")
                    (name rel)
           in maybeToRight errorMsg . Map.lookup attr1 . unHeading $ heading leftRel

        handleFold :: (Elem, Vector Elem) -> Map Elem [Vector Elem] -> Map Elem [Vector Elem]
        handleFold (valElem, valTuple) acc =
          let x =
                maybe
                  [valTuple]
                  (valTuple :)
                  $ Map.lookup valElem acc
           in Map.insert valElem x acc

        doMerge :: (Int, Relation) -> (Int, Relation) -> Either Text Relation
        doMerge
          (buildIndex, Relation {name = buildName, heading = buildHeading})
          (probeIndex, Relation {name = probeName, heading = probeHeading, tuples = probeTuples}) =
            let joinMap =
                  foldr
                    handleFold
                    mempty
                    ( (\v -> (Vector.unsafeIndex v buildIndex, v))
                        <$> Map.elems (tuples leftRel)
                    )
                mergedHeadings = mergeHeadings (buildName, buildHeading) (probeName, probeHeading)
                mergedTuples = mconcat
                  . catMaybes
                  $ flip fmap (Map.assocs probeTuples) $ \(i, t) -> do
                    found <- Map.lookup (Vector.unsafeIndex t probeIndex) joinMap
                    return $ Map.fromList $ fmap (\tmp -> (i, tmp <> t)) found
             in Right $ Relation {name = mempty, heading = mergedHeadings, tuples = mergedTuples}