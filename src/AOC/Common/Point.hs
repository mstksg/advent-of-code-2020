
module AOC.Common.Point (
  -- * Points
    Point
  , cardinalNeighbs
  , cardinalNeighbsSet
  , fullNeighbs
  , fullNeighbsSet
  , mannDist
  , mulPoint
  , lineTo
  -- * Directions
  , Dir(..)
  , parseDir
  , dirPoint
  , dirPoint'
  , rotPoint
  , mulDir
  , allDir
  , allDirSet
  -- * Orientations
  , D8(..)
  , mulD8
  , orientPoint
  , allD8
  , allD8Set
  -- * 2D Maps
  , memoPoint
  , boundingBox
  , boundingBox'
  , inBoundingBox
  , minCorner, minCorner'
  , shiftToZero
  , shiftToZero'
  , parseAsciiMap
  , asciiGrid
  , parseAsciiSet
  , ScanPoint(..)
  , displayAsciiMap
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Data.Char
import           Data.Finitary
import           Data.Foldable
import           Data.Group
import           Data.Hashable
import           Data.List.NonEmpty      (NonEmpty(..))
import           Data.Map                (Map)
import           Data.Map.Lens
import           Data.MemoCombinators    (Memo)
import           Data.Monoid
import           Data.Ord
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Set                (Set)
import           Data.Set.Lens
import           Data.Set.NonEmpty       (NESet)
import           Data.Tuple.Strict
import           GHC.Generics
import           Linear
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as M
import qualified Data.Map.NonEmpty       as NEM
import qualified Data.MemoCombinators    as Memo
import qualified Data.Set                as S
import qualified Data.Set.NonEmpty       as NES

-- | 2D Coordinate
type Point = V2 Int

-- | Find the minimum and maximum x and y from a collection of points.
--
-- Returns @'V2' (V2 xMin yMin) (V2 xMax yMax)@.
boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> V2 (g a)
boundingBox = (\(T2 (Ap mn) (Ap mx)) -> V2 (getMin <$> mn) (getMax <$> mx))
            . foldMap1 (\p -> T2 (Ap (Min <$> p)) (Ap (Max <$> p)))

-- | A version of 'boundingBox' that works for normal possibly-empty lists.
boundingBox' :: (Foldable f, Applicative g, Ord a) => f (g a) -> Maybe (V2 (g a))
boundingBox' = fmap boundingBox . NE.nonEmpty . toList

minCorner :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> g a
minCorner = fmap getMin . getAp . foldMap1 (Ap . fmap Min)

minCorner' :: (Foldable f, Applicative g, Ord a) => f (g a) -> Maybe (g a)
minCorner' = fmap minCorner . NE.nonEmpty . toList

-- | Shift corner to (0,0)
shiftToZero
    :: (Applicative f, Num a, Ord a)
    => NESet (f a) -> NESet (f a)
shiftToZero ps = NES.mapMonotonic (liftA2 subtract mn) ps
  where
    mn = minCorner ps

-- | Shift corner to (0,0)
shiftToZero'
    :: (Applicative f, Num a, Ord a)
    => Set (f a) -> Set (f a)
shiftToZero' ps = case minCorner' ps of
    Nothing -> ps
    Just mn -> S.mapMonotonic (liftA2 subtract mn) ps


inBoundingBox
    :: (Applicative g, Foldable g, Ord a)
    => V2 (g a)
    -> g a
    -> Bool
inBoundingBox (V2 mn mx) x = all id $ go <$> x <*> mn <*> mx
  where
    go x' mn' mx' = x' >= mn' && x' <= mx'


cardinalNeighbs :: Point -> [Point]
cardinalNeighbs p = (p +) <$> [ V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0 ]

cardinalNeighbsSet :: Point -> Set Point
cardinalNeighbsSet p = S.fromAscList . map (p +) $
  [ V2 (-1)   0
  , V2   0  (-1)
  , V2   0    1
  , V2   1    0
  ]

fullNeighbs
    :: (Applicative f, Num a, Traversable f)
    => f a
    -> [f a]
fullNeighbs p = tail
    [ liftA2 (+) p d
    | d <- sequence (pure [0,-1,1])
    ]
{-# INLINE fullNeighbs #-}

fullNeighbsSet
    :: (Applicative f, Num a, Ord (f a), Traversable f)
    => f a
    -> Set (f a)
fullNeighbsSet p = S.fromDistinctAscList $
    [ liftA2 (+) p d
    | d <- sequence (pure [-1,0,1])
    , d /= pure 0
    ]

memoPoint :: Memo Point
memoPoint = Memo.wrap (uncurry V2) (\(V2 x y) -> (x, y)) $
                Memo.pair Memo.integral Memo.integral

mannDist :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
mannDist x y = sum . abs $ x - y

-- | Treat as complex number multiplication. useful for rotations
mulPoint :: Num a => V2 a -> V2 a -> V2 a
mulPoint (V2 x y) (V2 u v) = V2 (x*u - y*v) (x*v + y*u)

data Dir = North | East | South | West
  deriving (Show, Eq, Ord, Generic, Enum)

instance Hashable Dir
instance NFData Dir
instance Finitary Dir

dirPoint :: Num a => Dir -> V2 a
dirPoint = \case
    North -> V2   0   1
    East  -> V2   1   0
    South -> V2   0 (-1)
    West  -> V2 (-1)  0

-- | 'dirPoint' but with inverted y axis
dirPoint' :: Num a => Dir -> V2 a
dirPoint' = \case
    North -> V2   0 (-1)
    East  -> V2   1   0
    South -> V2   0   1
    West  -> V2 (-1)  0

-- | Rotate a point by a direction
rotPoint :: Num a => Dir -> V2 a -> V2 a
rotPoint = \case
    North -> id
    East  -> \(V2 x y) -> V2   y  (-x)
    West  -> \(V2 x y) -> V2 (-y)   x
    South -> negate

parseDir :: Char -> Maybe Dir
parseDir = flip M.lookup dirMap . toUpper
  where
    dirMap = M.fromList [
        ('N', North) , ('E', East) , ('S', South) , ('W', West)
      , ('U', North) , ('R', East) , ('D', South) , ('L', West)
      ]

-- | Multiply headings, taking North as straight, East as clockwise turn,
-- West as counter-clockwise turn, and South as reverse.
--
-- Should be a commutative group; it's essentially complex number
-- multiplication like 'mulPoint', with North = 1, West = i.  The identity
-- is 'North' and the inverse is the opposite direction.
mulDir :: Dir -> Dir -> Dir
mulDir North = id
mulDir East  = \case North -> East
                     East  -> South
                     South -> West
                     West  -> North
mulDir South = \case North -> South
                     East  -> West
                     South -> North
                     West  -> East
mulDir West  = \case North -> West
                     East  -> North
                     South -> East
                     West  -> South

allDir :: NonEmpty Dir
allDir = North :| [ East .. ]

allDirSet :: NESet Dir
allDirSet = NES.fromDistinctAscList allDir

-- | '<>' is 'mulDir'.
instance Semigroup Dir where
    (<>) = mulDir
    stimes n x = case n `mod` 4 of
      1 -> x
      2 -> x <> x
      3 -> invert x
      _ -> North

instance Monoid Dir where
    mempty = North

instance Group Dir where
    invert = \case North -> North
                   East  -> West
                   South -> South
                   West  -> East
    pow = flip stimes

instance Abelian Dir

-- | Represents an orientation of a 2d tile.
data D8 = D8 { d8Rot :: !Dir, d8Flip :: !Bool }
  deriving (Show, Eq, Ord, Generic)

instance Hashable D8
instance NFData D8
instance Finitary D8

-- | '<>' is 'mulDir'.
instance Semigroup D8 where
    D8 x1 False <> D8 x2 y2 = D8 (x1 <> x2) y2
    D8 x1 True  <> D8 x2 y2 = D8 (x1 <> invert x2) (not y2)

instance Monoid D8 where
    mempty = D8 North False

instance Group D8 where
    invert (D8 x False) = D8 (invert x) False
    invert (D8 x True ) = D8 x          True

-- | @a `mulD8` b@ represents applying b, then a.
mulD8 :: D8 -> D8 -> D8
mulD8 = (<>)

allD8 :: NonEmpty D8
allD8 = D8 <$> allDir <*> (False :| [ True ])

allD8Set :: NESet D8
allD8Set = NES.fromDistinctAscList allD8

-- | Rotate and flip a point by a 'D8'
orientPoint :: Num a => D8 -> V2 a -> V2 a
orientPoint = \case
    D8 North False -> id
    D8 East  False -> \(V2 x y) -> V2   y  (-x)
    D8 West  False -> \(V2 x y) -> V2 (-y)   x
    D8 South False -> \(V2 x y) -> V2 (-x) (-y)
    D8 North True  -> \(V2 x y) -> V2 (-x)   y
    D8 East  True  -> \(V2 x y) -> V2   y    x
    D8 West  True  -> \(V2 x y) -> V2 (-y) (-x)
    D8 South True  -> \(V2 x y) -> V2   x  (-y)

-- -- | Rotate a point by a direction
-- rotPoint :: Num a => Dir -> V2 a -> V2 a
-- rotPoint = \case
--     North -> id
--     East  -> \(V2 x y) -> V2   y  (-x)
--     West  -> \(V2 x y) -> V2 (-y)   x
--     South -> negate



-- | It's 'Point', but with a newtype wrapper so we have an 'Ord' that
-- sorts by y first, then x
newtype ScanPoint = SP { _getSP :: Point }
  deriving (Eq, Show, Generic)
  deriving newtype Num

instance Hashable ScanPoint
instance NFData ScanPoint

instance Ord ScanPoint where
    compare = comparing (view _y . _getSP)
           <> comparing (view _x . _getSP)

parseAsciiMap
    :: (Char -> Maybe a)
    -> String
    -> Map Point a
parseAsciiMap f = toMapOf (asciiGrid <. folding f)

parseAsciiSet
    :: (Char -> Bool)
    -> String
    -> Set Point
parseAsciiSet f = setOf (asciiGrid . filtered f . asIndex)

asciiGrid :: IndexedTraversal Point String [a] Char a
asciiGrid = conjoined traverse $ \pcfa ->
      sequenceA
    . concat
    . zipWith (\y -> zipWith (\x -> indexed pcfa (V2 x y :: Point)) [0..]) [0..]
    . lines

displayAsciiMap
    :: Char             -- ^ default tile
    -> Map Point Char   -- ^ tile map
    -> String
displayAsciiMap d (NEM.IsNonEmpty mp) = unlines
    [ [ NEM.findWithDefault d (V2 x y) mp
      | x <- [xMin .. xMax]
      ]
    | y <- [yMin .. yMax]
    ]
  where
    V2 xMin yMin `V2` V2 xMax yMax = boundingBox $ NEM.keysSet mp
displayAsciiMap _ _ = ""


-- | Lattice points for line between points, not including endpoints
lineTo :: Point -> Point -> [Point]
lineTo p0 p1 = [ p0 + t *^ step | t <- [1 .. gcf  - 1] ]
  where
    d@(V2 dx dy) = p1 - p0
    gcf          = gcd dx dy
    step         = (`div` gcf) <$> d

