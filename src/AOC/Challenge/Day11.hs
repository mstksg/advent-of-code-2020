-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import           AOC.Common      (Point, boundingBox', inBoundingBox, fullNeighbs, fullNeighbsSet, parseAsciiMap, fixedPoint, countTrue)
import           AOC.Solver      ((:~>)(..))
import           Data.List       (find)
import           Data.Map        (Map)
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import           Linear          (V2(..))
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

seatRule
    :: Int                       -- ^ exit seat threshold
    -> Map Point (Set Point)     -- ^ neighbors for each point
    -> Map Point Bool
    -> Map Point Bool
seatRule thr nmp mp = M.intersectionWith go nmp mp
  where
    go neighbs = \case
      False -> not (any (mp M.!) neighbs)
      True  ->
        let onNeighbs = countTrue (mp M.!) neighbs
        in  not (onNeighbs >= thr)

solveWith
    :: Int                      -- ^ exit seat threshold
    -> Map Point (Set Point)    -- ^ neighbors for each point
    -> Map Point Bool           -- ^ initial state
    -> Int                      -- ^ equilibrium size
solveWith thr neighbs = countTrue id . fixedPoint (seatRule thr neighbs)

parseSeatMap :: String -> Map Point Bool
parseSeatMap = parseAsciiMap $ \case
    'L' -> Just False
    '#' -> Just True    -- not in the input, but just for completion's sake
    _   -> Nothing

-- | Get a map of points to all of those points' neighbors where there is
-- a seat. Should only need to be computed once.
lineOfSights1
    :: Set Point
    -> Map Point (Set Point)
lineOfSights1 pts = M.fromSet go pts
  where
    go p = fullNeighbsSet p `S.intersection` pts


day11a :: Map Point Bool :~> Int
day11a = MkSol
    { sParse = Just . parseSeatMap
    , sShow  = show
    , sSolve = \mp -> Just $
        let los = lineOfSights1 (M.keysSet mp)
        in  solveWith 4 los mp
    }

-- | Get a map of points to all of those points' visible neighbors. Should
-- only need to be computed once.
lineOfSights2
    :: V2 Point
    -> Set Point
    -> Map Point (Set Point)
lineOfSights2 bb pts = M.fromSet go pts
  where
    go p = S.fromList
         . mapMaybe (los p)
         $ fullNeighbs 0
    los p d = find (`S.member` pts)
            . takeWhile (inBoundingBox bb)
            . tail
            $ iterate (+ d) p

day11b :: Map Point Bool :~> Int
day11b = MkSol
    { sParse = Just . parseSeatMap
    , sShow  = show
    , sSolve = \mp -> do
        bb <- boundingBox' (M.keys mp)
        let los = lineOfSights2 bb (M.keysSet mp)
        pure $ solveWith 5 los mp
    }
