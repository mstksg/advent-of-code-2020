-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:

module AOC.Challenge.Day10 (
    day10a
  , day10b
  ) where

import           AOC.Common  (freqs, lookupFreq)
import           AOC.Solver  ((:~>)(..))
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import           Data.Maybe  (mapMaybe)
import           Text.Read   (readMaybe)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

toChain :: [Int] -> IntSet
toChain xs = xsset `IS.union` IS.fromList [0, top + 3]
  where
    xsset = IS.fromList xs
    top   = IS.findMax xsset

day10a :: [Int] :~> (Int, Int)
day10a = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = \(x,y) -> show (x * y)
    , sSolve = \(IS.toList . toChain->xs) -> Just
        let fs = freqs (zipWith (-) (drop 1 xs) xs)
        in  (lookupFreq 1 fs, lookupFreq 3 fs)
    }

toGraph :: IntSet -> IntMap IntSet
toGraph ss = IM.fromListWith (<>)
    [ (x, IS.singleton opt)
    | x <- IS.toList ss
    , opt <- [x+1,x+2,x+3]
    , opt `IS.member` ss
    ]

-- | A map of numbers to the count of how many paths from that number to
-- the goal
pathsToGoal :: IntMap IntSet -> IntMap Int
pathsToGoal is = res
  where
    res = flip IM.mapWithKey is $ \i ks ->
      if i == goal
        then 1
        else sum . mapMaybe (flip IM.lookup res) $ IS.toList ks
    (goal,_) = IM.findMax is

day10b :: [Int] :~> Int
day10b = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = Just . IM.findWithDefault 0 0 . pathsToGoal . toGraph . toChain
    }
