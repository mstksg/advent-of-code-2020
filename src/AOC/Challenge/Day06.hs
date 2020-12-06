-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC.Solver         ((:~>)(..))
import           Data.Char          (ord)
import           Data.IntSet        (IntSet)
import           Data.List.NonEmpty (NonEmpty)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (mapMaybe)
import qualified Data.IntSet        as IS
import qualified Data.List.NonEmpty as NE

type Answers = [NonEmpty IntSet]

answers :: String -> [NonEmpty IntSet]
answers = mapMaybe ((fmap . fmap) (IS.fromList . map ord) . NE.nonEmpty . lines)
        . splitOn "\n\n"

day06With
    :: (IntSet -> IntSet -> IntSet)
    -> Answers :~> Int
day06With f = MkSol
    { sParse = Just . answers
    , sShow  = show
    , sSolve = Just . sum . map (IS.size . foldr1 f)
    }

day06a :: Answers :~> Int
day06a = day06With IS.union

day06b :: Answers :~> Int
day06b = day06With IS.intersection
