-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import           AOC.Common    (firstJust)
import           AOC.Solver    ((:~>)(..))
import           Control.Monad (guard)
import           Data.Set      (Set)
import           Text.Read     (readMaybe)
import qualified Data.Set      as S

-- | Given a number @n@ of items and a goal sum and a set of numbers to
-- pick from, finds the @n@ numbers in the set that add to the goal sum.
knapsack
    :: Int              -- ^ number of items n to pick
    -> Int              -- ^ goal sum
    -> Set Int          -- ^ set of options
    -> Maybe [Int]      -- ^ resulting n items that sum to the goal
knapsack 0 _    _  = Nothing
knapsack 1 goal xs
    | goal `S.member` xs = Just [goal]
    | otherwise          = Nothing
knapsack n goal xs = flip firstJust (S.toList xs) $ \x ->
    let goal'   = goal - x
        (_, ys) = S.split x xs
    in  (x:) <$> knapsack (n - 1) goal' ys

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = fmap product . knapsack 2 2020 . S.fromList
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = fmap product . knapsack 3 2020 . S.fromList
    }
