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
import           Data.Type.Nat
import           Debug.Trace
import           Text.Read     (readMaybe)
import qualified Data.Set      as S
import qualified Data.Vec.Lazy as Vec

-- | Given a goal sum and a set of numbers to pick from, finds the @n@
-- numbers in the set that add to the goal sum.  The number of items
-- desired is inferred from the desired length of the return type.
knapsack
    :: forall n. SNatI n
    => Int              -- ^ goal sum
    -> Set Int          -- ^ set of options
    -> Maybe (Vec.Vec ('S n) Int)      -- ^ resulting n items that sum to the goal
knapsack = case snat :: SNat n of
    SZ -> \goal xs ->
      if goal `S.member` xs
        then Just $ Vec.singleton goal
        else Nothing
    SS -> \goal xs -> flip firstJust (S.toList xs) $ \x ->
      let goal'   = goal - x
          (_, ys) = S.split x xs
      in  (x Vec.:::) <$> knapsack goal' ys

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = fmap product . knapsack @Nat1 2020 . S.fromList
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = fmap product . knapsack @Nat2 2020 . S.fromList
    }
