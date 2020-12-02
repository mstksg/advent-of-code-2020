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
  , knapsack
  ) where

import           AOC.Common       (firstJust)
import           AOC.Solver       ((:~>)(..))
import           Data.IntSet      (IntSet)
import           Data.Type.Nat    (Nat(..), Nat1, Nat2, SNat(..), SNatI(..))
import           Text.Read        (readMaybe)
import qualified Data.IntSet      as IS
import qualified Data.Vec.Lazy    as Vec

-- | Given a goal sum and a set of numbers to pick from, finds the @n@
-- numbers in the set that add to the goal sum.  The number of items
-- desired is inferred from the desired length of the return type.
knapsack
    :: forall n. SNatI n
    => Int                             -- ^ goal sum
    -> IntSet                          -- ^ set of options
    -> Maybe (Vec.Vec ('S n) Int)      -- ^ resulting n items that sum to the goal
knapsack = case snat :: SNat n of
    SZ -> \goal xs ->
      if goal `IS.member` xs
        then Just $ Vec.singleton goal
        else Nothing
    SS -> \goal xs -> flip firstJust (IS.toList xs) $ \x ->
      let goal'   = goal - x
          (_, ys) = IS.split x xs
      in  (x Vec.:::) <$> knapsack goal' ys

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = fmap product . knapsack @Nat1 2020 . IS.fromList
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = fmap product . knapsack @Nat2 2020 . IS.fromList
    }
