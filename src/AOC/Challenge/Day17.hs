-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day17 (
    day17a
  , day17b
  ) where

import           AOC.Common      (fullNeighbs, (!!!), asciiGrid)
import           AOC.Solver      ((:~>)(..))
import           Control.Lens    (to, set, asIndex, filtered)
import           Data.Set        (Set)
import           Data.Set.Lens   (setOf)
import           Linear          (R2(..), V3(..), V4(..))
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

stepper
    :: (Applicative f, Num a, Ord (f a), Traversable f)
    => Set (f a)
    -> Set (f a)
stepper cs = stayAlive <> comeAlive
  where
    neighborCounts = M.fromListWith ((+) @Int) $
      map (,1) . fullNeighbs =<< S.toList cs
    stayAlive = M.keysSet . M.filter (\n -> n == 2 || n == 3) $
                  neighborCounts `M.restrictKeys` cs
    comeAlive = M.keysSet . M.filter (== 3) $
                  neighborCounts `M.withoutKeys`  cs

day17
    :: (Applicative f, R2 f, Ord (f Int), Traversable f)
    => Set (f Int) :~> Int
day17 = MkSol
    { sParse = Just . parseMap
    , sShow  = show
    , sSolve = Just . S.size . (!!! 6) . iterate stepper
    }
{-# INLINE day17 #-}

day17a :: Set (V3 Int) :~> Int
day17a = day17

day17b :: Set (V4 Int) :~> Int
day17b = day17

parseMap
    :: (Applicative f, R2 f, Ord (f Int))
    => String
    -> Set (f Int)
parseMap = setOf $ asciiGrid
                 . filtered (== '#')
                 . asIndex
                 . to (\p -> set _xy p (pure 0))

