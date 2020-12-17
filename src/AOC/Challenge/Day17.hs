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

import           AOC.Common      (fullNeighbsSet, (!!!), asciiGrid)
import           Debug.Trace
import           Data.Foldable (toList)
import           Control.Applicative (liftA2)
import           AOC.Solver      ((:~>)(..))
import           Control.Lens
-- import           Control.Lens    (to, set, asIndex, filtered)
import           Data.Set        (Set)
import           Data.Set.Lens   (setOf)
import           Linear          (R2(..), V3(..), V4(..))
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

stepper
    :: (Applicative f, Num a, Ord (f a), Eq a, Traversable f, R2 f)
    => Set (f a)
    -> Set (f a)
stepper cs = stayAlive <> comeAlive
  where
    neighborCounts = M.unionsWith ((+) @Int) $
      [ M.fromSet (weight c) (S.map (abszy) (fullNeighbsSet c))
      | c <- S.toList cs
      ]
    stayAlive = M.keysSet . M.filter (\n -> n == 2 || n == 3) $
                  neighborCounts `M.restrictKeys` cs
    comeAlive = M.keysSet . M.filter (== 3) $
                  neighborCounts `M.withoutKeys`  cs
    weight x y = product (zipWith (\i j -> if i == 1 && j == 0 then 2 else 1) xs ys)
      where
        xs = drop 2 (toList x)
        ys = drop 2 (toList y)
    abszy x = set _xy (view _xy x) (fmap abs x)

day17
    :: (Applicative f, R2 f, Ord (f Int), Traversable f, Show (f Int))
    => Set (f Int) :~> Int
day17 = MkSol
    { sParse = Just . parseMap
    , sShow  = show
    , sSolve = Just . S.size . foldMap dupSym  . (!!! 6) . iterate stepper
    }
  where
    dupSym x = S.fromList
      [ liftA2 (*) x p
      | p <- set _xy 1 <$> sequence (pure [1,-1])
      ]
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

