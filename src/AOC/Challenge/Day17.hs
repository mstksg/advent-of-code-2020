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

import           AOC.Common               (fullNeighbsSet, (!!!), asciiGrid, foldMapParChunk)
import           AOC.Solver               ((:~>)(..))
import           Control.DeepSeq          (NFData)
import           Control.Lens             (iover, traversed, set, to, asIndex, filtered)
import           Data.Coerce              (coerce)
import           Data.Foldable            (toList)
import           Data.Map                 (Map)
import           Data.Semigroup           (Sum(..))
import           Data.Set                 (Set)
import           Data.Set.Lens            (setOf)
import           Linear                   (R2(..), V3(..), V4(..))
import qualified Data.Map.Monoidal.Strict as MM
import qualified Data.Map.Strict          as M
import qualified Data.Set                 as S

stepper
    :: forall f a.
     ( Applicative f
     , Traversable f
     , Num a
     , Ord a
     , Ord (f a)
     , NFData (f a)
     )
    => Set (f a)
    -> Set (f a)
stepper cs = stayAlive <> comeAlive
  where
    neighborCounts :: Map (f a) Int
    neighborCounts = coerce (foldMapParChunk @(MM.MonoidalMap (f a) (Sum Int)) 10 id)
      [ M.fromSet (weight c) (S.map abszy (fullNeighbsSet c))
      | c <- S.toList cs
      ]
    stayAlive = M.keysSet . M.filter (\n -> n == 2 || n == 3) $
                  neighborCounts `M.restrictKeys` cs
    comeAlive = M.keysSet . M.filter (== 3) $
                  neighborCounts `M.withoutKeys`  cs
    abszy = iover traversed (\i x -> if i > 1 then abs x else x)
{-# INLINE stepper #-}


weight
    :: (Foldable f, Num a, Ord a)
    => f a
    -> f a
    -> Int
weight x y = product $
    zipWith go (drop 2 (toList x)) (drop 2 (toList y))
  where
    go i j
      | i > 0 && j == 0 = 2
      | otherwise       = 1
{-# INLINE weight #-}

day17
    :: (Applicative f, R2 f, Ord (f Int), Traversable f, NFData (f Int))
    => Set (f Int) :~> Int
day17 = MkSol
    { sParse = Just . parseMap
    , sShow  = show
    , sSolve = Just . sum . M.fromSet (`weight` pure 0)  . (!!! 6) . iterate stepper
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

-- stepper
--     :: forall f a. (Applicative f, Num a, Ord (f a), Ord a, Traversable f, NFData (f a), NFData (f Int))
--     => Set (f a)
--     -> Set (f a)
-- stepper cs = trace ("size: " <> show (S.size cs)) $
--              trace ("neighbor size: " <> show (M.size neighborCounts)) $
--     trace' "stayAlive" stayAlive <> trace' "comeAlive" comeAlive
--   where
--     neighborCounts :: Map (f a) Int
--     neighborCounts = coerce (foldMapParChunk @(MM.MonoidalMap (f a) (Sum Int)) 500 id)
--       [ M.fromSet (weight c) (S.map abszy (fullNeighbsSet c))
--       | c <- S.toList cs
--       ]
--     stayAlive = M.keysSet . M.filter (\n -> n == 2 || n == 3) $
--                   trace' ("raw stayAlive") $ neighborCounts `M.restrictKeys` cs
--     comeAlive = M.keysSet . M.filter (== 3) $
--                   trace' ("raw comeAlive") $ neighborCounts `M.withoutKeys`  cs
--     abszy = iover traversed (\i x -> if i > 1 then abs x else x)
-- {-# INLINE stepper #-}

    -- , sSolve = Just . sum . M.fromSet (`weight` pure 0)  . (!!! 6) . zipWith traceShow [0..] . iterate stepper
