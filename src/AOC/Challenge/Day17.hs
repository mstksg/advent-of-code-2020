-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day17 (
    day17a
  , day17b
  -- , getWeight
  -- , neighborWeights
  -- , duplicands
  -- , finalWeight
  ) where

import           AOC.Common
import           AOC.Solver               ((:~>)(..))
import           Control.DeepSeq          (NFData)
import           Control.Lens
import           Control.Monad.State
import           Data.Coerce              (coerce)
import           Data.Foldable
import           Data.List
import           Data.Map                 (Map)
import           Data.Maybe
import           Data.Semigroup           (Sum(..))
import           Data.Set                 (Set)
import           Data.Set.Lens            (setOf)
import           Debug.Trace
import           Linear                   (R2(..), V3(..), V4(..))
import qualified Data.Map.Monoidal.Strict as MM
import qualified Data.Map.Strict          as M
import qualified Data.Set                 as S
import qualified Data.Vector.Sized        as V

stepper
    :: forall f a.
     ( Applicative f
     , Traversable f
     , Num a
     , Ord a
     , Ord (f a)
     , NFData (f a)
     )
    => Map [a] (Map [a] Int)    -- ^ symmetry map
    -> Set (f a)
    -> Set (f a)
stepper syms cs = stayAlive <> comeAlive
  where
    chnk :: Int
    chnk = min 1000 (max 10 (S.size cs `div` 100))
    neighborCounts :: Map (f a) Int
    neighborCounts = coerce (foldMapParChunk @(MM.MonoidalMap (f a) (Sum Int)) chnk id)
      [ M.fromSet (getWeight syms c) (S.map symmer (fullNeighbsSet c))
      | c <- S.toList cs
      ]
    stayAlive = M.keysSet . M.filter (\n -> n == 2 || n == 3) $
                  neighborCounts `M.restrictKeys` cs
    comeAlive = M.keysSet . M.filter (== 3) $
                  neighborCounts `M.withoutKeys`  cs
    symmer = over (partsOf (traversed .> indices (> 1))) (sort . map abs)
{-# INLINE stepper #-}

getWeight :: (Ord a, Foldable f) => Map [a] (Map [a] Int) -> f a -> f a -> Int
getWeight syms x y = zWeight + selfWeight
  where
    (x0, xs) = splitAt 2 $ toList x
    (y0, ys) = splitAt 2 $ toList y
    zWeight = fromMaybe 0 $
      M.lookup xs =<< M.lookup ys syms
    selfWeight
      | xs == ys && x0 /= y0  = 1
      | otherwise             = 0
{-# INLINE getWeight #-}

neighborWeights
    :: (Num a, Ord a, Enum a)
    => a            -- ^ maximum
    -> Int          -- ^ length (dimension)
    -> Map [a] (Map [a] Int)
neighborWeights mx n =
        M.fromSet ( M.mapKeysWith (+) symmer
                  . M.fromSet (const (1 :: Int))
                  . neighbs
                  )
      $ allZs
  where
    deltas    = tail $ replicateM n [0,-1,1]
    neighbs p = S.fromList $ map (zipWith (+) p) deltas
    symmer    = sort . map abs
    allZs     = S.fromList . flip evalStateT 0 $ replicateM n go
      where
        go = StateT $ \i -> map dup [i .. mx]

-- used to test finalWeights
_duplicands
    :: (Ord a, Num a, Enum a)
    => a      -- ^ maximum
    -> Int    -- ^ length (dimension)
    -> Map [a] Int
_duplicands mx n = freqs . map symmer $ replicateM n [-mx .. mx]
  where
    symmer    = sort . map abs

finalWeight
    :: (Foldable f, Num a, Ord a)
    => f a
    -> Int
finalWeight x = process . freqs . drop 2 . toList $ x
  where
    n = length x - 2
    process mp = (2 ^ numNonZeroes) * perms
      where
        numNonZeroes = n - lookupFreq 0 mp
        perms = factorial n
          `div` product (factorial <$> mp)

day17
    :: forall f. (Applicative f, R2 f, Ord (f Int), Traversable f, NFData (f Int))
    => Set (f Int) :~> Int
day17 = MkSol
    { sParse = Just . parseMap
    , sShow  = show
    , sSolve = Just
             . sum
             . M.fromSet finalWeight
             . (!!! 6)
             -- . zipWith traceShow [0..]
             . iterate (stepper wts)
    }
  where
    n   = length (pure @f ()) - 2
    wts = neighborWeights 6 n
{-# INLINE day17 #-}

day17a :: Set (V3 Int) :~> Int
day17a = day17

day17b :: Set (V4 Int) :~> Int
day17b = day17

-- d=5: 5760 / 16736; 274ms
-- d=6: 35936 / 95584; 1.5s
-- d=7: 178720 / 502240; 7.7s
-- d=8: ? / 2567360; 30s
-- d=9: 4333056 / 12764416; 2m20s
-- d=10: ? / 62771200; 8m58s
-- d=11: ? / 309176832; 43m54s

parseMap
    :: (Applicative f, R2 f, Ord (f Int))
    => String
    -> Set (f Int)
parseMap = setOf $ asciiGrid
                 . filtered (== '#')
                 . asIndex
                 . to (\p -> set _xy p (pure 0))
