{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

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

import           AOC.Common                ((!!!), factorial, freqs, lookupFreq, dup, foldMapParChunk, sortSizedBy)
import           AOC.Common.Point          (asciiGrid)
import           AOC.Solver                ((:~>)(..))
import           Control.DeepSeq           (force)
import           Control.Lens              (to, asIndex, filtered)
import           Control.Monad             (replicateM)
import           Control.Monad.State       (StateT(..), evalStateT)
import           Data.Coerce               (coerce)
import           Data.List                 (sort)
import           Data.Map                  (Map)
import           Data.Maybe                (fromMaybe)
import           Data.Semigroup            (Sum(..))
import           Data.Set                  (Set)
import           Data.Set.Lens             (setOf)
import           GHC.TypeNats              (KnownNat, type (+))
import           Linear                    (V2(..))
import qualified Data.Map.Monoidal.Strict  as MM
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import qualified Data.Vector.Unboxed.Sized as V

-- pascals :: [[Int]]
-- pascals = repeat 1 : map (tail . scanl' (+) 0) pascals

stepper
    :: forall n a.
      ( Ord a
      , Num a
      , V.Unbox a
      )
    => Map (V.Vector n a) (Map (V.Vector n a) Int)    -- ^ symmetry map
    -> Set (V.Vector (n + 2) a)
    -> Set (V.Vector (n + 2) a)
stepper syms cs = stayAlive <> comeAlive
  where
    chnk :: Int
    chnk = min 1000 (max 10 (S.size cs `div` 100))
    neighborCounts :: Map (V.Vector (n + 2) a) Int
    neighborCounts = coerce (foldMapParChunk @(MM.MonoidalMap (V.Vector (n + 2) a) (Sum Int)) chnk id)
      [ M.fromSet (getWeight syms c) (S.fromList . map symmer $ neighbs c)
      | c <- S.toList cs
      ]
    stayAlive = M.keysSet . M.filter (\n -> n == 2 || n == 3) $
                  neighborCounts `M.restrictKeys` cs
    comeAlive = M.keysSet . M.filter (== 3) $
                  neighborCounts `M.withoutKeys`  cs
    symmer x = x0 V.++ xs'
      where
        (x0, xs) = V.splitAt @2 @n x
        xs'      = sortSizedBy compare . V.map abs $ xs

getWeight
    :: forall n a. (Ord a, V.Unbox a)
    => Map (V.Vector n a) (Map (V.Vector n a) Int)
    -> V.Vector (n + 2) a
    -> V.Vector (n + 2) a
    -> Int
getWeight syms x y = zWeight + selfWeight
  where
    (x0, xs) = V.splitAt @2 @n x
    (y0, ys) = V.splitAt @2 @n y
    zWeight = fromMaybe 0 $
      M.lookup xs =<< M.lookup ys syms
    selfWeight
      | xs == ys && x0 /= y0  = 1
      | otherwise             = 0

neighbs :: (Num a, V.Unbox a) => V.Vector n a -> [V.Vector n a]
neighbs p = tail $ V.mapM (\x -> [x,x-1,x+1]) p

neighborWeights
    :: (KnownNat n, Num a, Ord a, Enum a, V.Unbox a)
    => a            -- ^ maximum
    -> Map (V.Vector n a) (Map (V.Vector n a) Int)
neighborWeights mx =
        M.fromSet ( M.mapKeysWith (+) symmer
                  . M.fromList
                  . map (,1)
                  . neighbs
                  )
      $ allZs
  where
    symmer    = sortSizedBy compare . V.map abs
    allZs     = S.fromList $ evalStateT (V.replicateM go) 0
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
    :: (KnownNat n, Num a, Ord a, V.Unbox a)
    => V.Vector n a
    -> Int
finalWeight x = process . freqs . drop 2 . V.toList $ x
  where
    n = V.length x - 2
    process mp = (2 ^ numNonZeroes) * perms
      where
        numNonZeroes = n - lookupFreq 0 mp
        perms = factorial n
          `div` product (factorial <$> mp)

day17
    :: forall n. KnownNat n
    => Set (V.Vector (n + 2) Int) :~> Int
day17 = MkSol
    { sParse = Just . parseMap
    , sShow  = show
    , sSolve = Just . sum
             . M.fromSet finalWeight
             . (!!! 6) . iterate (force . stepper wts)
    }
  where
    wts = force $ neighborWeights 6
{-# INLINE day17 #-}

day17a :: Set (V.Vector 3 Int) :~> Int
day17a = day17

day17b :: Set (V.Vector 4 Int) :~> Int
day17b = day17

-- d=5: 5760 / 16736; 274ms     -- with unboxed, 96ms
-- d=6: 35936 / 95584; 1.5s     -- with unboxed, 309ms
-- d=7: 178720 / 502240; 7.7s
-- d=8: ? / 2567360; 30s
-- d=9: 4333056 / 12764416; 2m20s
-- d=10: ? / 62771200; 8m58s    -- with unboxed, 1m6s
-- d=11: ? / 309176832; 43m54s  -- with unboxed, 5m3s
-- d=12: ? / 1537981440 -- with unboxed, 22m10s

parseMap
    :: KnownNat n
    => String
    -> Set (V.Vector (n + 2) Int)
parseMap = setOf $ asciiGrid
                 . filtered (== '#')
                 . asIndex
                 . to (\(V2 x y) -> V.replicate 0 V.// [(0, x), (1, y)])
