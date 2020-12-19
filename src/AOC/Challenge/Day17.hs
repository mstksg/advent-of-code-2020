-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day17 (
    day17a
  , day17b
  , abszyy
  , weight
  , explode
  , testExplode
  , allZs
  , neighborWeights
  ) where

import           AOC.Common
import           AOC.Common               (fullNeighbsSet, (!!!), asciiGrid, foldMapParChunk)
import           AOC.Solver               ((:~>)(..))
import           Control.DeepSeq          (NFData)
import           Control.Lens
import           Control.Lens             (iover, traversed, set, to, asIndex, filtered)
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Coerce              (coerce)
import           Data.Foldable
import           Data.Foldable            (toList)
import           Data.List
import           Data.Map                 (Map)
import           Data.Maybe
import           Data.Semigroup           (Sum(..), First(..))
import           Data.Set                 (Set)
import           Data.Set.Lens            (setOf)
import           Data.These
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
     , Show a
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
      [ M.fromSet (getWeight c) (S.map symmer (fullNeighbsSet c))
      | c <- S.toList cs
      ]
    stayAlive = M.keysSet . M.filter (\n -> n == 2 || n == 3) $
                  neighborCounts `M.restrictKeys` cs
    comeAlive = M.keysSet . M.filter (== 3) $
                  neighborCounts `M.withoutKeys`  cs
    symmer = over (partsOf (traversed .> indices (> 1))) (sort . map abs)
    getWeight x y = zWeight + selfWeight
      where
        (x0, xs) = splitAt 2 $ toList x
        (y0, ys) = splitAt 2 $ toList y
        zWeight = fromMaybe 0 $
          M.lookup ys =<< M.lookup xs syms
        selfWeight
          | xs == ys && x0 /= y0  = 1
          | otherwise             = 0
{-# INLINE stepper #-}

abszyy :: (Traversable f, Num a, Ord a) => f a -> f a
abszyy = over (partsOf (traversed .> indices (> 1))) (reverse . sort . map abs)

absz :: (Traversable f, Num a, Ord a) => f a -> f a
absz = iover traversed (\i x -> if i > 1 then abs x else x)

testExplode :: (Traversable f, Num a, Ord a, Applicative f, Ord (f a)) => f a -> _
testExplode c = gatherSame
              . M.mapKeysWith const (first match2 . splitAt 2 . toList)
              . M.fromSet (weight True c)
              . S.map abszyy
              $ fullNeighbsSet c
  where
    match2 xs = xs == take 2 (toList c)

explode :: (Traversable f, Num a, Ord a, Applicative f, Ord (f a)) => f a -> _
explode c = gatherSame
          . M.mapKeysWith const (first match2 . splitAt 2 . toList)
          . M.mapKeysWith (+) abszyy
          . fmap (lookupFreq c)
          . M.fromSet (freqs . fullNeighbs)
          $ fullNeighbsSet c
  where
    match2 xs = xs == take 2 (toList c)

data Count = CNoCheck Int
           | CDepends (These Int Int)
  deriving (Show, Eq)

gatherSame :: Ord a => Map (Bool, a) Int -> Map a Count
gatherSame = fmap toCount
           . M.fromListWith (<>)
           . map (\((b,k), v) -> (k, if b then This (First v) else That (First v)))
           . M.toList
  where
    toCount = \case
      This (First v) -> CDepends $ This v
      That (First v) -> CDepends $ That v
      These (First u) (First v)
        | u == v -> CNoCheck u
        | otherwise -> CDepends (These u v)

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
      $ allZs mx n
  where
    deltas    = tail $ replicateM n [0,-1,1]
    neighbs p = S.fromList $ map (zipWith (+) p) deltas
    symmer    = sort . map abs

allZs
    :: (Ord a, Num a, Enum a)
    => a      -- ^ maximum
    -> Int    -- ^ length (dimension)
    -> Set [a]
allZs mx n = S.fromList . flip evalStateT 0 $ replicateM n go
  where
    go = StateT $ \i -> map dup [i .. mx]

-- weight
--     :: (Foldable f, Num a, Ord a, Eq (f a))
--     => Bool         -- ^ consider self-reflections
--     -> f a
--     -> f a
--     -> Int
-- weight ss x y
--     | ss && xs == ys =
--         if x0 == y0
--           then selfSym1 * selfSym2
--           else selfSym1 * selfSym2 + 1
--     -- selfSym1 * selfSym2 + (if x == y then 0 else 1)
--     | otherwise      = product (zipWith go xs ys) * sym2
--   where
--     (x0, xs) = splitAt 2 (toList x)
--     (y0, ys) = splitAt 2 (toList y)
--     go i j
--       | i /= 0 && j == 0 = 2
--       | otherwise        = 1
--     sym2
--       | not (allSame xs) && allSame ys = 2
--       | otherwise                      = 1
--     selfSym1 = sum (zipWith (\i j -> if i == j - 1 then 1 else 0) (drop 1 xs) xs)
--     selfSym2 = product (map (\i -> if i == 0 then 2 else 1) xs)
-- {-# INLINE weight #-}

-- finalWeight
--     :: (Foldable f, Num a, Ord a, Eq (f a))
--     => Map [a] (Map [a] Int)    -- ^ symmetry map
--     -> f a
--     -> Int
-- finalWeight syms = zWeight
--   where
--     (x0, xs) = splitAt 2 $ toList x
--     zWeight = fromMaybe 0 $
--       M.lookup ys =<< M.lookup xs syms

allSame :: Eq a => [a] -> Bool
allSame []     = True
allSame (x:xs) = all (== x) xs

day17
    :: forall f. (Applicative f, R2 f, Ord (f Int), Traversable f, NFData (f Int))
    => Set (f Int) :~> Int
day17 = MkSol
    { sParse = Just . parseMap
    , sShow  = show
    , sSolve = Just
             . sum
             . M.fromSet (finalWeight wts)
             . (!!! 6)
             . iterate (stepper wts)
    }
  where
    n   = length (pure @f ()) - 2
    wts = neighborWeights 6 n
{-# INLINE day17 #-}

day17a :: Set (V3 Int) :~> Int
day17a = day17

day17b :: Set (V.Vector 4 Int) :~> Int
day17b = day17

-- d=5: 5760 / 16736
-- d=6: 35936 / 95584
-- d=7: 178720 / 502240


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
