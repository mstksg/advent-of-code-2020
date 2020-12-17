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

import           AOC.Common          (fullNeighbsSet, (!!!), asciiGrid)
import           Data.Map  (Map)
import           AOC.Solver          ((:~>)(..))
import           Control.Lens        (iover, traversed, set, to, asIndex, filtered)
import           Control.Monad
import           Data.Foldable       (toList)
import           Data.Set            (Set)
import           Data.Set.Lens       (setOf)
import           Linear              (R2(..), V3(..), V4(..))
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S

-- | Find the coordinates of live cells.
--
-- >>> parse [".#.", "..#", "###"]
-- [[1,0],[2,1],[0,2],[1,2],[2,2]]
parse :: [String] -> [(Int,Int)]
parse input = [(x,y) | (y,line) <- zip [0..] input, (x,'#') <- zip [0..] line]

day17a :: Set C3 :~> Int
day17a = MkSol
    { sParse = Just . S.fromList . map toC3 . parse . lines
    , sShow  = show
    , sSolve = Just . S.size . (!!! 6) . iterate step
    }

day17b :: Set C4 :~> Int
day17b = MkSol
    { sParse = Just . S.fromList . map toC4 . parse . lines
    , sShow  = show
    , sSolve = Just . S.size . (!!! 6) . iterate step
    }

-- | Determine if a cell should be alive in the next generation.
rule ::
  Ord a =>
  Set a {- ^ previous generation      -} ->
  a    {- ^ coordinate               -} ->
  Int   {- ^ live neighbor count      -} ->
  Bool  {- ^ alive in next generation -}
rule world c n = n == 3 || n == 2 && S.member c world

-- | Compute the next generation from the previous generation
step :: Co a => Set a -> Set a
step world
  = M.keysSet
  $ M.filterWithKey (rule world)
  $ M.unionsWith (+)
  $ map neighborCount
  $ S.toList world

data C3 = C3 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Eq, Ord)
data C4 = C4 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Eq, Ord)

toC3 :: (Int,Int) -> C3
toC3 (x,y) = C3 x y 0

toC4 :: (Int,Int) -> C4
toC4 (x,y) = C4 x y 0 0

neighborCount :: Co a => a -> Map a Int
neighborCount c = M.mapKeysMonotonic (add c) neighborhood

class Ord a => Co a where
  add          :: a -> a -> a
  neighborhood :: Map a Int

instance Co C3 where
  add (C3 a b c) (C3 x y z) = C3 (a+x) (b+y) (c+z)
  neighborhood = M.fromList [(C3 x y z,1) | [x,y,z] <- tail (replicateM 3 [0,-1,1])]

instance Co C4 where
  add (C4 a b c d) (C4 x y z w) = C4 (a+x) (b+y) (c+z) (d+w)
  neighborhood = M.fromList [(C4 x y z w,1) | [x,y,z,w] <- tail (replicateM 4 [0,-1,1])]

-- stepper
--     :: (Applicative f, Num a, Ord (f a), Ord a, Traversable f)
--     => Set (f a)
--     -> Set (f a)
-- stepper cs = stayAlive <> comeAlive
--   where
--     neighborCounts = M.unionsWith ((+) @Int) $
--       [ M.fromSet (weight c) (S.map abszy (fullNeighbsSet c))
--       | c <- S.toList cs
--       ]
--     stayAlive = M.keysSet . M.filter (\n -> n == 2 || n == 3) $
--                   neighborCounts `M.restrictKeys` cs
--     comeAlive = M.keysSet . M.filter (== 3) $
--                   neighborCounts `M.withoutKeys`  cs
--     abszy = iover traversed (\i -> if i > 1 then abs else id)
-- {-# INLINE stepper #-}

-- weight
--     :: (Foldable f, Num a, Ord a)
--     => f a
--     -> f a
--     -> Int
-- weight x y = product $
--     zipWith go (drop 2 (toList x)) (drop 2 (toList y))
--   where
--     go i j
--       | i > 0 && j == 0 = 2
--       | otherwise       = 1
-- {-# INLINE weight #-}

-- day17
--     :: (Applicative f, R2 f, Ord (f Int), Traversable f)
--     => Set (f Int) :~> Int
-- day17 = MkSol
--     { sParse = Just . parseMap
--     , sShow  = show
--     , sSolve = Just . sum . M.fromSet (`weight` pure 0)  . (!!! 6) . iterate stepper
--     }
-- {-# INLINE day17 #-}

-- day17a :: Set (V3 Int) :~> Int
-- day17a = day17

-- day17b :: Set (V4 Int) :~> Int
-- day17b = day17

-- parseMap
--     :: (Applicative f, R2 f, Ord (f Int))
--     => String
--     -> Set (f Int)
-- parseMap = setOf $ asciiGrid
--                  . filtered (== '#')
--                  . asIndex
--                  . to (\p -> set _xy p (pure 0))
