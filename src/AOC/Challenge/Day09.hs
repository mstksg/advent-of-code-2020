-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC.Common    (slidingWindows, firstJust)
import           AOC.Solver    ((:~>)(..), dyno_)
import           Control.Monad (guard)
import           Data.Foldable (toList)
import           Data.List     (scanl', tails)
import           Data.Sequence (Seq(..))
import           Text.Read     (readMaybe)
import qualified Data.Vector   as V

isBad :: Seq Int -> Maybe Int
isBad xs0 = do
    (xs :|> x) <- pure xs0
    let badCheck = null do
          y:ys <- tails (toList xs)
          z    <- ys
          guard $ (y + z) == x
    x <$ guard badCheck

oddOneOut :: Int -> [Int] -> Maybe Int
oddOneOut w = firstJust isBad . slidingWindows (w + 1)

day09a :: [Int] :~> Int
day09a = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = oddOneOut (dyno_ "window" 25)
    }

findBounds :: V.Vector Int -> Int -> Maybe (Int, Int)
findBounds ns goal = go 0 1
  where
    go !i !j = do
      x <- ns V.!? i
      y <- ns V.!? j
      case compare (y - x) goal of
        LT -> go i (j + 1)
        EQ -> pure (i, j)
        GT -> go (i + 1) j

day09b :: [Int] :~> (Int, Int)
day09b = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = \(x,y) -> show (x + y)
    , sSolve = \ns -> do
        goal   <- oddOneOut (dyno_ "window" 25) ns
        let cumsum = V.fromList (scanl' (+) 0 ns)
        (i, j) <- findBounds cumsum goal
        let xs = take (j - i) . drop i $ ns
        pure (minimum xs, maximum xs)
    }

-- an implementation using a priority search queue, which should have
-- efficient lookup and popping. but unfortunately it has too much overhead
-- to offer any overall advantage

-- isBad2 :: IntPSQ Int () -> Maybe Int
-- isBad2 q = do
--     (goal, _, _, xs) <- IntPSQ.minView q
--     let badCheck = null do
--           (x,_,_) <- IntPSQ.toList xs
--           let y = goal - x
--           guard $ y > x
--           guard $ y `IntPSQ.member` xs
--     goal <$ guard badCheck

-- oddOneOut2 :: Int -> [Int] -> Maybe Int
-- oddOneOut2 w = firstJust isBad2
--              . reverse
--              . sortedSlidingWindowsInt (w + 1)
--              . reverse
--              . map (,())

