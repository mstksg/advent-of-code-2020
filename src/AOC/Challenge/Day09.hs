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

import           AOC.Common     (slidingWindows)
import           AOC.Solver     ((:~>)(..))
import           Control.Monad  (guard)
import           Data.Foldable  (toList)
import           Data.List      (scanl', tails, find)
import           Data.Sequence  (Seq(..))
import           Text.Read      (readMaybe)
import qualified Data.Vector    as V

isGood :: Seq Int -> Bool
isGood (xs :|> x) = not . null $ do
    y:ys <- tails (toList xs)
    z    <- ys
    guard $ (y + z) == x
isGood _          = False

oddOneOut :: [Int] -> Maybe Int
oddOneOut xs = do
    _ :|> x <- find (not . isGood) (slidingWindows 26 xs)
    pure x

day09a :: [Int] :~> Int
day09a = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = oddOneOut
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
        goal   <- oddOneOut ns
        let cumsum = V.fromList (scanl' (+) 0 ns)
        (i, j) <- findBounds cumsum goal
        let xs = take (j - i) . drop i $ ns
        pure (minimum xs, maximum xs)
    }
