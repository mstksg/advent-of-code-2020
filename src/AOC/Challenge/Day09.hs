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

import           AOC.Solver                        ((:~>)(..))
import           Control.Monad                     (guard)
import           Data.List                         (scanl', tails, find)
import           Text.Read                         (readMaybe)
import qualified Data.Vector                       as V

isGood :: [Int] -> Bool
isGood (reverse->(x:xs)) = not . null $ do
    y:ys <- tails xs
    z <- ys
    guard $ (y + z) == x
isGood _ = False

oddOneOut :: [Int] -> Maybe Int
oddOneOut = fmap last . find (not . isGood) . map (take 26) . tails

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
        goal <- oddOneOut ns
        let nseq = V.fromList (scanl' (+) 0 ns)
        (i, j) <- findBounds nseq goal
        let xs = take (j - i) . drop i $ ns
        pure (minimum xs, maximum xs)
    }
