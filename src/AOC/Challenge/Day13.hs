-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day13 (
    day13a
  , day13b
  ) where

import           AOC.Solver                       ((:~>)(..))
import           Data.Foldable                    (minimumBy)
import           Data.List.Split                  (splitOn)
import           Data.Maybe                       (mapMaybe)
import           Data.Ord                         (comparing)
import           Math.NumberTheory.Moduli.Chinese (chineseRemainder)
import           Text.Read                        (readMaybe)

day13a :: (Int, [Int]) :~> _
day13a = MkSol
    { sParse = \str -> do
        [l1, l2] <- pure $ lines str
        t0 <- readMaybe l1
        let xs = mapMaybe readMaybe (splitOn "," l2)
        pure (t0, xs)
    , sShow  = \(x,y) -> show $ x * y
    , sSolve = \(t0, xs) -> Just $ minimumBy (comparing snd)
            [ (x, waitTime)
            | x <- xs
            , let waitTime = (x - t0) `mod` x
            ]
    }

day13b :: _ :~> _
day13b = MkSol
    { sParse = Just
             . mapMaybe (traverse readMaybe)
             . zip [0,-1..]
             . splitOn ","
             . dropWhile (/= '\n')
    , sShow  = show
    , sSolve = chineseRemainder
    }
