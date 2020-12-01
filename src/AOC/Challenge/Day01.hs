-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import           AOC.Solver    ((:~>)(..))
import           Control.Monad (guard)
import           Data.List     (tails)
import           Data.Maybe    (listToMaybe)
import           Text.Read     (readMaybe)

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = \xs -> listToMaybe do
        x:ys <- tails xs
        y    <- ys
        guard (x + y == 2020)
        pure (x*y)
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = \xs -> listToMaybe do
        x:ys <- tails xs
        y:zs <- tails ys
        z    <- zs
        guard (x + y + z == 2020)
        pure (x*y*z)
    }
