-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Common         (firstJust)
import           AOC.Solver         ((:~>)(..))
import           Control.Lens       (preview)
import           Control.Monad      (guard)
import           Data.IntSet        (IntSet)
import           Data.Maybe         (mapMaybe)
import           Numeric.Lens       (binary)
import qualified Data.IntSet        as IS
import qualified Data.List.NonEmpty as NE

seatId :: String -> Maybe Int
seatId = preview binary . map toBin
  where
    toBin = \case
      'B' -> '1'
      'F' -> '0'
      'R' -> '1'
      'L' -> '0'
      _   -> '🤡'

-- | Find the first missing item in a set
findHole :: IntSet -> Maybe Int
findHole ids = do
    (x, xs) <- IS.minView ids
    firstJust (\(a, b) -> a <$ guard (a /= b)) $
      zip [x+1 ..] (IS.toList xs)

day05a :: [String] :~> Int
day05a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = fmap maximum . NE.nonEmpty . mapMaybe seatId
    }

day05b :: [String] :~> Int
day05b = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = findHole . IS.fromList . mapMaybe seatId
    }
