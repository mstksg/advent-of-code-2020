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

import           AOC.Solver         ((:~>)(..))
import           Control.Lens       (preview)
import           Data.Maybe         (mapMaybe)
import           Numeric.Lens       (binary)
import qualified Control.Foldl      as F
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

-- | Find the first missing item in the collection, in a single pass
findHole :: F.Fold Int (Maybe Int)
findHole = do
    mn <- F.minimum
    mx <- F.maximum
    sm  <- F.sum
    pure $
      missingItem <$> mn <*> mx <*> pure sm
  where
    missingItem mn mx sm = totalSum - sm
      where
        totalSum = mx*(mx+1)`div`2 - mn*(mn-1)`div`2

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
    , sSolve = F.fold findHole . mapMaybe seatId
    }
