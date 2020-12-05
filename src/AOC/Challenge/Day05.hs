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

import           AOC.Solver    ((:~>)(..))
import           Data.Bits     (complement, shiftR, (.&.))
import           Data.Char     (ord)
import           Data.List     (foldl')
import qualified Control.Foldl as F

seatId :: String -> Int
seatId = foldl' iGuessWe'reDoingThis 0
  where
    -- heh
    iGuessWe'reDoingThis n c =
      2 * n + (complement (ord c) `shiftR` 2) .&. 1

-- | Find the first missing item in the collection, in a single pass
findHole :: F.Fold Int (Maybe Int)
findHole = do
    mn <- F.minimum
    mx <- F.maximum
    sm <- F.sum
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
    , sSolve = F.fold (F.premap seatId F.maximum)
    }

day05b :: [String] :~> Int
day05b = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = F.fold (F.premap seatId findHole)
    }
