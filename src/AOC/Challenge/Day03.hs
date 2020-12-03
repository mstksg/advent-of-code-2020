-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Common  (parseAsciiMap, countTrue)
import           AOC.Solver  ((:~>)(..))
import           Data.Finite (Finite, modulo)
import           Data.Set    (Set)
import           Linear      (V2(..))
import qualified Data.Map    as M
import qualified Data.Set    as S

maxY :: Int
maxY = 322

type Coord = (Finite 31, Int)

parseCoords :: String -> Set Coord
parseCoords = S.mapMonotonic (\(V2 x y) -> (fromIntegral x, y))
            . M.keysSet
            . parseAsciiMap (\case '#' -> Just (); _ -> Nothing)

countLine :: Finite 31 -> Int -> Set Coord -> Int
countLine dx dy s = flip countTrue [0..maxY] $ \i ->
    (modulo (fromIntegral i) * dx, i * dy) `S.member` s

day03a :: Set Coord :~> Int
day03a = MkSol
    { sParse = Just . parseCoords
    , sShow  = show
    , sSolve = Just . countLine 3 1
    }

day03b :: Set Coord :~> Int
day03b = MkSol
    { sParse = Just . parseCoords
    , sShow  = show
    , sSolve = \s -> Just . product $
        [ countLine 1 1
        , countLine 3 1
        , countLine 5 1
        , countLine 7 1
        , countLine 1 2
        ] <*> [s]
    }
