-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day03 (
    day03a
  , day03b
  , validCoord
  ) where

import           AOC.Common  (countTrue)
import           AOC.Solver  ((:~>)(..))
import           Data.Char   (isSpace)
import           Data.Finite (Finite, modulo)

type Coord = (Finite 31, Int)

validCoord
    :: Finite 31        -- ^ dx
    -> Int              -- ^ dy
    -> Coord
    -> Bool
validCoord dx dy = \(x,y) ->
    let (i,r) = y `divMod` dy
    in  r == 0 && dx * modulo (fromIntegral i) == x

countLine :: Finite 31 -> Int -> String -> Int
countLine dx dy = countTrue (uncurry tree)
                . zip (splitOut <$> [0..])
  where
    checkCoord = validCoord dx dy
    tree xy c  = c == '#' && checkCoord xy
    splitOut i = (fromIntegral x, y)
      where
        (!y, !x) = i `divMod` 31

day03a :: String :~> Int
day03a = MkSol
    { sParse = Just . filter (not . isSpace)
    , sShow  = show
    , sSolve = Just . countLine 3 1
    }

day03b :: String :~> Int
day03b = MkSol
    { sParse = Just . filter (not . isSpace)
    , sShow  = show
    , sSolve = \s -> Just . product $
        [ countLine 1 1
        , countLine 3 1
        , countLine 5 1
        , countLine 7 1
        , countLine 1 2
        ] <*> [s]
    }
