-- |
-- Module      : AOC.Challenge.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day24 (
    day24a
  , day24b
  ) where

import           AOC.Common                           ((!!!))
import           AOC.Common.Point                     (Point)
import           AOC.Solver                           ((:~>)(..))
import           Data.Bits                            (xor)
import           Data.Functor                         ((<&>))
import           Data.Map                             (Map)
import           Data.Set                             (Set)
import           Linear.V2                            (V2(..))
import           Math.Geometry.Grid.HexagonalInternal (HexDirection(..))
import qualified Data.Map.Strict                      as M
import qualified Data.Set                             as S
import qualified Math.Geometry.Grid                   as G

neighbors :: Point -> Set Point
neighbors (V2 x y) = S.fromDistinctAscList
    [ V2 (x-1) y
    , V2 (x-1) (y+1)
    , V2 x     (y-1)
    , V2 x     (y+1)
    , V2 (x+1) (y-1)
    , V2 (x+1) y
    ]

toDirs :: String -> Maybe [HexDirection]
toDirs = \case
    [] -> Just []
    'w':ds -> (West:) <$> toDirs ds
    'e':ds -> (East:) <$> toDirs ds
    'n':'e':ds -> (Northeast:) <$> toDirs ds
    'n':'w':ds -> (Northwest:) <$> toDirs ds
    's':'e':ds -> (Southeast:) <$> toDirs ds
    's':'w':ds -> (Southwest:) <$> toDirs ds
    _ -> Nothing

hexOffset :: HexDirection -> Point
hexOffset = \case
    West      -> V2 (-1)  0
    Northwest -> V2 (-1)  1
    Northeast -> V2   0   1
    East      -> V2   1   0
    Southeast -> V2   1 (-1)
    Southwest -> V2   0 (-1)

initialize :: [[HexDirection]] -> Set Point
initialize = M.keysSet . M.filter id . M.fromListWith xor
           . map ((,True) . sum . map hexOffset)

day24a :: [[HexDirection]] :~> Int
day24a = MkSol
    { sParse = traverse toDirs . lines
    , sShow  = show
    , sSolve = Just . S.size . initialize
    }

day24b :: [[HexDirection]] :~> Int
day24b = MkSol
    { sParse = traverse toDirs . lines
    , sShow  = show
    , sSolve = Just . S.size . (!!! 100) . iterate step . initialize
    }

step :: Set Point -> Set Point
step ps = stayAlive <> comeAlive
  where
    neighborCounts :: Map Point Int
    neighborCounts = M.unionsWith (+) $
      S.toList ps <&> \p ->
        M.fromSet (const 1) (neighbors p)
    stayAlive = M.keysSet . M.filter (\n -> n == 1 || n == 2) $
                  neighborCounts `M.restrictKeys` ps
    comeAlive = M.keysSet . M.filter (== 2) $
                  neighborCounts `M.withoutKeys`  ps
