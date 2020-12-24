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

import           AOC.Common                           (symDiff, (!!!))
import           AOC.Common.Point                     (Point)
import           AOC.Solver                           ((:~>)(..))
import           Data.Functor                         ((<&>))
import           Data.List                            (foldl')
import           Data.Map                             (Map)
import           Data.Set                             (Set)
import           Linear.V2                            (V2(..))
import           Math.Geometry.Grid.HexagonalInternal (HexDirection)
import qualified Data.Map                             as M
import qualified Data.Set                             as S
import qualified Math.Geometry.Grid                   as G
import qualified Math.Geometry.Grid.Hexagonal         as G
import qualified Math.Geometry.Grid.HexagonalInternal as G

neighbor :: Point -> HexDirection -> Point
neighbor (V2 x y) = maybe (error "what") (uncurry V2)
                  . G.neighbour G.UnboundedHexGrid (x, y)

neighbors :: Point -> Set Point
neighbors (V2 x y) = S.fromList $
    uncurry V2 <$> G.neighbours G.UnboundedHexGrid (x, y)

toDirs :: String -> Maybe [HexDirection]
toDirs = \case
    [] -> Just []
    'w':ds -> (G.West :) <$> toDirs ds
    'e':ds -> (G.East :) <$> toDirs ds
    'n':'e':ds -> (G.Northeast :) <$> toDirs ds
    'n':'w':ds -> (G.Northwest :) <$> toDirs ds
    's':'e':ds -> (G.Southeast :) <$> toDirs ds
    's':'w':ds -> (G.Southwest :) <$> toDirs ds
    _ -> Nothing

stepDs :: [HexDirection] -> Point
stepDs = go 0
  where
    go p []     = p
    go p (d:ds) = go (neighbor p d) ds

initialize :: [[HexDirection]] -> Set Point
initialize = foldl' symDiff S.empty . map (S.singleton . stepDs)

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
  where

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
