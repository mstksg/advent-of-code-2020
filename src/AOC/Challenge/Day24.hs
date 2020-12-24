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

import           AOC.Common                           ((!!!), foldMapParChunk)
import           AOC.Common.Point                     (Point)
import           AOC.Solver                           ((:~>)(..))
import           Control.DeepSeq                      (NFData)
import           Data.Coerce                          (coerce)
import           Data.Map                             (Map)
import           Data.Semigroup                       (Sum(..))
import           Data.Set                             (Set)
import           GHC.Generics                         (Generic)
import           Linear.V2                            (V2(..))
import           Math.Geometry.Grid.HexagonalInternal (HexDirection(..))
import qualified Data.Map.Monoidal.Strict             as MM
import qualified Data.Map.Strict                      as M
import qualified Data.Set                             as S

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

newtype Xor = Xor { getXor :: Bool }
  deriving Generic
instance NFData Xor
instance Semigroup Xor where
    Xor x <> Xor y = Xor (x /= y)
instance Monoid Xor where
    mempty = Xor False

initialize :: [[HexDirection]] -> Set Point
initialize = M.keysSet . M.filter getXor . coerce
           . foldMapParChunk 125 go
  where
    go = MM.MonoidalMap . (`M.singleton` Xor True) . sum . map hexOffset

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
    neighborCounts = coerce $ foldMapParChunk 75
        (MM.MonoidalMap . M.fromSet (const (Sum (1 :: Int))) . neighbors)
        (S.toList ps)
    stayAlive = M.keysSet . M.filter (\n -> n == 1 || n == 2) $
                  neighborCounts `M.restrictKeys` ps
    comeAlive = M.keysSet . M.filter (== 2) $
                  neighborCounts `M.withoutKeys`  ps
