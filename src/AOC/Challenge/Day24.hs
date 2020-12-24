{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day24 (
    day24a
  , day24b
  ) where

import           AOC.Prelude

-- import qualified Data.Graph.Inductive        as G
import qualified Math.Geometry.Grid             as G
import qualified Math.Geometry.Grid.Hexagonal   as G
import qualified Math.Geometry.Grid.HexagonalInternal   as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

toDirs :: String -> [G.HexDirection]
toDirs = \case
    [] -> []
    'w':ds -> G.West : toDirs ds
    'e':ds -> G.East : toDirs ds
    'n':'e':ds -> G.Northeast : toDirs ds
    'n':'w':ds -> G.Northwest : toDirs ds
    's':'e':ds -> G.Southeast : toDirs ds
    's':'w':ds -> G.Southwest : toDirs ds

stepDs :: [G.HexDirection] -> (Int, Int)
stepDs = go (0,0)
  where
    go :: (Int, Int) -> [G.HexDirection] -> (Int, Int)
    go p [] = p
    go p (d:ds) = case G.neighbour G.UnboundedHexGrid p d of
      Just p' -> go p' ds
      Nothing  -> error "what"

day24a :: _ :~> _
day24a = MkSol
    { sParse = Just . map toDirs . lines
    , sShow  = show
    , sSolve = Just . M.size . M.filter odd . freqs . map stepDs
    }

type P = (Int, Int)

day24b :: _ :~> _
day24b = MkSol
    { sParse = Just . map toDirs . lines
    , sShow  = show
    , sSolve = Just . S.size . (!!! 100) . iterate go . M.keysSet . M.filter odd . freqs . map stepDs
    }
  where
    go :: Set P -> Set P
    go ps = stayAlive <> comeAlive
      where
        neighborCounts = M.fromListWith (+)
          [ (neighb, 1::Int)
          | p <- S.toList ps
          , neighb <- G.neighbours G.UnboundedHexGrid p
          ]
        stayAlive = M.keysSet . M.filter (\n -> n == 1 || n == 2) $
                      neighborCounts `M.restrictKeys` ps
        comeAlive = M.keysSet . M.filter (== 2) $
                      neighborCounts `M.withoutKeys`  ps
