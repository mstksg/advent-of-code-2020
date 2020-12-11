{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
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

-- module AOC.Challenge.Day11 (
--     day11a
--   , day11b
--   ) where

module AOC.Challenge.Day11 where
    -- day11a
  -- , day11b
  -- ) where


import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
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

seatRule
    :: Map Point Bool
    -> Map Point Bool
seatRule mp = M.mapWithKey go mp
  where
    go p False = if any (\d -> M.findWithDefault False d mp) (fullNeighbs p)
                then False
                else True
    go p True = not (n >= 4)
      where
        n = countTrue (\d -> M.findWithDefault False d mp) (fullNeighbs p)

day11a :: _ :~> _
day11a = MkSol
    { sParse = Just . parseAsciiMap (\case 'L' -> Just False; _ -> Nothing)
    , sShow  = show
    , sSolve = Just . M.size . M.filter id . fixedPoint seatRule
    }

day11b :: _ :~> _
day11b = MkSol
    { sParse = sParse day11a
    , sShow  = show
    , sSolve = \mp ->
        let Just bb = boundingBox' (M.keys mp)
        in  Just . M.size . M.filter id . fixedPoint (seatRule2 bb) $ mp
    }

seatRule2
    :: V2 Point
    -> Map Point Bool
    -> Map Point Bool
seatRule2 bb mp = res
  where
    res = M.mapWithKey go mp
    go p False = if any (\d -> fromMaybe False $ scanDir p d) (fullNeighbs 0)
                then False
                else True
    go p True = not (n >= 5)
      where
        n = countTrue (\d -> fromMaybe False $ scanDir p d) (fullNeighbs 0)
    scanDir p d = firstJust (`M.lookup` mp)
                . takeWhile (inBoundingBox bb)
                . tail
                $ iterate (+ d) p
