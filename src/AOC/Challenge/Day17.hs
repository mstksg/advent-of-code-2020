{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day17 (
    day17a
  , day17b
  ) where

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

type P3 = V3 Int

stepper :: Set P3 -> Set P3
stepper cs = S.fromList
    [ i
    | i <- range (mn - 1, mx + 1)
    , let ni = S.fromList (neighbs3 i)
    , let nn = S.size (ni `S.intersection` cs)
    , case i `S.member` cs of
        False -> nn == 3
        True  -> nn == 2 || nn == 3
    ]
  where
    Just bb@(V2 mn mx) = boundingBox' cs

neighbs3 :: P3 -> [P3]
neighbs3 p = [ p + V3 dx dy dz
             | dx <- [-1 .. 1]
             , dy <- [-1 .. 1]
             , dz <- [-1 .. 1]
             , V3 dx dy dz /= 0
             ]


inBoundingBox
    :: (Applicative g, Foldable g, Ord a)
    => V2 (g a)
    -> g a
    -> Bool
inBoundingBox (V2 mn mx) x = all id $ go <$> x <*> mn <*> mx
  where
    go x' mn' mx' = x' >= mn' && x' <= mx'

day17a :: _ :~> _
day17a = MkSol
    { sParse = Just . S.map (\(V2 x y) -> V3 x y 0) . M.keysSet . parseAsciiMap (\case '#' -> Just (); _ -> Nothing)
    , sShow  = show . S.size
    , sSolve = Just . (!! 6) . iterate stepper
    }

type P4 = V4 Int

stepper4 :: Set P4 -> Set P4
stepper4 cs = S.fromList
    [ i
    | i <- range (mn - 1, mx + 1)
    , let ni = S.fromList (neighbs4 i)
    , let nn = S.size (ni `S.intersection` cs)
    , case i `S.member` cs of
        False -> nn == 3
        True  -> nn == 2 || nn == 3
    ]
  where
    Just bb@(V2 mn mx) = boundingBox' cs

-- neighbs4 :: P3 -> [P3]
neighbs4 p = [ p + V4 dx dy dz dw
             | dx <- [-1 .. 1]
             , dy <- [-1 .. 1]
             , dz <- [-1 .. 1]
             , dw <- [-1 .. 1]
             , V4 dx dy dz dw /= 0
             ]

day17b :: _ :~> _
day17b = MkSol
    { sParse = fmap (S.map (\(V3 x y z) -> V4 x y z 0)) . sParse day17a
    , sShow  = show . S.size
    , sSolve = Just . (!! 6) . iterate stepper4
    }
