{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day10 (
    day10a
  , day10b
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

toChain :: [Int] -> IntSet
toChain xs = xsset `IS.union` IS.fromList [0, top + 3]
  where
    xsset = IS.fromList xs
    top   = IS.findMax xsset

day10a :: _ :~> _
day10a = MkSol
    { sParse = fmap toChain . traverse (readMaybe @Int) . lines
    , sShow  = show
    , sSolve = \(IS.toList->xs) ->
        let fs = freqs $ zipWith (-) (drop 1 xs) xs
        in  Just $ lookupFreq 1 fs * lookupFreq 3 fs
    }

toGraph2 :: IntSet -> IntMap IntSet
toGraph2 ss = IM.fromListWith (<>)
    [ (x, IS.singleton opt)
    | x <- IS.toList ss
    , opt <- [x+1,x+2,x+3]
    , opt `IS.member` ss
    ]

pathsToGoal :: IntMap IntSet -> IntMap Int
pathsToGoal is = res
  where
    res = flip IM.mapWithKey is $ \i ks ->
      if i == goal
        then 1
        else sum . mapMaybe (flip IM.lookup res) $ IS.toList ks
    (goal,_) = IM.findMax is
      

day10b :: _ :~> _
day10b = MkSol
    { sParse = fmap toChain . traverse (readMaybe @Int) . lines
    , sShow  = show
    , sSolve = Just . (IM.! 0) . pathsToGoal . toGraph2
    }
