-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC.Solver         ((:~>)(..))
import           Data.List.NonEmpty (NonEmpty)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (mapMaybe)
import           Data.Set           (Set)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as S

type Answers = [NonEmpty (Set Char)]

answers :: String -> [NonEmpty (Set Char)]
answers = mapMaybe ((fmap . fmap) S.fromList . NE.nonEmpty . lines)
        . splitOn "\n\n"

day06With 
    :: (Set Char -> Set Char -> Set Char)
    -> Answers :~> Int
day06With f = MkSol
    { sParse = Just . answers
    , sShow  = show
    , sSolve = Just . sum . map (S.size . foldr1 f)
    }

day06a :: Answers :~> Int
day06a = day06With S.union

day06b :: Answers :~> Int
day06b = day06With S.intersection
