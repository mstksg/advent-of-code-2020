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
import           Data.Bits          (setBit, popCount, (.&.), (.|.))
import           Data.Char          (ord)
import           Data.List          (foldl')
import           Data.List.NonEmpty (NonEmpty)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (mapMaybe)
import qualified Data.List.NonEmpty as NE

type CharSet = Word

toCharSet :: [Char] -> CharSet
toCharSet = foldl' insertCharSet 0
  where
    insertCharSet cs c = cs `setBit` i
      where
        i = ord c - ord 'a'

answers :: [String] -> Maybe (NonEmpty CharSet)
answers = (fmap . fmap) toCharSet . NE.nonEmpty

day06With
    :: (CharSet -> CharSet -> CharSet)
    -> [[String]] :~> Int
day06With f = MkSol
    { sParse = Just . map lines . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . sum . mapMaybe (fmap (popCount . foldr1 f) . answers)
    }

day06a :: [[String]] :~> Int
day06a = day06With (.|.)

day06b :: [[String]] :~> Int
day06b = day06With (.&.)
