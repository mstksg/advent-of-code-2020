{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-dodgy-exports  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC.Challenge
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
--

module AOC.Challenge (
    module AOC
  , ChallengeMap
  , ChallengeSpec(..), Part(..)
  , challengeMap
  , lookupSolution
  , dayToInt
  , solSpec
  , charPart
  ) where

import           AOC.Challenge.Day01 as AOC
import           AOC.Challenge.Day02 as AOC
import           AOC.Challenge.Day03 as AOC
import           AOC.Challenge.Day04 as AOC
import           AOC.Challenge.Day05 as AOC
import           AOC.Challenge.Day06 as AOC
import           AOC.Challenge.Day07 as AOC
import           AOC.Challenge.Day08 as AOC
import           AOC.Challenge.Day09 as AOC
import           AOC.Challenge.Day10 as AOC
import           AOC.Challenge.Day11 as AOC
import           AOC.Challenge.Day12 as AOC
import           AOC.Challenge.Day13 as AOC
import           AOC.Challenge.Day14 as AOC
import           AOC.Challenge.Day15 as AOC
import           AOC.Challenge.Day16 as AOC
import           AOC.Challenge.Day17 as AOC
import           AOC.Challenge.Day18 as AOC
import           AOC.Challenge.Day19 as AOC
import           AOC.Challenge.Day20 as AOC
import           AOC.Challenge.Day21 as AOC
import           AOC.Challenge.Day22 as AOC
import           AOC.Challenge.Day23 as AOC
import           AOC.Challenge.Day24 as AOC
import           AOC.Challenge.Day25 as AOC

import           AOC.Discover
import           AOC.Solver
import           Advent
import           Control.Monad
import           Data.Finite
import           Data.Map         (Map)
import qualified Data.Map         as M

-- | A map of all challenges.
challengeMap :: ChallengeMap
challengeMap = mkChallengeMap $$(solutionList "src/AOC/Challenge")

-- | Lookup up a solution from a 'ChallengeMap'
lookupSolution :: ChallengeSpec -> Map (Finite 25) (Map Part a) -> Maybe a
lookupSolution CS{..} = M.lookup _csPart <=< M.lookup _csDay
