-- |
-- Module      : AOC
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Single-stop entry point for the library's functionality and all
-- challenge solutions.
--

module AOC (
    module AOC
  ) where

import           AOC.Challenge       as AOC
import           AOC.Run             as AOC
import           AOC.Run.Config      as AOC
import           AOC.Run.Interactive as AOC
import           AOC.Run.Load        as AOC
import           AOC.Solver          as AOC
import           AOC.Util            as AOC

