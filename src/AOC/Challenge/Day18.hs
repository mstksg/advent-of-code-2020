{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day18 (
    -- day18a
  -- , day18b
  ) where

import           AOC.Prelude

day18a :: _ :~> _
day18a = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = Just
    }

day18b :: _ :~> _
day18b = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = Just
    }
