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
    -- day17a
  -- , day17b
  ) where

import           AOC.Prelude

day17a :: _ :~> _
day17a = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = Just
    }

day17b :: _ :~> _
day17b = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = Just
    }
