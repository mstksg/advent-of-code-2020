{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day23 (
    -- day23a
  -- , day23b
  ) where

import           AOC.Prelude

day23a :: _ :~> _
day23a = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = Just
    }

day23b :: _ :~> _
day23b = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = Just
    }
