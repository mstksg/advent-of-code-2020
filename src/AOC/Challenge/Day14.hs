{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import           AOC.Prelude hiding (binary)

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import           Numeric.Lens
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

day14a :: _ :~> _
day14a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . sum . fst . foldl' process (IM.empty, replicate 36 Nothing)
    }
  where
    process :: (IntMap Int, [Maybe Bool]) -> String -> (IntMap Int, [Maybe Bool])
    process (mp,mks) str = case words str of
      "mask":_:bs:_ -> (mp, map (\case '0' -> Just False; '1' -> Just True; 'X' -> Nothing) bs)
      (takeWhile isDigit.drop 4->memer):_:n:_ ->
        let digitstr = zipWith go mks (printf "%036b" (read @Int n))
            val = digitstr ^?! binary
        in  (IM.insert (read memer) val mp, mks)
    go Nothing = id
    go (Just True) = const '1'
    go (Just False) = const '0'





day14b :: _ :~> _
day14b = MkSol
    { sParse = sParse day14a
    , sShow  = show
    , sSolve = Just . sum . fst . foldl' process (IM.empty, replicate 36 Nothing)
    }
  where
    process :: (IntMap Int, [Maybe Bool]) -> String -> (IntMap Int, [Maybe Bool])
    process (mp,mks) str = case words str of
      "mask":_:bs:_ -> (mp, map (\case '0' -> Just False; '1' -> Just True; 'X' -> Nothing) bs)
      (takeWhile isDigit.drop 4->memer):_:n:_ ->
        let memers :: [[Char -> Char]]
            memers = for mks \case
              Nothing -> [const '0', const '1']
              Just True -> [const '1']
              Just False -> [id]
            ixes = map (fromJust . preview binary . zipWith (&) (printf "%036b" (read @Int memer))) memers
        in  (foldl' (\mm x -> IM.insert x (read n) mm) mp ixes, mks)
