{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Prelude

import qualified Data.Graph                     as G
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Data.Set as S
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

type Validator = Map String (String -> Bool)

validator1 :: Validator
validator1 = const True <$ validator2

validator2 :: Validator
validator2 = M.fromList
    [ ("byr", \str -> isJust $ mfilter (\n -> n >= 1920 && n <= 2002) (readMaybe str))
    , ("iyr", \str -> isJust $ mfilter (\n -> n >= 2010 && n <= 2020) (readMaybe str))
    , ("eyr", \str -> isJust $ mfilter (\n -> n >= 2020 && n <= 2030) (readMaybe str))
    , ("hgt", \str ->
        let (x,u) = span isDigit str
            cond = case u of
              "cm" -> \n -> n >= 150 && n <= 193
              "in" -> \n -> n >= 59 && n <= 76
              _    -> const False
        in  isJust $ mfilter cond (readMaybe x)
      )
    , ("hcl", \str -> case str of
                '#':ns -> all isHexDigit ns && length ns == 6
                _ -> False
      )
    , ("ecl", \str -> str `S.member` S.fromList
            ["amb","blu","brn","gry","grn","hzl","oth"]
      )
    , ("pid", \str -> all isDigit str && length str == 9
      )
    ]

validate :: Validator -> Map String String -> Bool
validate vld = (== 7) . M.size . M.filter id . M.intersectionWith ($) vld

tokenMap :: String -> Map String String
tokenMap = M.fromList . mapMaybe (go . splitOn ":") . words
  where
    go [x,y] = Just (x, y)
    go _     = Nothing

day04a :: _ :~> _
day04a = MkSol
    { sParse = Just . map tokenMap . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . countTrue (validate validator1)
    }

day04b :: _ :~> _
day04b = MkSol
    { sParse = Just . map tokenMap . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . countTrue (validate validator2)
    }
