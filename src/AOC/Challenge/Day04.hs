-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Common      (countTrue)
import           AOC.Solver      ((:~>)(..))
import           Control.DeepSeq (NFData)
import           Data.Char       (isDigit, isHexDigit, toUpper)
import           Data.Foldable   (toList)
import           Data.Ix         (inRange)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import           Data.Set        (Set)
import           GHC.Generics    (Generic)
import           Text.Read       (readMaybe)
import qualified Control.Foldl   as F
import qualified Data.Map        as M
import qualified Data.Set        as S

data Field =
      BYR
    | IYR
    | EYR
    | HGT
    | HCL
    | ECL
    | PID
    | CID
  deriving (Show, Read, Eq, Ord, Generic, Enum)
instance NFData Field

parseField :: String -> Maybe Field
parseField = readMaybe . map toUpper

requiredFields :: Set Field
requiredFields = S.fromList [BYR .. PID]

-- | Check if all the items match a predicate and that the collection is of
-- the given length.  Done using 'F.Fold' to do it all in one traversal.
allWithLength :: (a -> Bool) -> Int -> F.Fold a Bool
allWithLength p expected = do
    allTrue <- F.all p
    len     <- F.length
    pure (allTrue && len == expected)

validateField :: Field -> String -> Bool
validateField = \case
    BYR -> any (inRange (1920, 2002)) . readMaybe @Int
    IYR -> any (inRange (2010, 2020)) . readMaybe @Int
    EYR -> any (inRange (2020, 2030)) . readMaybe @Int
    HGT -> \str ->
        let (x, u) = span isDigit str
            cond   = case u of
             "cm" -> inRange (150, 193)
             "in" -> inRange (59, 76)
             _    -> const False
        in any cond (readMaybe @Int x)
    HCL -> \case
        '#':ns -> F.fold (allWithLength isHexDigit 6) ns
        _      -> False
    ECL -> flip S.member $ S.fromList
        ["amb","blu","brn","gry","grn","hzl","oth"]
    PID -> F.fold (allWithLength isDigit 9)
    CID -> const True

buildPassport :: String -> Map Field String
buildPassport str = M.fromList
    [ (fld, v)
    | [k,v] <- splitOn ":" <$> words str
    , fld   <- toList $ parseField k
    ]

validate :: Map Field a -> Bool
validate = S.null . (requiredFields S.\\) . M.keysSet

day04a :: [Map Field String] :~> Int
day04a = MkSol
    { sParse = Just . map buildPassport . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . countTrue validate
    }

day04b :: [Map Field String] :~> Int
day04b = MkSol
    { sParse = Just . map buildPassport . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . countTrue (validate . M.filterWithKey validateField)
    }
