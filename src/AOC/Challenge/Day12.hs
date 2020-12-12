-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day12 (
    day12a
  , day12b
  ) where

import           AOC.Common      (Point, Dir(..), dirPoint, rotPoint, mannDist)
import           AOC.Solver      ((:~>)(..))
import           Control.DeepSeq (NFData)
import           Data.Group      (pow)
import           Data.List       (foldl')
import           Data.Map        (Map)
import           GHC.Generics    (Generic)
import           Linear          (V2(..), (*^))
import           Text.Read       (readMaybe)
import qualified Data.Map        as M

data Instr = Forward Int
           | Turn Dir
           | Move Point
  deriving (Show, Eq, Generic)
instance NFData Instr

mkInstr :: Map Char (Int -> Instr)
mkInstr = M.fromList
    [ ('F', Forward)
    , ('L', Turn . pow West . (`div` 90))
    , ('R', Turn . pow East . (`div` 90))
    , ('N', Move . (*^ dirPoint North))
    , ('S', Move . (*^ dirPoint South))
    , ('E', Move . (*^ dirPoint East ))
    , ('W', Move . (*^ dirPoint West ))
    ]

parseInstr :: String -> Maybe Instr
parseInstr []    = Nothing
parseInstr (c:n) = M.lookup c mkInstr <*> readMaybe n

day12a :: [Instr] :~> Point
day12a = MkSol
    { sParse = traverse parseInstr . lines
    , sShow  = show . mannDist 0
    , sSolve = Just . snd . foldl' go (East, 0)
    }
  where
    go :: (Dir, Point) -> Instr -> (Dir, Point)
    go (!dir, !p) = \case
      Forward n -> (dir     , p + n *^ dirPoint dir)
      Turn d    -> (dir <> d, p                    )
      Move r    -> (dir     , p + r                )

day12b :: [Instr] :~> Point
day12b = MkSol
    { sParse = sParse day12a
    , sShow  = show . mannDist 0
    , sSolve = Just . fst . foldl' go (0, V2 10 1)
    }
  where
    go :: (Point, Point) -> Instr -> (Point, Point)
    go (!shp, !wp) = \case
      Forward n -> (shp + n *^ wp, wp           )
      Turn d    -> (shp          , rotPoint d wp)
      Move r    -> (shp          , wp + r       )
