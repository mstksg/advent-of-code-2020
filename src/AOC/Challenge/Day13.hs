-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day13 (
    day13a
  , day13b
  ) where

import           AOC.Common                       (CharParser)
import           AOC.Solver                       ((:~>)(..))
import           Control.Applicative              ((<|>))
import           Data.Foldable                    (minimumBy)
import           Data.List.Split                  (splitOn)
import           Data.Maybe                       (mapMaybe, catMaybes)
import           Data.Ord                         (comparing)
import           Math.NumberTheory.Moduli.Chinese (chineseRemainder)
import           Text.Read                        (readMaybe)
import qualified Text.Megaparsec                  as P
import qualified Text.Megaparsec.Char             as P
import qualified Text.Megaparsec.Char.Lexer       as PL

parseTrains :: Num a => CharParser [Maybe a]
parseTrains = (Nothing <$ P.char 'x' <|> Just <$> PL.decimal)
    `P.sepBy` P.char ','

day13a :: (Int, [Int]) :~> (Int, Int)
day13a = MkSol
    { sParse = P.parseMaybe $
        (,) <$> (PL.decimal <* P.newline)
            <*> (catMaybes <$> parseTrains)
    , sShow  = \(x,y) -> show $ x * y
    , sSolve = \(t0, xs) -> Just $ minimumBy (comparing snd)
            [ (x, waitTime)
            | x <- xs
            , let waitTime = (x - t0) `mod` x
            ]
    }

day13b :: [(Integer, Integer)] :~> Integer
day13b = MkSol
    { sParse = P.parseMaybe $ do
        _ <- P.manyTill P.anySingle P.newline
        mapMaybe sequenceA . zip [0,-1..] <$> parseTrains
    , sShow  = show
    , sSolve = chineseRemainder
    }
