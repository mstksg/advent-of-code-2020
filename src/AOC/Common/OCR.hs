{-# LANGUAGE QuasiQuotes #-}

module AOC.Common.OCR (
    parseLetters
  , parseLettersSafe
  , parseLettersAll
  , contiguousShapes
  ) where

import           AOC.Common
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map           (Map)
import           Data.Semigroup
import           Data.Set           (Set)
import           Data.Set.NonEmpty  (NESet)
import           Linear
import           Text.Heredoc       (here)
import qualified Control.Foldl      as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import qualified Data.Map.NonEmpty  as NEM
import qualified Data.Set           as S
import qualified Data.Set.NonEmpty  as NES

-- | The set of unconnected shapes, indexed by their original center of
-- mass
contiguousShapes :: Set Point -> Map (V2 Double) (NESet Point)
contiguousShapes s0 = M.fromList
    [ (com, NES.map (subtract topCorner) s)
    | NES.IsNonEmpty s <- S.toList . S.map flood $ s0
    , let com            = F.fold ((lmap . fmap) fromIntegral F.mean) s
          V2 topCorner _ = boundingBox s
    ]
  where
    flood = floodFill (S.fromList . filter (`S.member` s0) . fullNeighbs)
          . S.singleton

-- | The set of unconnected shapes, sorted against some function on their
-- original center of masses
contiguousShapesBy
    :: Ord a
    => (V2 Double -> a)
    -> Set Point
    -> [NESet Point]
contiguousShapesBy f = toList . M.mapKeys f . contiguousShapes

parseLettersAll
    :: Set Point
    -> NonEmpty String
parseLettersAll letters = snd $ NEM.findMin attempts
  where
    NEM.IsNonEmpty attempts = M.fromListWith (<>)
        [ (score :: Int, res :| [])
        | refl <- [id, over _x negate]
        , rots <- [id, perp, negate, negate . perp]
        , let ls = S.map (rots . refl) letters
              (Sum score, res) = tryMe ls
        ]
    tryMe = traverse (\c -> maybe (Sum 1, '?') (Sum 0,) . M.lookup c $ letterMap)
          . contiguousShapesBy (view _x)


parseLetters
    :: Set Point
    -> String
parseLetters = NE.head . parseLettersAll

parseLettersSafe
    :: Set Point
    -> Maybe String
parseLettersSafe pts = x <$ guard (NES.size (NES.fromList ls) == 1)
  where
    ls@(x :| _) = parseLettersAll pts


-- | A map of a set of "on" points (for a 4x6 grid) to the letter they
-- represent
letterMap :: Map (NESet Point) Char
letterMap = M.fromList
          . uncurry (zipWith (flip (,)))
          . second ( contiguousShapesBy (view _x)
                   . M.keysSet
                   . parseAsciiMap (guard . (== '#'))
                   )
          $ rawLetterforms

rawLetterforms :: (String, String)
rawLetterforms = ("ABCEFGHJKLPRUYZ", drop 1 [here|
.##..###...##..####.####..##..#..#...##.#..#.#....###..###..#..#.#...#.####
#..#.#..#.#..#.#....#....#..#.#..#....#.#.#..#....#..#.#..#.#..#.#...#....#
#..#.###..#....###..###..#....####....#.##...#....#..#.#..#.#..#..#.#....#.
####.#..#.#....#....#....#.##.#..#....#.#.#..#....###..###..#..#...#....#..
#..#.#..#.#..#.#....#....#..#.#..#.#..#.#.#..#....#....#.#..#..#...#...#...
#..#.###...##..####.#.....###.#..#..##..#..#.####.#....#..#..##....#...####
|])
