{-# LANGUAGE InstanceSigs #-}
-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day11  where

-- module AOC.Challenge.Day11 (
--     day11a
--   , day11b
--   ) where

import           AOC.Common            (Point, boundingBox', inBoundingBox, fullNeighbs, fullNeighbsSet, parseAsciiMap, fixedPoint, countTrue, dup)
import           AOC.Solver            ((:~>)(..))
import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Data.Coerce
import           Data.Foldable
import           Data.Functor
import           Data.List
import           Data.List             (find)
import           Data.Map              (Map)
import           Data.Maybe            (mapMaybe)
import           Data.Set              (Set)
import           GHC.Generics
import           Linear                (V2(..))
import qualified Data.List.PointedList as PL
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

newtype Pointed2D a = Pointed2D { runPointed2D :: PL.PointedList (PL.PointedList a) }
  deriving (Functor, Show, Eq, Foldable, Generic)

instance Comonad Pointed2D where
    extract = PL._focus . PL._focus . runPointed2D

    duplicate :: forall a. Pointed2D a -> Pointed2D (Pointed2D a)
    -- duplicate (Pointed2D xs) = Pointed2D $ coerce . PL.positions . fmap PL.positions $ xs
    duplicate = coerce (positions2D @a)

prevRows
    :: PL.PointedList (PL.PointedList a)
    -> Maybe (PL.PointedList (PL.PointedList a))
prevRows = traverse PL.previous

nextRows
    :: PL.PointedList (PL.PointedList a)
    -> Maybe (PL.PointedList (PL.PointedList a))
nextRows = traverse PL.next

shiftRows
    :: PL.PointedList (PL.PointedList a)
    -> PL.PointedList (PL.PointedList (PL.PointedList a))
shiftRows p = PL.PointedList ls' p rs'
  where
    ls' = unfoldr (fmap dup . prevRows) p
    rs' = unfoldr (fmap dup . nextRows) p

positions2D
    :: PL.PointedList (PL.PointedList a)
    -> PL.PointedList (PL.PointedList (PL.PointedList (PL.PointedList a)))
positions2D = fmap shiftRows . PL.positions

peekP2D
    :: V2 Int
    -> Pointed2D a
    -> Maybe a
peekP2D (V2 x y) (Pointed2D pss) = do
    ps <- PL._focus <$> PL.moveN x pss
    PL._focus <$> PL.moveN y ps

instance Wrapped (Pointed2D a)
instance Rewrapped (Pointed2D a) (Pointed2D a)

focusP2D :: Lens' (Pointed2D a) a
focusP2D = _Wrapped . PL.focus . PL.focus


seekP2D
    :: V2 Int
    -> Pointed2D a
    -> Maybe (Pointed2D a)
seekP2D (V2 x y) (Pointed2D pss) = do
    qss <- PL.moveN x pss
    Pointed2D <$> traverse (PL.moveN y) qss

-- | the kleisli arrow for part 1
rule1 :: Pointed2D (Maybe Bool) -> Maybe Bool
rule1 xs = extract xs <&> \case
    False -> not $ any ((== Just (Just True)) . flip peekP2D xs) (fullNeighbs 0)
    True  ->
      let onNeighbs = countTrue ((== Just (Just True)) . flip peekP2D xs) (fullNeighbs 0)
      in  not (onNeighbs >= 4)

parseMap :: String -> Maybe (Pointed2D (Maybe Bool))
parseMap xs = Pointed2D <$> do
    ls <- PL.fromList (lines xs)
    traverse (PL.fromList . map interp) ls
  where
    interp 'L' = Just False
    interp '#' = Just True
    interp _   = Nothing

dispMap :: Pointed2D (Maybe Bool) -> String
dispMap p = unlines (toList <$> toList (runPointed2D p'))
  where
    p' = p & mapped %~ reChar
           & focusP2D %~ focusup
  -- where
  --   reChar = id
        -- unlines $ map (map go . toList) (toList xss)
  -- where
    reChar Nothing      = '.'
    reChar (Just False) = 'L'
    reChar (Just True)  = '#'
    focusup 'L' = 'H'
    focusup '#' = '&'
    focusup _   = ':'

  -- where
  --   go xs = []

seatRule
    :: Int                       -- ^ exit seat threshold
    -> Map Point (Set Point)     -- ^ neighbors for each point
    -> Map Point Bool
    -> Map Point Bool
seatRule thr nmp mp = M.intersectionWith go nmp mp
  where
    go neighbs = \case
      False -> not (any (mp M.!) neighbs)
      True  ->
        let onNeighbs = countTrue (mp M.!) neighbs
        in  not (onNeighbs >= thr)

solveWith
    :: Int                      -- ^ exit seat threshold
    -> Map Point (Set Point)    -- ^ neighbors for each point
    -> Map Point Bool           -- ^ initial state
    -> Int                      -- ^ equilibrium size
solveWith thr neighbs = countTrue id . fixedPoint (seatRule thr neighbs)

parseSeatMap :: String -> Map Point Bool
parseSeatMap = parseAsciiMap $ \case
    'L' -> Just False
    '#' -> Just True    -- not in the input, but just for completion's sake
    _   -> Nothing

-- | Get a map of points to all of those points' neighbors where there is
-- a seat. Should only need to be computed once.
lineOfSights1
    :: Set Point
    -> Map Point (Set Point)
lineOfSights1 pts = M.fromSet go pts
  where
    go p = fullNeighbsSet p `S.intersection` pts


day11a :: _ :~> _
day11a = MkSol
    { sParse = parseMap
    , sShow  = show
    , sSolve = Just . countTrue (== Just True) . fixedPoint (extend rule1)
    -- , sSolve = Just . take 3 . drop 100 . iterate (extend rule1)
        -- let los = lineOfSights1 (M.keysSet mp)
        -- in  solveWith 4 los mp
    }

-- day11a :: Map Point Bool :~> Int
-- day11a = MkSol
--     { sParse = Just . parseSeatMap
--     , sShow  = show
--     , sSolve = \mp -> Just $
--         let los = lineOfSights1 (M.keysSet mp)
--         in  solveWith 4 los mp
--     }

-- | Get a map of points to all of those points' visible neighbors. Should
-- only need to be computed once.
lineOfSights2
    :: V2 Point
    -> Set Point
    -> Map Point (Set Point)
lineOfSights2 bb pts = M.fromSet go pts
  where
    go p = S.fromList
         . mapMaybe (los p)
         $ fullNeighbs 0
    los p d = find (`S.member` pts)
            . takeWhile (inBoundingBox bb)
            . tail
            $ iterate (+ d) p

day11b :: Map Point Bool :~> Int
day11b = MkSol
    { sParse = Just . parseSeatMap
    , sShow  = show
    , sSolve = \mp -> do
        bb <- boundingBox' (M.keys mp)
        let los = lineOfSights2 bb (M.keysSet mp)
        pure $ solveWith 5 los mp
    }
