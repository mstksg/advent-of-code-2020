-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import           AOC.Common                (Point, boundingBox', inBoundingBox, fullNeighbs, fullNeighbsSet, parseAsciiMap, countTrue, loopMaybe)
import           AOC.Solver                ((:~>)(..))
import           Control.Monad             (guard)
import           Control.Monad.State       (State, modify, runState)
import           Data.Bit                  (Bit(..))
import           Data.Bits                 (popCount)
import           Data.Finite               (Finite)
import           Data.Foldable             (find, toList)
import           Data.Map                  (Map)
import           Data.Maybe                (mapMaybe)
import           Data.Set                  (Set)
import           GHC.TypeNats              (KnownNat)
import           Linear                    (V2(..))
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import qualified Data.Vector.Generic.Sized as VG
import qualified Data.Vector.Sized         as V
import qualified Data.Vector.Unboxed.Sized as VU

parseSeatMap :: String -> Map Point Bool
parseSeatMap = parseAsciiMap $ \case
    'L' -> Just False
    '#' -> Just True    -- not in the input, but just for completion's sake
    _   -> Nothing

compile
    :: Map Point (Set Point, Bool)
    -> (forall n. KnownNat n => V.Vector n [Finite n] -> VU.Vector n Bit -> r)
    -> r
compile mp f = V.withSizedList (toList mp) $ \xs ->
            f (map go . S.toList . fst <$> xs)
              (VG.convert (Bit . snd <$> xs))
  where
    go :: KnownNat n => Point -> Finite n
    go = fromIntegral . (`M.findIndex` mp)

seatRule
    :: forall n. ()
    => Int                       -- ^ exit seat threshold
    -> V.Vector n [Finite n]
    -> VU.Vector n Bit
    -> Maybe (VU.Vector n Bit)
seatRule thr ns xs = res <$ guard changed
  where
    (res, changed) = runState (VU.imapM go xs) False
    go :: Finite n -> Bit -> State Bool Bit
    go i (Bit x)
        | x == x'   = pure $ Bit x
        | otherwise = Bit x' <$ modify (const True)
      where
        n = countTrue (unBit . (xs `VU.index`)) $ ns `V.index` i
        x' = case x of
          False -> not (n > 0)
          True  -> not (n >= thr)

solveWith
    :: KnownNat n
    => Int                      -- ^ exit seat threshold
    -> V.Vector n [Finite n]
    -> VU.Vector n Bit
    -> Int                      -- ^ equilibrium size
solveWith thr ns = popCount . loopMaybe (seatRule thr ns)

-- | Get a map of points to all of those points' neighbors where there is
-- a seat. Should only need to be computed once.
lineOfSights1
    :: Set Point
    -> Map Point (Set Point)
lineOfSights1 pts = M.fromSet go pts
  where
    go p = fullNeighbsSet p `S.intersection` pts


day11a :: Map Point Bool :~> Int
day11a = MkSol
    { sParse = Just . parseSeatMap
    , sShow  = show
    , sSolve = \mp -> Just $
        let los = lineOfSights1 (M.keysSet mp)
        in  compile (M.intersectionWith (,) los mp) (solveWith 4)

    }

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
        pure $ compile (M.intersectionWith (,) los mp) (solveWith 5)
    }
