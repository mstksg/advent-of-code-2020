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

import           AOC.Common                        (Point, boundingBox', inBoundingBox, fullNeighbs, fullNeighbsSet, parseAsciiMap, countTrue)
import           AOC.Solver                        ((:~>)(..))
import           Control.Monad.Loops               (whileM_)
import           Control.Monad.Primitive           (PrimMonad, PrimState)
import           Control.Monad.ST                  (runST)
import           Control.Monad.State               (execStateT, modify, StateT)
import           Data.Bit                          (Bit(..))
import           Data.Bits                         (popCount)
import           Data.Finite                       (Finite, finites)
import           Data.Foldable                     (find, toList, traverse_)
import           Data.Map                          (Map)
import           Data.Maybe                        (mapMaybe)
import           Data.Set                          (Set)
import           GHC.TypeNats                      (KnownNat)
import           Linear                            (V2(..))
import qualified Data.Map.Strict                   as M
import qualified Data.Set                          as S
import qualified Data.Vector.Generic.Sized         as VG
import qualified Data.Vector.Sized                 as V
import qualified Data.Vector.Unboxed.Mutable.Sized as MVU
import qualified Data.Vector.Unboxed.Sized         as VU

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
    :: forall n m s. (KnownNat n, PrimMonad m, s ~ PrimState m)
    => Int                          -- ^ exit seat threshold
    -> V.Vector n [Finite n]        -- ^ neighbors
    -> MVU.MVector n s Bit          -- ^ source
    -> MVU.MVector n s Bit          -- ^ target
    -> StateT Bool m ()             -- ^ changed
seatRule thr ns src targ = traverse_ go finites
  where
    go :: Finite n -> StateT Bool m ()
    go i = do
      Bit x <- MVU.read src i
      n     <- countTrue unBit <$> traverse (MVU.read src) (ns `V.index` i)
      let x' = case x of
            False -> not (n > 0)
            True  -> not (n >= thr)
      modify (|| (x /= x'))
      MVU.write targ i (Bit x')
    {-# INLINE go #-}
{-# INLINE seatRule #-}

solveWith
    :: KnownNat n
    => Int                      -- ^ exit seat threshold
    -> V.Vector n [Finite n]    -- ^ neighbors
    -> VU.Vector n Bit
    -> Int                      -- ^ equilibrium size
solveWith thr ns xs = runST $ do
    xs0 <- VU.thaw xs
    xs1 <- MVU.unsafeNew
    (`whileM_` pure ()) . flip execStateT False $ do
      seatRule thr ns xs0 xs1
      seatRule thr ns xs1 xs0
    popCount <$> VU.unsafeFreeze xs0


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
