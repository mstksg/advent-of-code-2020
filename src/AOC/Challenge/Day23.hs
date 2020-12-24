-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day23 (
    day23a
  , day23b
  ) where

import           AOC.Solver                ((:~>)(..))
import           Control.Monad             ((<=<), replicateM_, unless)
import           Control.Monad.Primitive   (PrimMonad, PrimState)
import           Control.Monad.ST          (runST)
import           Control.Monad.Trans.Class (lift)
import           Data.Bifunctor            (first)
import           Data.Char                 (digitToInt, intToDigit)
import           Data.Finite               (Finite, packFinite, getFinite, finites)
import           Data.Primitive.MutVar     (MutVar, newMutVar, writeMutVar, readMutVar)
import           Data.Traversable          (for)
import           Data.Vector.Mutable.Sized (MVector)
import           Data.Vector.Sized         (Vector)
import           GHC.TypeNats              (KnownNat)
import qualified Data.Conduino             as C
import qualified Data.Conduino.Combinators as C
import qualified Data.Vector.Mutable.Sized as MV
import qualified Data.Vector.Sized         as V

data CrabState n s = CrabState
    { csRight  :: MVector n s (Finite n)
    , csActive :: MutVar s (Finite n)
    }

sourceCrabState
    :: (PrimMonad m, PrimState m ~ s)
    => CrabState n s
    -> Finite n         -- ^ item to start from
    -> C.Pipe i (Finite n) u m ()
sourceCrabState CrabState{..} i0 = go i0
  where
    go i = do
      j <- lift $ MV.read csRight i
      unless (j == i0) $ do
        C.yield j
        go j

step
    :: forall n m s. (KnownNat n, PrimMonad m, PrimState m ~ s)
    => CrabState n s
    -> m ()
step CrabState{..} = do
    lab <- readMutVar csActive
    (grabbed, lab') <- pullN 3 lab
    MV.write csRight lab lab'
    let target = until (`notElem` grabbed) (subtract 1) (lab - 1)
    aftertarg <- MV.read csRight target
    MV.write csRight target (head grabbed)
    MV.write csRight (last grabbed) aftertarg
    writeMutVar csActive lab'
  where
    pullN :: Int -> Finite n -> m ([Finite n], Finite n)
    pullN n i
      | n == 0    = do
          j <- MV.read csRight i
          pure ([], j)
      | otherwise = do
          j <- MV.read csRight i
          first (j:) <$> pullN (n - 1) j

initialize
    :: (KnownNat n, PrimMonad m, PrimState m ~ s)
    => Vector n (Finite n)
    -> m (CrabState n s)
initialize v0 = do
    csRight <- MV.new
    for finites $ \i -> MV.write csRight (v0 `V.index` (i - 1)) (v0 `V.index` i)
    csActive <- newMutVar $ v0 `V.index` 0
    pure CrabState{..}

day23a :: _ :~> _
day23a = MkSol
    { sParse = V.fromList @9 <=< traverse toFin
    , sShow  = fmap intToDigit
    , sSolve = \v0 -> Just $ runST $ do
        cs0 <- initialize v0
        replicateM_ 100 (step cs0)
        C.runPipe $ sourceCrabState cs0 0
               C..| C.map fromFin
               C..| C.sinkList
    }

day23b :: Vector 1000000 (Finite 1000000) :~> [Int]
day23b = MkSol
    { sParse = V.fromList . (++ [9 .. ]) <=< traverse toFin
    , sShow  = show . product
    , sSolve = \v0 -> Just $ runST $ do
        cs0 <- initialize v0
        replicateM_ 10000000 (step cs0)
        C.runPipe $ sourceCrabState cs0 0
               C..| C.map fromFin
               C..| C.take 2
               C..| C.sinkList
    }

toFin :: KnownNat n => Char -> Maybe (Finite n)
toFin = packFinite . subtract 1 . fromIntegral . digitToInt

fromFin :: Finite n -> Int
fromFin = fromIntegral . (+ 1) . getFinite
