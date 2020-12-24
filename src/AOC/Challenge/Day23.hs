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
import           Control.Monad             ((<=<), unless)
import           Control.Monad.Primitive   (PrimMonad, PrimState)
import           Control.Monad.ST          (runST)
import           Control.Monad.Trans.Class (lift)
import           Data.Bifunctor            (first)
import           Data.Char                 (digitToInt, intToDigit)
import           Data.Finite               (Finite, packFinite, getFinite, finites)
import           Data.Foldable             (for_)
import           Data.Vector.Mutable.Sized (MVector)
import           Data.Vector.Sized         (Vector)
import           GHC.TypeNats              (KnownNat)
import qualified Data.Conduino             as C
import qualified Data.Conduino.Combinators as C
import qualified Data.Vector.Mutable.Sized as MV
import qualified Data.Vector.Sized         as V

newtype CrabState n s = CrabState { csRight  :: MVector n s (Finite n) }

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
    -> Finite n
    -> m (Finite n)
step CrabState{..} lab = do
    (grabbed, lab') <- pullN 3 lab
    MV.write csRight lab lab'
    let target = until (`notElem` grabbed) (subtract 1) (lab - 1)
    aftertarg <- MV.read csRight target
    MV.write csRight target (head grabbed)
    MV.write csRight (last grabbed) aftertarg
    pure lab'
  where
    pullN :: Int -> Finite n -> m ([Finite n], Finite n)
    pullN n i = do
      j <- MV.read csRight i
      if n == 0
        then pure ([], j)
        else first (j:) <$> pullN (n - 1) j
{-# INLINE step #-}

initialize
    :: (KnownNat n, PrimMonad m, PrimState m ~ s)
    => Vector n (Finite n)
    -> m (Finite n, CrabState n s)      -- ^ initial pointer
initialize v0 = do
    csRight <- MV.new
    for_ finites $ \i ->
      MV.write csRight (v0 `V.index` (i - 1)) (v0 `V.index` i)
    let i0 = v0 `V.index` 0
    pure (i0, CrabState{..})
{-# INLINE initialize #-}

run :: (KnownNat n, PrimMonad m, PrimState m ~ s)
    => Int
    -> Finite n
    -> CrabState n s
    -> m ()
run n i0 cs = go 0 i0
  where
    go !m !i
      | m == n    = pure ()
      | otherwise = go (m + 1) =<< step cs i
    {-# INLINE go #-}
{-# INLINE run #-}


day23a :: Vector 9 (Finite 9) :~> [Int]
day23a = MkSol
    { sParse = V.fromList @9 <=< traverse toFin
    , sShow  = fmap intToDigit
    , sSolve = \v0 -> Just $ runST $ do
        (i0, cs) <- initialize v0
        run 100 i0 cs
        C.runPipe $ sourceCrabState cs 0
               C..| C.map fromFin
               C..| C.sinkList
    }

day23b :: Vector 1000000 (Finite 1000000) :~> [Int]
day23b = MkSol
    { sParse = V.fromList . (++ [9 .. ]) <=< traverse toFin
    , sShow  = show . product
    , sSolve = \v0 -> Just $ runST $ do
        (i0, cs) <- initialize v0
        run 10000000 i0 cs
        C.runPipe $ sourceCrabState cs 0
               C..| C.map fromFin
               C..| C.take 2
               C..| C.sinkList
    }

toFin :: KnownNat n => Char -> Maybe (Finite n)
toFin = packFinite . subtract 1 . fromIntegral . digitToInt

fromFin :: Finite n -> Int
fromFin = fromIntegral . (+ 1) . getFinite
