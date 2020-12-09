-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC.Common            (slidingWindows)
import           Debug.Trace
import           AOC.Solver            ((:~>)(..))
import           Control.Monad         (guard, join)
import           Data.Foldable         (toList)
import           Data.Functor.Compose
import           Data.Functor.Foldable
import           Data.List             (scanl', tails, find)
import           Data.Semigroup
import           Data.Sequence         (Seq(..))
import           Numeric.Natural
import           Text.Read             (readMaybe)
import qualified Data.Sequence as      Seq
import qualified Data.Vector           as V

isBad :: Seq Int -> Bool
isBad (xs :|> x) = null $ do
    y:ys <- tails (toList xs)
    z    <- ys
    guard $ (y + z) == x
isBad _          = True

oddOneOut :: [Int] -> Maybe Int
oddOneOut xs = do
    _ :|> x <- find isBad (slidingWindows 26 xs)
    pure x

day09a :: [Int] :~> Int
day09a = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = oddOneOut
    }

findBounds :: V.Vector Int -> Int -> Maybe (Int, Int)
findBounds ns goal = go 0 1
  where
    go !i !j = do
      x <- ns V.!? i
      y <- ns V.!? j
      case compare (y - x) goal of
        LT -> go i (j + 1)
        EQ -> pure (i, j)
        GT -> go (i + 1) j

findBounds_ :: Seq Int -> Int -> Maybe (Int, Int)
findBounds_ ns goal = case anaM @_ @Natural go (1, ns) of
    (Nothing, _       ) -> Nothing
    (Just (First d), i) -> Just (fromIntegral i, fromIntegral i+d)
  where
    go :: (Int, Seq Int) -> (Maybe (First Int), Maybe (Int, Seq Int))
    go (!d, xs@(x :<| ys)) = case compare (y - x) goal of
        LT -> go (d+1, xs)
        EQ -> (Just (First d), Nothing)
        GT -> (Nothing, Just (d-1, ys))
      where
        Just y = xs Seq.!? d
    go _ = (Nothing, Nothing)

anaM :: (Monad m, Corecursive t, Traversable (Base t)) => (a -> m (Base t a)) -> a -> m t
anaM f = hylo (fmap embed . join . fmap sequenceA . getCompose) (Compose . f)

day09b :: [Int] :~> (Int, Int)
day09b = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = \(x,y) -> show (x + y)
    , sSolve = \ns -> do
        goal   <- oddOneOut ns
        traceM "hi"
        traceM $ show goal
        let cumsum = V.fromList (scanl' (+) 0 ns)
        (i, j) <- findBounds cumsum goal
        traceM $ show (V.head cumsum, V.last cumsum)
        traceM $ show (i, j, findBounds_ (Seq.fromList (V.toList cumsum)) goal)
        let xs = take (j - i) . drop i $ ns
        pure (minimum xs, maximum xs)
    }
