-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day15 (
    day15a
  , day15b
  ) where

import           AOC.Solver                  ((:~>)(..))
import           Control.Monad.Loops         (whileM_)
import           Control.Monad.ST            (ST, runST)
import           Control.Monad.State.Strict  (StateT(..), evalStateT, gets)
import           Data.Foldable               (traverse_)
import           Data.List.NonEmpty          (NonEmpty(..))
import           Data.List.Split             (splitOn)
import           Text.Read                   (readMaybe)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Vector.Unboxed.Mutable as MV

day15a :: NonEmpty Int :~> Int
day15a = MkSol
    { sParse = (NE.nonEmpty =<<) . traverse readMaybe . splitOn ","
    , sShow  = show
    , sSolve = Just . looper 2020
    }

day15b :: NonEmpty Int :~> Int
day15b = MkSol
    { sParse = sParse day15a
    , sShow  = show
    , sSolve = Just . looper 30000000
    }

looper :: Int -> NonEmpty Int -> Int
looper n xs0 = runST $ flip evalStateT (0,NE.head xs0) $ do
    v <- MV.replicate n 0
    traverse_ (initter v) xs0
    whileM_ (gets ((< n) . fst)) $ StateT $ \(!i, !x) -> do
      lst <- MV.read v x
      MV.write v x i
      let j | lst ==  0 = 0
            | otherwise = i - lst
      pure ((),(i+1, j))
    gets snd
  where
    initter :: MV.MVector s Int -> Int -> StateT (Int, Int) (ST s) ()
    initter v y = StateT $ \(!i, !x) ->
      (,(i+1, y)) <$> MV.write v x i
    {-# INLINE initter #-}
