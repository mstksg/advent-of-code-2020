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
import           Control.Monad.ST            (runST)
import           Control.Monad.State.Strict  (StateT(..), evalStateT, gets)
import           Data.Foldable               (for_)
import           Data.List.Split             (splitOn)
import           Data.Tuple.Strict           (T2(..), sfst, ssnd)
import           GHC.Int                     (Int32)
import           Text.Read                   (readMaybe)
import qualified Data.Vector.Unboxed.Mutable as MV

day15a :: [Int] :~> Int
day15a = MkSol
    { sParse = traverse readMaybe . splitOn ","
    , sShow  = show
    , sSolve = Just . looper 2020
    }

day15b :: [Int] :~> Int
day15b = MkSol
    { sParse = sParse day15a
    , sShow  = show
    , sSolve = Just . looper 30000000
    }

looper :: Int -> [Int] -> Int
looper n xs0 = runST $ flip evalStateT (T2 0 0 :: T2 Int32 Int) $ do
    v <- MV.replicate n 0
    for_ xs0 $ \y -> StateT $ \(T2 i _) ->
      (,T2 (i+1) y) <$> MV.unsafeWrite v y (i+1)
    whileM_ (gets ((< n32) . sfst)) $ StateT $ \(T2 i x) -> do
      lst <- MV.unsafeRead v x
      MV.unsafeWrite v x i
      let j | lst ==  0 = 0
            | otherwise = i - lst
      pure ((),T2 (i+1) (fromIntegral j))
    gets ssnd
  where
    n32 :: Int32
    n32 = fromIntegral n
