{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day17 (
    day17a
  , day17b
  , pascals
  , pascalIx
  , ixPascal
  , neighbs
  , symmer
  ) where

import           AOC.Common                  ((!!!), factorial, freqs, lookupFreq, foldMapParChunk, sortSizedBy)
import           AOC.Common.Point            (Point, parseAsciiSet)
import           AOC.Solver                  ((:~>)(..))
import           Control.DeepSeq             (force, NFData)
import           Control.Monad               (replicateM)
import           Control.Monad.State         (State, state, evalState)
import           Data.Coerce                 (coerce)
import           Data.Foldable               (toList)
import           Data.IntMap                 (IntMap)
import           Data.IntSet                 (IntSet)
import           Data.List                   (scanl')
import           Data.Maybe                  (fromMaybe, fromJust, mapMaybe)
import           Data.Proxy                  (Proxy(..))
import           Data.Semigroup              (Sum(..))
import           Data.Set                    (Set)
import           Data.Tuple.Strict           (T2(..))
import           GHC.Generics                (Generic)
import           GHC.TypeNats                (KnownNat, type (+), natVal)
import           Linear                      (V2(..))
import qualified Data.IntMap                 as IM
import qualified Data.IntMap.Monoidal.Strict as MIM
import qualified Data.IntSet                 as IS
import qualified Data.Set                    as S
import qualified Data.Vector.Unboxed.Sized   as V

pascals :: [[Int]]
pascals = repeat 1 : map (tail . scanl' (+) 0) pascals

pascalIx :: V.Vector n Int -> Int
pascalIx = sum
         . zipWith (\p x -> ((0:p) !! x)) (tail pascals)
         . V.toList

ixPascal :: forall n. KnownNat n => Int -> V.Vector n Int
ixPascal x = fromJust . V.fromList . reverse $
    evalState (replicateM n go) (T2 x (reverse p0))
  where
    n  = fromIntegral (natVal (Proxy @n))
    p0 = take n (tail pascals)
    go :: State (T2 Int [[Int]]) Int
    go = state $ \(T2 y (p:ps)) ->
      let qs = takeWhile (<= y) (0:p)
      in  (length qs - 1, T2 (y - last qs) ps)

doubleIx :: forall n. Int -> V.Vector (n + 2) Int -> Int
doubleIx n x = x0 `V.index` 0
             + (x0 `V.index` 1) * n
             + pascalIx xs * (n*n)
  where
    (x0, xs) = V.splitAt @2 @n x

ixDouble :: forall n. KnownNat n => Int -> Int -> V.Vector (n + 2) Int
ixDouble n i = V.fromTuple @_ @_ @2 (y, x) V.++ ixPascal @n a
  where
    (a, b) = i `divMod` (n*n)
    (x, y) = b `divMod` n

-- stepper
--     :: forall n a. (Ord a, Num a, V.Unbox a)
--     => Map (V.Vector n a) (Map (V.Vector n a) Int)    -- ^ symmetry map
--     -> Set (V.Vector (n + 2) a)
--     -> Set (V.Vector (n + 2) a)
-- stepper syms cs = stayAlive <> comeAlive
--   where
--     chnk :: Int
--     chnk = min 1000 (max 10 (S.size cs `div` 100))
--     neighborCounts :: Map (V.Vector (n + 2) a) Int
--     neighborCounts = coerce (foldMapParChunk @(MM.MonoidalMap (V.Vector (n + 2) a) (Sum Int)) chnk id)
--       [ M.fromSet (getWeight syms c) (S.fromList . map symmer $ neighbs c)
--       | c <- S.toList cs
--       ]
--     stayAlive = M.keysSet . M.filter (\n -> n == 2 || n == 3) $
--                   neighborCounts `M.restrictKeys` cs
--     comeAlive = M.keysSet . M.filter (== 3) $
--                   neighborCounts `M.withoutKeys`  cs
--     symmer x = x0 V.++ xs'
--       where
--         (x0, xs) = V.splitAt @2 @n x
--         xs'      = sortSizedBy compare . V.map abs $ xs

data NCount =
      NOne
    | NTwo
    | NThree
    | NMany
  deriving (Eq, Ord, Generic)
instance NFData NCount

nValid :: NCount -> Maybe Bool
nValid = \case
    NTwo   -> Just False
    NThree -> Just True
    _      -> Nothing

instance Semigroup NCount where
    (<>) = \case
      NOne -> \case
        NOne   -> NTwo
        NTwo   -> NThree
        _      -> NMany
      NTwo -> \case
        NOne   -> NThree
        _      -> NMany
      _        -> const NMany

stepper_
    :: forall n. KnownNat n
    => Int      -- ^ how big the xy plane is
    -> IntMap (IntMap NCount)        -- ^ symmetry map
    -> IntSet
    -> IntSet
stepper_ nxy syms cs = stayAlive <> comeAlive
  where
    chnk :: Int
    chnk = min 1000 (max 10 (IS.size cs `div` 100))
    neighborCounts :: IntMap Bool
    neighborCounts = IM.mapMaybe nValid
                   $ coerce (foldMapParChunk @(MIM.MonoidalIntMap NCount) chnk id)
      [   IM.mapMaybe id
        . IM.fromSet (getWeight_ nxy syms c)
        . IS.fromList
        . map (doubleIx nxy . symmerD)
        $ neighbs (ixDouble nxy c)
      | c <- IS.toList cs
      ]
    stayAlive = IM.keysSet $
                  neighborCounts `IM.restrictKeys` cs
    comeAlive = IM.keysSet . IM.filter id $
                  neighborCounts `IM.withoutKeys`  cs
    symmerD :: V.Vector (n + 2) Int -> V.Vector (n + 2) Int
    symmerD x = x0 V.++ symmer xs
      where
        (x0, xs) = V.splitAt @2 @n x

getWeight_
    :: Int                    -- ^ how big the xy plane is
    -> IntMap (IntMap NCount)
    -> Int
    -> Int
    -> Maybe NCount
getWeight_ nxy syms x y = zWeight <> selfWeight
  where
    (xs, x0) = x `divMod` (nxy*nxy)
    (ys, y0) = y `divMod` (nxy*nxy)
    zWeight = IM.lookup xs =<< IM.lookup ys syms
    selfWeight
      | xs == ys && x0 /= y0  = Just NOne
      | otherwise             = Nothing

-- getWeight
--     :: forall n a. (Ord a, V.Unbox a)
--     => Map (V.Vector n a) (Map (V.Vector n a) Int)
--     -> V.Vector (n + 2) a
--     -> V.Vector (n + 2) a
--     -> Int
-- getWeight syms x y = zWeight + selfWeight
--   where
--     (x0, xs) = V.splitAt @2 @n x
--     (y0, ys) = V.splitAt @2 @n y
--     zWeight = fromMaybe 0 $
--       M.lookup xs =<< M.lookup ys syms
--     selfWeight
--       | xs == ys && x0 /= y0  = 1
--       | otherwise             = 0

neighbs :: (Num a, V.Unbox a) => V.Vector n a -> [V.Vector n a]
neighbs p = tail $ V.mapM (\x -> [x,x-1,x+1]) p

-- neighborWeights
--     :: (KnownNat n, Num a, Ord a, Enum a, V.Unbox a)
--     => a            -- ^ maximum
--     -> Map (V.Vector n a) (Map (V.Vector n a) Int)
-- neighborWeights mx =
--         M.fromSet ( M.mapKeysWith (+) symmer
--                   . M.fromList
--                   . map (,1)
--                   . neighbs
--                   )
--       $ allZs
--   where
--     symmer    = sortSizedBy compare . V.map abs
--     allZs     = S.fromList $ evalStateT (V.replicateM go) 0
--       where
--         go = StateT $ \i -> map dup [i .. mx]

neighborWeights_
    :: forall n. KnownNat n
    => Int            -- ^ maximum
    -> IntMap (IntMap NCount)
neighborWeights_ mx = IM.fromList
    [ ( x
      , IM.fromListWith (<>)
      . map (\g -> (pascalIx (symmer @n g), NOne))
      $ neighbs (ixPascal x)
      )
    | x <- [0 .. n - 1]
    ]
  where
    n = pascals !! fromIntegral (natVal (Proxy @n)) !! mx

symmer :: V.Vector n Int -> V.Vector n Int
symmer = sortSizedBy compare . V.map abs

-- -- used to test finalWeights
-- _duplicands
--     :: (Ord a, Num a, Enum a)
--     => a      -- ^ maximum
--     -> Int    -- ^ length (dimension)
--     -> Map [a] Int
-- _duplicands mx n = freqs . map symmer $ replicateM n [-mx .. mx]
--   where
--     symmer    = sort . map abs

finalWeight
    :: (KnownNat n, Num a, Ord a, V.Unbox a)
    => V.Vector n a
    -> Int
finalWeight x = process . freqs . drop 2 . V.toList $ x
  where
    n = V.length x - 2
    process mp = (2 ^ numNonZeroes) * perms
      where
        numNonZeroes = n - lookupFreq 0 mp
        perms = factorial n
          `div` product (factorial <$> mp)

day17
    :: forall n. KnownNat n
    => Set Point :~> Int
day17 = MkSol
    { sParse = Just . parseMap
    , sShow  = show
    , sSolve = \(S.toList->x) ->
        let bounds  = maximum (concatMap toList x) + 1
            nxy = bounds + 12
            shifted = IS.fromList $
                (\(V2 i j) -> i + j * nxy) . (+ 6) <$> x
            wts = force $ neighborWeights_ @n 6
        in  Just . sum
                 . IM.fromSet (finalWeight @(n+2) . ixDouble nxy)
                 . (!!! 6)
                 -- . zipWith traceShow [0..]
                 . iterate (force . stepper_ @n nxy wts)
                 $ shifted
    }
{-# INLINE day17 #-}

day17a :: Set Point :~> Int
day17a = day17 @1

day17b :: Set Point :~> Int
day17b = day17 @2

-- d=5: 5760 / 16736; 274ms     -- with unboxed, 96ms
-- d=6: 35936 / 95584; 1.5s     -- with unboxed, 309ms
-- d=7: 178720 / 502240; 7.7s
-- d=8: ? / 2567360; 30s
-- d=9: 4333056 / 12764416; 2m20s
-- d=10: ? / 62771200; 8m58s    -- with unboxed, 1m6s
-- d=11: ? / 309176832; 43m54s  -- with unboxed, 5m3s
-- d=12: ? / 1537981440 -- with unboxed, 22m10s

parseMap
    :: String
    -> Set Point
parseMap = parseAsciiSet (== '#')
