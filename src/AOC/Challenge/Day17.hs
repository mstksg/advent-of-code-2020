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
  , neighborWeights
  , neighborWeights_
  , neighbs2
  , ixDouble
  , doubleIx
  ) where

import           AOC.Common                  ((!!!), factorial, freqs, lookupFreq, foldMapParChunk, sortSizedBy)
import           AOC.Common.Point            (Point, parseAsciiSet)
import           AOC.Solver                  ((:~>)(..))
import           Control.DeepSeq             (force, NFData)
import           Control.Monad               (replicateM)
import           Control.Monad.ST
import           Control.Monad.State         (State, state, evalState)
import           Data.Coerce                 (coerce)
import           Data.Foldable
import           Data.Foldable               (toList)
import           Data.IntMap                 (IntMap)
import           Data.IntSet                 (IntSet)
import           Data.List                   (scanl')
import           Debug.Trace
import           Data.Semigroup
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Proxy                  (Proxy(..))
import           Data.Set                    (Set)
import           Data.Traversable
import           Data.Tuple.Strict           (T2(..))
import           GHC.Generics                (Generic)
import           GHC.TypeNats                (KnownNat, type (+), natVal)
import           Linear                      (V2(..))
import qualified Data.IntMap                 as IM
import qualified Data.IntMap.Monoidal.Strict as MIM
import qualified Data.IntSet                 as IS
import qualified Data.List.NonEmpty          as NE
import qualified Data.Set                    as S
import qualified Data.Vector                 as UV
import qualified Data.Vector.Mutable         as UMV
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

neighbs2 :: Int -> Int -> [Int]
neighbs2 n i =
    [ i + dx + n*dy
    | dx <- [0,-1,1]
    , dy <- [0,-1,1]
    ]

data NCount =
      NOne
    | NTwo
    | NThree
    | NMany
  deriving (Show, Eq, Ord, Generic)
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

stepper
    :: Int      -- ^ how big the xy plane is
    -> UV.Vector (IntMap NCount)        -- ^ symmetry map
    -> IntSet
    -> IntSet
stepper nxy syms cs = stayAlive <> comeAlive
  where
    chnk :: Int
    chnk = min 1000 (max 10 (IS.size cs `div` 100))
    neighborCounts :: IntMap Bool
    neighborCounts = IM.mapMaybe nValid
                   $ coerce (foldMapParChunk @(MIM.MonoidalIntMap NCount) chnk id)
      [ IM.fromListWith (<>) $
        [ (gnIx + pnIx * (nxy*nxy), pnC)
        | (pnIx, pnC) <- IM.toList pNeighbs
        , gnIx <- gNeighbs
        ] <>
        [ (gnIx + pIx * (nxy*nxy), NOne)
        | gnIx <- tail gNeighbs
        ]
      | c <- IS.toList cs
      , let (pIx,gIx) = c `divMod` (nxy*nxy)
            pNeighbs = syms UV.! pIx
            gNeighbs = neighbs2 nxy gIx
      ]
    stayAlive = IM.keysSet $
                  neighborCounts `IM.restrictKeys` cs
    comeAlive = IM.keysSet . IM.filter id $
                  neighborCounts `IM.withoutKeys`  cs

stepper_
    :: Int      -- ^ how big the xy plane is
    -> UV.Vector (IntMap NCount)        -- ^ symmetry map
    -> UV.Vector Bool
    -> UV.Vector Bool
stepper_ nxy syms cs = UV.imap go cs
  where
    go :: Int -> Bool -> Bool
    go i self = maybe False flipper totPs
      where
        flipper
          | self      = const True
          | otherwise = id
        (pIx,gIx) = i `divMod` (nxy*nxy)
        pNeighbs = syms UV.! pIx
        gNeighbs = neighbs2 nxy gIx
        totPs    = (nValid . sconcat =<<) . NE.nonEmpty $
          [ pnC
          | (pnIx, pnC) <- IM.toList pNeighbs
          , gnIx <- gNeighbs
          , fromMaybe False $ cs UV.!? (gnIx + pnIx * (nxy*nxy))
          -- makes sense because we have to worry simulate the points at
          -- the borders too
          ] <>
          [ NOne
          | gnIx <- tail gNeighbs
          , fromMaybe False $ cs UV.!? (gnIx + pIx * (nxy*nxy))
          ]

  -- where
    -- chnk :: Int
    -- chnk = min 1000 (max 10 (IS.size cs `div` 100))
    -- neighborCounts :: IntMap Bool
    -- neighborCounts = IM.mapMaybe nValid
    --                $ coerce (foldMapParChunk @(MIM.MonoidalIntMap NCount) chnk id)
    --   [ IM.fromListWith (<>) $
    --     [ (gnIx + pnIx * (nxy*nxy), pnC)
    --     | (pnIx, pnC) <- IM.toList pNeighbs
    --     , gnIx <- gNeighbs
    --     ] <>
    --     [ (gnIx + pIx * (nxy*nxy), NOne)
    --     | gnIx <- tail gNeighbs
    --     ]
    --   | c <- IS.toList cs
    --   , let (pIx,gIx) = c `divMod` (nxy*nxy)
    --         pNeighbs = syms IM.! pIx
    --         gNeighbs = neighbs2 nxy gIx
    --   ]
    -- stayAlive = IM.keysSet $
    --               neighborCounts `IM.restrictKeys` cs
    -- comeAlive = IM.keysSet . IM.filter id $
    --               neighborCounts `IM.withoutKeys`  cs


neighbs :: (Num a, V.Unbox a) => V.Vector n a -> [V.Vector n a]
neighbs p = tail $ V.mapM (\x -> [x,x-1,x+1]) p

flipIM
    :: IntMap (IntMap a)
    -> IntMap (IntMap a)
flipIM xs = IM.fromListWith (<>)
    [ (y, IM.singleton x z)
    | (x, ys) <- IM.toList xs
    , (y, z ) <- IM.toList ys
    ]

-- should compute this directly
neighborWeights
    :: forall n. KnownNat n
    => Int            -- ^ maximum
    -> IntMap (IntMap NCount)
neighborWeights mx = flipIM . IM.fromList $
    [ ( x
      , IM.fromListWith (<>)
      . map (\g -> (pascalIx (symmer @n g), NOne))
      $ neighbs (ixPascal x)
      )
    | x <- [0 .. n - 1]
    ]
  where
    n = pascals !! fromIntegral (natVal (Proxy @n)) !! mx

neighborWeights_
    :: forall n. KnownNat n
    => Int            -- ^ maximum
    -> UV.Vector (IntMap NCount)
neighborWeights_ mx = runST $ do
    v  <- UMV.replicate n1 IM.empty
    for_ [0 .. n - 1] $ \x -> do
      let nmap = IM.fromListWith (<>)
               . map (\g -> (pascalIx (symmer @n g), NOne))
               $ neighbs (ixPascal x)
      for_ (IM.toList nmap) $ \(i, c) ->
        UMV.modify v (IM.insert x c) i
    UV.freeze v
  where
    n  = pascals !! fromIntegral (natVal (Proxy @n)) !! mx
    n1 = pascals !! fromIntegral (natVal (Proxy @n)) !! (mx+1)

neighborWeights2
    :: forall n. KnownNat n
    => Int            -- ^ maximum
    -> UV.Vector (IntMap NCount)
neighborWeights2 mx = UV.generate n $
        IM.fromListWith (<>)
      . map (\g -> (pascalIx (symmer @n g), NOne))
      . neighbs
      . ixPascal
  where
    n  = pascals !! fromIntegral (natVal (Proxy @n)) !! mx

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
            wts = force $ neighborWeights2 @n 6
            nitems = UV.length wts * nxy * nxy
            v0  = UV.generate nitems (`IS.member` shifted)
        in  Just . sum
                 . UV.imap (\i b ->
                              if b then 0
                                   else finalWeight @(n+2) (ixDouble nxy i)
                           )
                 -- . IM.fromSet (finalWeight @(n+2) . ixDouble nxy)
                 . (!!! 6)
                 -- . zipWith traceShow [0..]
                 . iterate (force . stepper_ nxy wts)
                 $ v0
    }
{-# INLINE day17 #-}

day17a :: Set Point :~> Int
day17a = day17 @1

day17b :: Set Point :~> Int
day17b = day17 @2

-- d=5: 5760 / 16736; 274ms     -- with unboxed, 96ms
-- d=6: 35936 / 95584; 1.5s     -- with unboxed, 309ms, with pre-neighb: 105ms
-- d=7: 178720 / 502240; 7.7s
-- d=8: ? / 2567360; 30s
-- d=9: 4333056 / 12764416; 2m20s
-- d=10: ? / 62771200; 8m58s    -- with unboxed, 1m6s, with pre-neighb: 21s
-- d=11: ? / 309176832; 43m54s  -- with unboxed, 5m3s, with pre-neighb: 1m43s
-- d=12: ? / 1537981440 -- with unboxed, 22m10s, with pre-neighb: 8m30s

parseMap
    :: String
    -> Set Point
parseMap = parseAsciiSet (== '#')
