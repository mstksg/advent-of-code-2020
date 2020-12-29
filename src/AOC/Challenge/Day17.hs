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
  -- , pascals
  , neighborWeights
  , neighborWeightsNoCache
  -- , NCount(..)
  ) where

import           AOC.Common                  ((!!!), factorial, freqs, lookupFreq, foldMapParChunk)
import           AOC.Common.Point            (Point, parseAsciiSet)
import           AOC.Solver                  ((:~>)(..))
import           Control.DeepSeq             (force, NFData)
import           Data.Coerce                 (coerce)
import           Data.Foldable               (toList)
import           Data.IntMap                 (IntMap)
import           Data.IntSet                 (IntSet)
import           Data.List                   (scanl', sort)
import           Data.Set                    (Set)
import           GHC.Generics                (Generic)
import           Linear                      (V2(..))
import qualified Data.IntMap                 as IM
import qualified Data.IntMap.Monoidal.Strict as MIM
import qualified Data.IntSet                 as IS
import qualified Data.Set                    as S

pascals :: [[Int]]
pascals = repeat 1 : map (tail . scanl' (+) 0) pascals

pascalIx :: [Int] -> Int
pascalIx = sum
         . zipWith (\p x -> ((0:p) !! x)) (tail pascals)

ixPascal
    :: Int      -- ^ dimension
    -> Int
    -> [Int]
ixPascal n x = go x (reverse p0) []
  where
    p0 = take n (tail pascals)
    go :: Int -> [[Int]] -> [Int] -> [Int]
    go _ []     r = r
    go y (p:ps) r = go (y - last qs) ps ((length qs - 1) : r)
      where
        qs = takeWhile (<= y) (0:p)

ixDouble :: Int -> Int -> Int -> [Int]
ixDouble d n i = y : x : ixPascal d a
  where
    (a, b) = i `divMod` (n*n)
    (x, y) = b `divMod` n

neighbs2d :: Int -> Int -> [Int]
neighbs2d n i =
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
    -> IntMap (IntMap NCount)        -- ^ symmetry map
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
            pNeighbs = syms IM.! pIx
            gNeighbs = neighbs2d nxy gIx
      ]
    stayAlive = IM.keysSet neighborCounts `IS.intersection` cs
    comeAlive = IM.keysSet (IM.filter id neighborCounts) `IS.difference` cs

neighbs :: Num a => [a] -> [[a]]
neighbs = tail . traverse (\x -> [x,x-1,x+1])


flipIM
    :: IntMap (IntMap a)
    -> IntMap (IntMap a)
flipIM xs = IM.fromListWith (<>) $
    [ (y, IM.singleton x z)
    | (x, ys) <- IM.toList xs
    , (y, z ) <- IM.toList ys
    ]

neighborWeights
    :: Int            -- ^ dimension
    -> Int            -- ^ maximum
    -> IntMap (IntMap NCount)
neighborWeights d mx = flipIM . IM.fromDistinctAscList $
    [ ( x
      , IM.fromListWith (<>)
      . map (\g -> (pascalIx (sort (map abs g)), NOne))
      $ neighbs (ixPascal d x)
      )
    | x <- [0 .. n - 1]
    ]
  where
    n = pascals !! d !! mx

neighborWeightsNoCache
    :: Int            -- ^ dimension
    -> Int            -- ^ maximum
    -> a
    -> IntMap (IntMap NCount)
neighborWeightsNoCache d mx q = (q `seq`) $ flipIM . IM.fromDistinctAscList $
    [ ( x
      , IM.fromListWith (<>)
      . map (\g -> (pascalIx (sort (map abs g)), NOne))
      $ neighbs (ixPascal d x)
      )
    | x <- [0 .. n - 1]
    ]
  where
    n = pascals !! d !! mx

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
    :: (Num a, Ord a)
    => Int              -- ^ dim
    -> [a]
    -> Int
finalWeight n x = process . freqs $ x
  where
    process mp = (2 ^ numNonZeroes) * perms
      where
        numNonZeroes = n - lookupFreq 0 mp
        perms = factorial n
          `div` product (factorial <$> mp)

day17
    :: Int
    -> Set Point :~> Int
day17 d = MkSol
    { sParse = Just . parseMap
    , sShow  = show
    , sSolve = \(S.toList->x) ->
        let bounds  = maximum (concatMap toList x) + 1
            nxy     = bounds + 12
            shifted = IS.fromList $
                (\(V2 i j) -> i + j * nxy) . (+ 6) <$> x
            -- wts = force $ neighborWeights d 6
            wts = force $ neighborWeightsNoCache d 6 x
        in  Just . sum
                 . IM.fromSet (finalWeight d . drop 2 . ixDouble d nxy)
                 . (!!! 6)
                 -- . zipWith traceShow [0..]
                 . iterate (force . stepper nxy wts)
                 $ shifted
    }
{-# INLINE day17 #-}

day17a :: Set Point :~> Int
day17a = day17 1

day17b :: Set Point :~> Int
day17b = day17 2

-- d=5: 5760 / 16736; 274ms     -- with unboxed, 96ms, with pre-neighb: 35ms
-- d=6: 35936 / 95584; 1.5s     -- with unboxed, 309ms, with pre-neighb: 105ms
-- d=7: 178720 / 502240; 7.7s   -- with pre-neighbs: 356ms (no cache: 290ms)
-- d=8: ? / 2567360; 30s        -- with pre-neighbs: 1.2s (no cache: 690ms)
-- d=9: 4333056 / 12764416; 2m20s   -- with pre-neighbs: 4.8s (no cache: 1.5s)
--                                                  no knownnat: 4.3s
-- d=10: ? / 62771200; 8m58s    -- with unboxed, 1m6s, with pre-neighb: 21s (no cache: 2.56?)
--                                      no knownnat: 19s
-- d=11: ? / 309176832; 43m54s  -- with unboxed, 5m3s, with pre-neighb: 1m43s (no cache: 4.731?)
-- d=12: ? / 1537981440 -- with unboxed, 22m10s, with pre-neighb: 8m30s

parseMap
    :: String
    -> Set Point
parseMap = parseAsciiSet (== '#')
