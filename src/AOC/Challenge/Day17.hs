{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day17 (
    day17a
  , day17b
  , RunTree(..)
  , runTreeWeightsF
  , runTreeListF
  , runTreeNeighbs
  , pascals
  , pascalRunIx
  , pascalIx
  , ixPascal
  , neighborWeights
  , neighborWeights_
  , neighborWeights__
  , oldNeighborWeights
  , neighborPairs
  , neighborPairs_
  , loadNeighborWeights
  , continuousRunNeighbors
  , pointRunNeighbs
  , allPointRuns
  , vecRunNeighbs
  , vecRunNeighbs_
  , allVecRuns
  -- , NCount(..)
  ) where

import           AOC.Common                  ((!!!), factorial, freqs, lookupFreq, foldMapParChunk)
import           AOC.Common.Point            (Point, parseAsciiSet)
import           AOC.Solver                  ((:~>)(..))
import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.MVar     (takeMVar, putMVar, newEmptyMVar)
import           Control.Concurrent.QSem     (waitQSem, signalQSem, newQSem)
import           Control.DeepSeq             (force, NFData)
import           Control.Exception           (bracket_, evaluate)
import           Control.Monad               (unless, void, when, guard)
import           Control.Monad.ST            (runST)
import           Control.Monad.State         (StateT(..), evalStateT, get, modify)
import           Control.Monad.Trans.Class
import           Data.Bifunctor              (second, first)
import           Data.Coerce                 (coerce)
import           Data.Foldable               (toList, for_, asum)
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.IntMap.Strict          (IntMap)
import           Data.IntSet                 (IntSet)
import           Data.List                   (scanl', sort, foldl1', foldl')
import           Data.List.NonEmpty          (NonEmpty(..))
import           Data.List.Split             (chunksOf)
import           Data.Map                    (Map)
import           Data.Maybe                  (catMaybes, fromMaybe, maybeToList, mapMaybe, isJust)
import           Data.Semigroup              (Sum(..))
import           Data.Set                    (Set)
import           Data.Tuple.Strict           (T2(..), sfst, ssnd, T3(..))
import           Debug.Trace
import           GHC.Generics                (Generic)
import           Linear                      (V2(..))
import           System.IO.Unsafe            (unsafePerformIO)
import           Text.Printf                 (printf)
import qualified Control.Foldl               as F
import qualified Data.IntMap.Monoidal.Strict as MIM
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map.Strict             as M
import qualified Data.MemoCombinators        as Memo
import qualified Data.MemoCombinators.Class  as Memo
import qualified Data.Set                    as S
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MV
import qualified Data.Vector.Unboxed         as VU
import qualified Database.SQLite.Simple      as D

data RunTree k a = RTNode (Map k (RunTree k a))
                 | RTLeaf a
  deriving Show

makeBaseFunctor ''RunTree

runTreeNeighbs :: VU.Vector Int -> [([Int], NCount)]
runTreeNeighbs xs = mapMaybe sequence
                  $ hylo runTreeListF (runTreeWeightsF xs) (0, M.singleton (True, xs) p0)
-- first VU.fromList
  where
    p0 = product . map factorial $ VU.toList xs

runTreeListF
    :: RunTreeF k a [([k],a)]
    -> [([k], a)]
runTreeListF = \case
    RTNodeF mp -> concatMap (\(k, xs) -> map (first (k:)) xs) (M.toList mp)
    RTLeafF x -> [([], x)]

-- see if we can get the Chunk method ported to here
runTreeWeightsF
    :: VU.Vector Int                  -- original
    -> (Int, Map (Bool, VU.Vector Int) Int)
    -> RunTreeF Int (Maybe NCount) (Int, Map (Bool, VU.Vector Int) Int)
runTreeWeightsF xs0 (i, opts)
    | i < VU.length xs0 = RTNodeF $ (i+1,) <$> M.fromListWith (M.unionWith (+)) opts'
    | otherwise         = RTLeafF $ M.foldMapWithKey pullSame opts
  where
    pullSame (True , _) _ = Nothing
    pullSame (_    , _) p = Just (toNCount p)
    opts' = do
      ((b, xs), p) <- M.toList opts
      let l  = fromMaybe 0 $ xs VU.!? (i-1)
          r  = xs VU.!? (i + 1)
          x  = xs VU.! i
          x0 = xs0 VU.! i
      (xrContrib, xs', xContrib, p') <- case r of
        Nothing -> pure (x, xs, x, p `div` factorial x)
        Just r' ->
          [ ( totContrib
            , xs VU.// [(i, x-xContrib),(i+1, r'-rContrib)]
            , xContrib
            , p `div` factorial xContrib `div` factorial rContrib
            )
          | totContrib <- [0..(x+r')]
          , xContrib <- [max 0 (totContrib-r')..min x totContrib]
          , let rContrib = totContrib - xContrib
          ]
      let p''
            | i == 1    = p' * (2^l)
            | otherwise = p' `div` factorial l
          res = l + xrContrib
      pure (res, M.singleton (b && xContrib == x0, xs') p'')

pascals :: [[Int]]
pascals = repeat 1 : map (tail . scanl' (+) 0) pascals

pascalIx :: [Int] -> Int
pascalIx = sum . zipWith (\p x -> ((0:p) !! x)) (tail pascals)

pascalRunIx :: IntMap Int -> Int
pascalRunIx = pascalIx . concatMap (uncurry (flip replicate)) . IM.toList

pascalVecRunIx :: VU.Vector Int -> Int
pascalVecRunIx = go 0 ((0:) <$> tail pascals). VU.toList
  where
    go !tot !cs = \case
      []   -> tot
      x:xs ->
        let (c,cs') = splitAt x cs
        in  go (tot + sum (map head c)) (tail <$> cs') xs

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
    -> V.Vector (IntMap NCount)        -- ^ symmetry map
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
            pNeighbs = syms V.! pIx
            gNeighbs = neighbs2d nxy gIx
      ]
    stayAlive = IM.keysSet neighborCounts `IS.intersection` cs
    comeAlive = IM.keysSet (IM.filter id neighborCounts) `IS.difference` cs

neighbs :: (Num a, Eq a) => a -> [a] -> [[a]]
neighbs mx = tail . traverse (\x -> if | x == mx   -> [x,x-1]
                                       | x == 0    -> [x,x+1,x+1]
                                       | otherwise -> [x,x-1,x+1]
                             )
{-# INLINE neighbs #-}

oldNeighborWeights
    :: Int            -- ^ dimension
    -> Int            -- ^ maximum
    -> V.Vector (IntMap NCount)
oldNeighborWeights d mx = runST $ do
    v <- MV.replicate n IM.empty
    for_ [0 .. n-1] $ \x ->
      for_ (neighbs mx (ixPascal d x)) $ \i -> do
        MV.unsafeModify v (IM.insertWith (flip (<>)) x NOne) $
          pascalIx (sort i)
    V.freeze v
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

-- | Calculate the neighbors and weights from a continuous run of the same
-- number
--
-- This could be memoized but maybe it doesn't matter, it's pretty fast?
continuousRunNeighbors
    :: Int      -- ^ max
    -> Int      -- ^ number
    -> Int      -- ^ run length
    -> [(IntMap Int, NCount)]    -- ^ neighbors run map, and weight
continuousRunNeighbors = Memo.memo3 Memo.integral Memo.integral Memo.integral
                            continuousRunNeighbors_

continuousRunNeighbors_
    :: Int      -- ^ max
    -> Int      -- ^ number
    -> Int      -- ^ run length
    -> [(IntMap Int, NCount)]    -- ^ neighbors run map, and weight
continuousRunNeighbors_ mx n r = ((IM.singleton n r, NOne):) . map wt . flip evalStateT r $ do
    xs <- fmap (IM.fromDistinctAscList . catMaybes) . traverse (\o -> fmap (o,) <$> go) $ opts
    lastCall <- get
    let ys
          | lastCall > 0 = IM.insertWith addTup lastVal (T2 lastCall (factorial lastCall)) xs
          | otherwise    = xs
    ys <$ guard (IM.keysSet ys /= IS.singleton n)
  where
    -- unchanged has to be the first item for the 'tail' trick to work in
    -- pointRunNeighbs
    go :: StateT Int [] (Maybe (T2 Int Int))
    go = StateT $ \i ->
        (Nothing, i)
      : [ ( Just (T2 j (factorial j))
          , i - j
          )
        | j <- [1..i]
        ]
    wt :: IntMap (T2 Int Int) -> (IntMap Int, NCount)
    wt mp = (sfst <$> mp, toNCount $ factorial r `div` product (ssnd <$> mp))
    (opts, lastVal)
      | n == 0    = ([0, 1], 1)     -- hm, this is wasteful?
      | n == mx   = ([n-1], n)
      | otherwise = ([n-1, n], n+1)
    addTup (T2 x fx) (T2 y fy) = T2 (x + y) (fx * fy)

-- | All point runs for a given dim and max
allPointRuns
    :: Int    -- ^ dim
    -> Int    -- ^ max
    -> [IntMap Int]
allPointRuns d mx = flip evalStateT d $ do
    xs <- fmap (IM.fromDistinctAscList . catMaybes) . traverse (\o -> fmap (o,) <$> go) $ [0 .. mx-1]
    lastCall <- get
    let ys
          | lastCall > 0 = IM.insert mx lastCall xs
          | otherwise    = xs
    pure ys
  where
    go :: StateT Int [] (Maybe Int)
    go = StateT $ \i ->
        (Nothing, i)
      : [ (Just j, i - j)
        | j <- [1..i]
        ]

-- | All neighbors (and weights) for a given point run
pointRunNeighbs
    :: Int                        -- ^ max
    -> IntMap Int                 -- ^ point runs
    -> [(IntMap Int, NCount)]     -- ^ neighbs and weights (potential duplicates)
pointRunNeighbs mx p = map (F.fold aggr)
                     . tail
                     -- whoa! there are some major redundancies here!
                     -- ie for 111223444, 1079 -> 486
                     . traverse (\(n, r) -> continuousRunNeighbors mx n r)
                     . IM.toList
                     $ p
  where
    aggr = (,) <$> F.Fold (\c (d, _) -> IM.unionWith (+) c d) IM.empty id
               <*> F.Fold (\c (_, d) -> c `mulNCount` d) NOne id

-- lots of waste here too but maybe it's faster?
-- nice thing is we can go directly to pascal-land
vecRunNeighbs_
    :: VU.Vector Int
    -> [(VU.Vector Int, NCount)]
vecRunNeighbs_ xs0 =
    mapMaybe pullSame $ runStateT (VU.imapM go xs0) (T3 xs0 True p0)
  where
    pullSame (_, (T3 _ True _)) = Nothing
    pullSame (x, (T3 _ _    c)) = Just (x, toNCount c)
    p0 = product . map factorial $ VU.toList xs0
    go :: Int -> Int -> StateT (T3 (VU.Vector Int) Bool Int) [] Int
    go i _ = StateT $ \(T3 xs b p) -> do
        let l  = fromMaybe 0 $ xs VU.!? (i-1)
            r  = xs VU.!? (i+1)
            x  = xs VU.! i
            x0 = xs0 VU.! i
        (xrContrib, xs', xContrib, p') <- case r of
          Nothing -> pure (x, xs, x, p `div` factorial x)
          Just r' ->
            [ ( totContrib
              , xs VU.// [(i, x-xContrib),(i+1, r'-rContrib)]
              , xContrib
              , p `div` factorial xContrib `div` factorial rContrib
              )
            | totContrib <- [0..(x+r')]
            , xContrib <- [max 0 (totContrib-r')..min x totContrib]
            , let rContrib = totContrib - xContrib
            ]
        let p''
              | i == 1    = p' * (2^l)
              | otherwise = p' `div` factorial l
            res = l + xrContrib
        pure (res, T3 xs' (b && xContrib == x0) p'')

data Chomper a = C { _cLeft2 :: !Bool        -- false if no exist, true if exist
                   , _cLeft  :: !(Maybe a)
                   , _cHere  :: !a
                   , _cOrig  :: !a
                   , _cRight :: ![a]
                   }

-- instance Data.MemoTable a => Memo.MemoTable (Chomper a) where
--     table f = \(C ll l x x0 rs) -> m ll l x x0 rs
--       where
--         m = Memo.memoize $ \ll -> Memo.memoize $ \l -> Memo.memoize $ \x ->
--               Memo.memoize $ \x0 -> Memo.memoize $ \rs -> f (C ll l x x0 rs)

toChomper :: [a] -> Maybe (Chomper a)
toChomper []     = Nothing
toChomper (x:xs) = Just (C False Nothing x x xs)

-- lots of waste here too but maybe it's faster?
-- nice thing is we can go directly to pascal-land
-- same amount of waste as the map method, but we can directly encode
-- i guess
vecRunNeighbs
    :: VU.Vector Int
    -> [(Int, NCount)]
vecRunNeighbs xs0 = mapMaybe pullSame $ go 0 True chomp0 p0 ((0:) <$> tail pascals)
  where
    xs0' = VU.toList xs0
    Just chomp0 = toChomper xs0'
    pullSame (True, _) = Nothing
    pullSame (_   , x) = Just x
    p0 = product . map factorial $ xs0'
    go !tot !allSame (C ll l x x0 rs) !p !cs = case rs of
        []    -> pure $
          let p'
                | isJust l && not ll = p * (2 ^ l') `div` factorial x
                | otherwise          = p `div` factorial l' `div` factorial x
              res   = l' + x
              c     = take res cs
              tot'  = tot + sum (map head c)
          in  (allSame && x == x0, (tot', toNCount p'))
        r:rs' -> do
          (xContrib, rContrib, p') <-
              [ ( xContrib
                , rContrib
                , p `div` factorial xContrib `div` factorial rContrib
                )
              | totContrib <- [0..(x+r)]
              , xContrib <- [max 0 (totContrib-r)..min x totContrib]
              , let rContrib = totContrib - xContrib
              ]
          let p''
                | ll' && not ll = p' * (2^l')
                | otherwise     = p' `div` factorial l'
              res = l' + xContrib + rContrib
              (c,cs') = splitAt res cs
              tot'    = tot + sum (map head c)
              chomp   = C ll' (Just (x - xContrib)) (r - rContrib) r rs'
          go tot' (allSame && xContrib == x0) chomp p'' (tail <$> cs')
      where
        (l', ll') = case l of
          Nothing -> (0, False)
          Just q  -> (q, True )

-- | All point runs for a given dim and max
allVecRuns
    :: Int    -- ^ dim
    -> Int    -- ^ max
    -> [VU.Vector Int]
allVecRuns d mx = go 0 d []
  where
    go i j rs
      | i < mx = do
          k <- [0..j]
          go (i + 1) (j - k) (k:rs)
      | otherwise = pure $ VU.fromListN (mx+1) (j:rs)

neighborPairs_
    :: Int    -- ^ dimension
    -> Int    -- ^ maximum
    -> [(Int, (Int, NCount))]
neighborPairs_ d mx =
    [ (pG, (pX, w))
    | x <- allVecRuns d mx
    , let pX = pascalVecRunIx x
    , (pG, w) <- vecRunNeighbs x
    ]
  -- where
  --   unRuns = concatMap (uncurry (flip replicate)) . zip [0..] . VU.toList


mulNCount :: NCount -> NCount -> NCount
mulNCount = \case
    NOne -> id
    NTwo -> \case
      NOne -> NTwo
      _    -> NMany
    NThree -> \case
      NOne -> NThree
      _    -> NMany
    NMany  -> const NMany

neighborPairs
    :: Int    -- ^ dimension
    -> Int    -- ^ maximum
    -> [(Int, (Int, NCount))]
neighborPairs d mx =
    [ (pG, (pX, w))
    | x <- allPointRuns d mx
    , let pX = pascalRunIx x
    , (g, w) <- pointRunNeighbs mx x
    , let pG = pascalRunIx g
    ]

neighborPairs__
    :: Int    -- ^ dimension
    -> Int    -- ^ maximum
    -> [(Int, (Int, NCount))]
neighborPairs__ d mx =
    [ (pG, (pX, w))
    | x <- allVecRuns d mx
    , let pX = pascalIx (unRuns (VU.toList x))
    , (g, w) <- runTreeNeighbs x
    , let pG = pascalIx (unRuns g)
    ]
  where
    unRuns = concatMap (uncurry (flip replicate)) . zip [0..]

neighborWeights
    :: Int            -- ^ dimension
    -> Int            -- ^ maximum
    -> V.Vector (IntMap NCount)
neighborWeights d mx = runST $ do
    v <- MV.replicate n IM.empty
    -- maybe keep a map of "done" pG/pX pairs and skip those
    for_ (neighborPairs d mx) $ \(pG, (pX, w')) ->
      MV.unsafeModify v (IM.insertWith (flip (<>)) pX w') pG
    V.freeze v
  where
    n = pascals !! d !! mx

neighborWeights_
    :: Int            -- ^ dimension
    -> Int            -- ^ maximum
    -> V.Vector (IntMap NCount)
neighborWeights_ d mx = runST $ do
    v <- MV.replicate n IM.empty
    -- maybe keep a map of "done" pG/pX pairs and skip those
    for_ (neighborPairs_ d mx) $ \(pG, (pX, w')) ->
      MV.unsafeModify v (IM.insertWith (flip (<>)) pX w') pG
    V.freeze v
  where
    n = pascals !! d !! mx

neighborWeights__
    :: Int            -- ^ dimension
    -> Int            -- ^ maximum
    -> V.Vector (IntMap NCount)
neighborWeights__ d mx = runST $ do
    v <- MV.replicate n IM.empty
    -- maybe keep a map of "done" pG/pX pairs and skip those
    for_ (neighborPairs__ d mx) $ \(pG, (pX, w')) ->
      MV.unsafeModify v (IM.insertWith (flip (<>)) pX w') pG
    V.freeze v
  where
    n = pascals !! d !! mx


toNCount :: Int -> NCount
toNCount = \case
    0 -> error "0 ncount"
    1 -> NOne
    2 -> NTwo
    3 -> NThree
    _ -> NMany

day17
    :: Int
    -> Set Point :~> Int
day17 d = MkSol
    { sParse = Just . parseAsciiSet (== '#')
    , sShow  = show
    , sSolve = \(S.toList->x) ->
        let bounds  = maximum (concatMap toList x) + 1
            nxy     = bounds + 12
            shifted = IS.fromList $
                (\(V2 i j) -> i + j * nxy) . (+ 6) <$> x
            -- wts = neighborWeights d 6
            wts = neighborWeights_ d (6 + length x - length x)   -- force no cache
            -- wts = unsafePerformIO $ loadNeighborWeights d (6 + length x - length x)
        -- in         Just . IS.size
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
-- d=8: 900288 / 2567360; 30s        -- with pre-neighbs: 1.2s (no cache: 690ms) (smallcache: 920ms)
-- d=9: 4333056 / 12764416; 2m20s   -- with pre-neighbs: 4.8s (no cache: 1.5s)
--                                                  no knownnat: 4.3s
-- d=10: 20251648 / 62771200; 8m58s    -- with unboxed, 1m6s, with pre-neighb: 21s (no cache: 2.56?)
--                                      no knownnat: 19s
--                                      smallcache: 12s
--                                      smart cache: 4.0s total
-- d=11: 93113856 / 309176832; 43m54s  -- with unboxed, 5m3s, with pre-neighb: 1m43s (no cache: 4.5s)
--                                      smallcache: 52s
--                                      8.8s v 7.7s
-- d=12: 424842240 / 1537981440 -- with unboxed, 22m10s, with pre-neighb: 8m30s (no cache: 7.4s)
--                                      smart cache: 21.5s total
--                                      21s vs 17s
-- d=13: 1932496896 / 7766482944 -- sqlite3 cache: 13.4s
--                                      smart cache: 1m10s total
--                                      new: 43s
-- d=14: 8778178560 / 39942504448 -- sqlite3 cache: 21.6s
--                                      new: 2m21s total
-- d=15: 39814275072 / 209681145856 -- sqlite3 cache: 32.5s, (including loading: 1m20s); smart cache: 4h35m
--    new method: total cache + run = 20m53s
-- d=16: ? / 1125317394432 -- build sqlite cache + run = 62m44; run = 2m25s

cacheNeighborWeights
    :: D.Connection
    -> Int    -- ^ dimension
    -> Int    -- ^ maximum
    -> IO ()
cacheNeighborWeights conn d mx = do
    dbsem <- newQSem 1
    threadsem <- newQSem 5
    done <- newEmptyMVar
    forEnd_ (chunkUp <$> chunksOf 5_000_000 (neighborPairs_ d mx)) $ \isLast pmap -> do
      waitQSem threadsem
      forkIO $ do
        let chunky   = IM.size pmap
            bunky    = sum $ IM.size <$> pmap
            lastSeen = fromIntegral (maximum (fst . IM.findMax <$> pmap) + 1)
        _ <- evaluate $ force pmap
        bracket_ (waitQSem dbsem) (signalQSem dbsem) $ do
          printf "[%05.2f%%] Cacheing chunk of size %d/%d ...\n" (lastSeen / n * 100) chunky bunky
          D.withTransaction conn $ D.executeMany conn
            "INSERT INTO cache(dim,source,target,weight) VALUES (?,?,?,?) ON CONFLICT(dim,source,target) DO UPDATE SET weight = weight + ? WHERE weight < 4"
            [ (d, x, y, c, c)
            | (x, ys) <- IM.toList pmap
            , (y, z ) <- IM.toList ys
            , let c = fromCount z
            ]
        signalQSem threadsem
        when isLast $ putMVar done ()
    takeMVar done
    threadDelay 1000000
  where
    fromCount :: NCount -> Int
    fromCount = \case
      NOne   -> 1
      NTwo   -> 2
      NThree -> 3
      NMany  -> 4
    n :: Double
    n = fromIntegral (pascals !! d !! mx)
    chunkUp :: [(Int, (Int, NCount))] -> IntMap (IntMap NCount)
    chunkUp = IM.fromListWith (IM.unionWith (<>))
            . (map . second) (uncurry IM.singleton)

forEnd_
    :: Applicative m
    => [a]
    -> (Bool -> a -> m c)
    -> m ()
forEnd_ xs0 f = go xs0
  where
    go []     = pure ()
    go [x]    = void $ f True x
    go (x:xs) = f False x *> go xs

loadCache
    :: D.Connection
    -> Int      -- ^ dimensions
    -> Int      -- ^ max
    -> IO (V.Vector (IntMap NCount))
loadCache conn d mx =
    V.generateM n $ \src -> do
      fmap toNCount . IM.fromList <$> D.queryNamed conn
        "SELECT target,weight FROM cache WHERE dim = :d AND source = :src"
        [ ":d" D.:= d, ":src" D.:= src ]
  where
    n = pascals !! d !! mx

loadNeighborWeights
    :: Int    -- ^ dimensions
    -> Int    -- ^ maximum
    -> IO (V.Vector (IntMap NCount))
loadNeighborWeights d mx = D.withConnection "cache/day17.db" $ \conn -> do
    D.execute_ conn
      "CREATE TABLE IF NOT EXISTS cache (dim INT, source INT, target INT, weight INT, CONSTRAINT dst UNIQUE(dim, source, target))"
    exists <- not . null @[] @(D.Only Int) <$> D.queryNamed conn
      "SELECT dim FROM cache WHERE dim = :d LIMIT 1" [":d" D.:= d]
    unless exists $ do
      putStrLn "Building cache..."
      cacheNeighborWeights conn d mx
      putStrLn "Cache completed."
    loadCache conn d mx
      -- <* putStrLn "cache loaded"
