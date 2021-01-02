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
  , pascals
  , pascalRunIx
  , pascalIx
  , ixPascal
  , neighborWeights
  , neighborWeights_
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
import           Data.Bifunctor              (second)
import           Data.Coerce                 (coerce)
import           Data.Foldable               (toList, for_, asum)
import           Control.Monad.Trans.Class
import           Data.IntMap.Strict          (IntMap)
import           Data.IntSet                 (IntSet)
import           Data.List                   (scanl', sort)
import           Data.List.Split             (chunksOf)
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Semigroup              (Sum(..))
import           Data.Set                    (Set)
import           Data.Tuple.Strict           (T2(..), sfst, ssnd)
import           Debug.Trace
import           GHC.Generics                (Generic)
import           Linear                      (V2(..))
import           System.IO.Unsafe            (unsafePerformIO)
import           Text.Printf                 (printf)
import qualified Control.Foldl               as F
import qualified Data.IntMap.Monoidal.Strict as MIM
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.MemoCombinators        as Memo
import qualified Data.Set                    as S
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MV
import qualified Data.Vector.Unboxed         as VU
import qualified Database.SQLite.Simple      as D

pascals :: [[Int]]
pascals = repeat 1 : map (tail . scanl' (+) 0) pascals

pascalIx :: [Int] -> Int
pascalIx = sum . zipWith (\p x -> ((0:p) !! x)) (tail pascals)

pascalRunIx :: IntMap Int -> Int
pascalRunIx = pascalIx . concatMap (uncurry (flip replicate)) . IM.toList

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
      | n == 0    = ([n, n+1], n+1)
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
-- vecRunNeighbs_ xs0 =
vecRunNeighbs_ xs0 = tail $
      -- second (ssnd) <$> runStateT (VU.imapM go xs0) (T2 xs0 p0)
      second (toNCount . ssnd) <$> runStateT (VU.imapM go xs0) (T2 xs0 p0)
  where
    p0 = product . map factorial $ VU.toList xs0
    go :: Int -> Int -> StateT (T2 (VU.Vector Int) Int) [] Int
    go i _ = do
        T2 xs _ <- get
        let l  = fromMaybe 0 $ xs VU.!? (i-1)
            r  = xs VU.!? (i+1)
            x  = xs VU.! i
            x0 = xs0 VU.! i
        -- if i == 1, then we double the leftward contribution's
        -- probability since it could have come from a reflection
        if i == 1
          then do
            asum
              [ modify $ \(T2 v p) -> T2 v (p `div` factorial lc `div` factorial (l - lc))
              | lc <- [0 .. l]
              ]
          else modify (\(T2 v p) -> T2 v (p `div` factorial l))
        xContrib <- case r of
          Nothing -> x <$ modify (\(T2 v p) -> T2 v (p `div` factorial x))
          Just _  -> asum
            [ xc <$ modify (\(T2 v p) -> T2 (v VU.// [(i, x-xc)]) (p `div` factorial xc))
            | xc <- if 0 <= x0 && x0 <= x
                      then x0 : filter (/= x0) [0..x]
                      else [0..x]
            -- | xc <- [0..x]
            ]
        rContrib <- case r of
          Nothing -> pure 0
          Just r' -> asum
            [ rc <$ modify (\(T2 v p) -> T2 (v VU.// [(i+1, r'-rc)]) (p `div` factorial rc))
            | rc <- [0 .. r']
            ]
        pure (l + xContrib + rContrib)

-- lots of waste here too but maybe it's faster?
-- nice thing is we can go directly to pascal-land
-- same amount of waste as the map method, but we can directly encode
-- i guess
vecRunNeighbs
    :: VU.Vector Int
    -> [(Int, Int)]
vecRunNeighbs xs0 = tail $ go 0 0 xs0 p0 ((0:) <$> tail pascals)
  where
    p0 = product . map factorial $ VU.toList xs0
    n  = VU.length xs0
    go i !tot !xs !p cs
      | i < n = do
          (xContrib, xs', p') <- case r of
            Nothing -> pure (x, xs, p `div` factorial x)
            Just _  ->
              [ (xc, xs VU.// [(i, x-xc)], p `div` factorial xc)
              | xc <- if 0 <= x0 && x0 <= x
                        then x0 : filter (/= x0) [0 .. x]
                        else [0..x]
              ]
          (rContrib, xs'', p'') <- case r of
            Nothing -> pure (0, xs', p')
            Just r' ->
              [ (rc, xs' VU.// [(i+1, r'-rc)], p' `div` factorial rc)
              | rc <- [0 .. r']
              ]
          p''' <- if i == 1
            then
              [ p'' `div` factorial lc `div` factorial (l - lc)
              | lc <- [0 .. l]
              ]
            else pure (p'' `div` factorial l)
          let res = l + xContrib + rContrib
              (c,cs') = splitAt res cs
              tot'    = tot + sum (map head c)
          go (i + 1) tot' xs'' p''' (tail <$> cs')
      | otherwise = pure (tot, p)
      where
        l = fromMaybe 0 $ xs VU.!? (i-1)
        r = xs VU.!? (i+1)
        x = xs VU.! i
        x0 = xs0 VU.! i

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
      | otherwise = pure $ VU.fromListN (mx+1) (reverse (j:rs))

-- flip evalStateT d $ do
--     xs <- fmap (IM.fromDistinctAscList . catMaybes) . traverse (\o -> fmap (o,) <$> go) $ [0 .. mx-1]
--     lastCall <- get
--     let ys
--           | lastCall > 0 = IM.insert mx lastCall xs
--           | otherwise    = xs
--     pure ys
--   where
--     go :: StateT Int [] (Maybe Int)
--     go = StateT $ \i ->
--         (Nothing, i)
--       : [ (Just j, i - j)
--         | j <- [1..i]
--         ]

neighborPairs_
    :: Int    -- ^ dimension
    -> Int    -- ^ maximum
    -> [(Int, (Int, NCount))]
neighborPairs_ d mx =
    [ (pG, (pX, toNCount w))
    | x <- allVecRuns d mx
    , let pX = pascalIx (unRuns x)
    , (pG, w) <- vecRunNeighbs x
    -- , let w' | pG == pX  = w - 1
    --          | otherwise = w
    -- , w' > 0
    -- , w' <- if pG == pX then filter (> 0) [w - 1]
    --                     else [w]
    ]
  where
    unRuns = concatMap (uncurry (flip replicate)) . zip [0..] . VU.toList


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
      -- MV.modify v (IM.insertWith (flip (<>)) pX w') pG
      MV.unsafeModify v (IM.insertWith (flip (<>)) pX w') pG
    V.freeze v
  where
    n = pascals !! d !! mx

toNCount :: Int -> NCount
toNCount = \case
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
            -- wts = unsafePerformIO $ loadNeighborWeights d 6
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
day17b = day17 8

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
-- d=12: 424842240 / 1537981440 -- with unboxed, 22m10s, with pre-neighb: 8m30s (no cache: 7.4s)
--                                      smart cache: 21.5s total
-- d=13: 1932496896 / 7766482944 -- sqlite3 cache: 13.4s
--                                      smart cache: 1m10s total
-- d=14: 8778178560 / 39942504448 -- sqlite3 cache: 21.6s
-- d=15: 39814275072 / 209681145856 -- sqlite3 cache: 32.5s, (including loading: 1m20s); smart cache: 4h35m

cacheNeighborWeights
    :: D.Connection
    -> Int    -- ^ dimension
    -> Int    -- ^ maximum
    -> IO ()
cacheNeighborWeights conn d mx = do
    dbsem <- newQSem 1
    threadsem <- newQSem 5
    done <- newEmptyMVar
    forEnd_ (chunkUp <$> chunksOf 1_000_000 (neighborPairs d mx)) $ \isLast pmap -> do
      waitQSem threadsem
      forkIO $ do
        let chunky   = IM.size pmap
            bunky    = sum $ IM.size <$> pmap
            lastSeen = n - fromIntegral (maximum (fst . IM.findMax <$> pmap) + 1)
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
