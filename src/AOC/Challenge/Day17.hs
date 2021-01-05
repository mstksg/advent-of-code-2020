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
  , runDay17
  , pascals
  , pascalIx
  , pascalVecRunIx
  , ixPascal
  , validNCount
  , neighborWeights
  , oldNeighborWeights
  , neighborPairs
  , loadNeighborWeights
  , vecRunNeighbs
  , vecRunNeighbs_
  , allVecRuns
  , finalWeight
  , encRun
  , NCount(..)
  ) where

import           AOC.Common                  (factorial, freqs, lookupFreq, foldMapParChunk, strictIterate)
import           AOC.Common.Point            (Point, parseAsciiSet)
import           AOC.Solver                  ((:~>)(..))
import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.MVar     (takeMVar, putMVar, newEmptyMVar)
import           Control.Concurrent.QSem     (waitQSem, signalQSem, newQSem)
import           Control.DeepSeq             (force, NFData)
import           Control.Exception           (bracket_, evaluate)
import           Control.Monad               (unless, void, when)
import           Control.Monad.ST            (runST)
import           Control.Monad.State         (StateT(..))
import           Data.Bifunctor              (second)
import           Data.Coerce                 (coerce)
import           Data.Foldable               (toList, for_)
import           Data.IntMap.Strict          (IntMap)
import           Data.IntSet                 (IntSet)
import           Data.List                   (scanl', sort)
import           Data.List.NonEmpty          (NonEmpty(..))
import           Data.List.Split             (chunksOf)
import           Data.Map                    (Map)
import           Data.Maybe                  (fromMaybe, mapMaybe)
import           Data.Set                    (Set)
import           Data.Tuple.Strict           (T3(..), T2(..))
-- import           Debug.Trace
import           GHC.Generics                (Generic)
import           Linear                      (V2(..))
import           Safe                        (lastMay)
import           System.IO.Unsafe            (unsafePerformIO)
import           Text.Printf                 (printf)
import qualified Data.IntMap.Monoidal.Strict as MIM
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MV
import qualified Data.Vector.Unboxed         as VU
import qualified Database.SQLite.Simple      as D

pascals :: [[Int]]
pascals = repeat 1 : map (tail . scanl' (+) 0) pascals

pascalIx :: [Int] -> Int
pascalIx = sum . zipWith (\p x -> ((0:p) !! x)) (tail pascals)

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

instance Semigroup NCount where
    NOne <> NOne = NTwo
    NOne <> NTwo = NThree
    NTwo <> NOne = NThree
    _    <> _    = NMany

data LiveCount = Dead !Ordering
               | LiveAlone
               | Live !Ordering
               | Overloaded
  deriving (Show, Eq, Ord, Generic)
instance NFData LiveCount

addOrdering :: a -> (Ordering -> a) -> Ordering -> Ordering -> a
addOrdering x f = go
  where
    go LT LT = f EQ
    go LT EQ = f GT
    go EQ LT = f GT
    go _  _  = x

toDead :: NCount -> LiveCount
toDead = \case
    NOne   -> Dead LT
    NTwo   -> Dead EQ
    NThree -> Dead GT
    NMany  -> Overloaded

instance Semigroup LiveCount where
    Dead n      <> Dead m      = addOrdering Overloaded Dead n m
    Dead n      <> LiveAlone   = Live n
    Dead n      <> Live  m     = addOrdering Overloaded Live n m
    LiveAlone   <> Dead m      = Live m
    LiveAlone   <> LiveAlone   = LiveAlone
    LiveAlone   <> Live m      = Live m
    Live n      <> Dead m      = addOrdering Overloaded Live n m
    Live n      <> LiveAlone   = Live n
    Live n      <> Live m      = addOrdering Overloaded Live n m
    _           <> _           = Overloaded

validLiveCount :: LiveCount -> Bool
validLiveCount = \case
    Dead GT -> True
    Live EQ -> True
    Live GT -> True
    _       -> False

validNCount :: NCount -> Maybe Bool
validNCount = \case
    NTwo   -> Just False
    NThree -> Just True
    _      -> Nothing

stepper
    :: Int      -- ^ how big the xy plane is
    -> V.Vector (IntMap LiveCount)        -- ^ symmetry map
    -> IntMap IntSet    -- ^ alive set: map of <x.y> to all zw+ points (pascal coords)
    -> IntMap IntSet
stepper nxy syms cs = fmap (IM.keysSet . IM.filter validLiveCount) $
      coerce (foldMapParChunk @(MIM.MonoidalIntMap (MIM.MonoidalIntMap LiveCount)) nxy id)
      [ IM.fromList $ zip (neighbs2d nxy gIx) (updateHere : repeat updateThere)
      | (gIx, ds) <- IM.toList cs
      , let T2 updateHere updateThere = prebaked M.! ds
      ]
  where
    -- the number of unique groups stays constant as you increase d
    uniqueGroups = S.fromList $ IM.elems cs
    prebaked :: Map IntSet (T2 (IntMap LiveCount) (IntMap LiveCount))
    prebaked = flip M.fromSet uniqueGroups $ \ds ->
      coerce $ foldMap id
        [ T2 (MIM.MonoidalIntMap $ IM.insertWith (<>) pIx LiveAlone pNeighbs)
             (MIM.MonoidalIntMap $ IM.insertWith (<>) pIx (Dead LT) pNeighbs)
        | pIx <- IS.toList ds
        , let pNeighbs = syms V.! pIx
        ]

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
    v <- MV.replicate n' IM.empty
    for_ [0 .. n-1] $ \x ->
      for_ (neighbs (mx-1) (ixPascal d x)) $ \i -> do
        let pIx = pascalIx (sort i)
        when (pIx < n') $
          MV.modify v (IM.insertWith (flip (<>)) x NOne) pIx
    V.freeze v
  where
    n  = pascals !! d !! mx
    n' = pascals !! d !! (mx-1)

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


data Chomper a = C { _cLeft2 :: !Bool        -- false if no exist, true if exist
                   , _cLeft  :: !(Maybe a)
                   , _cHere  :: !a
                   , _cOrig  :: !a
                   , _cRight :: !(NonEmpty a)
                   }

toChomper :: [a] -> Maybe (Chomper a)
toChomper (x:y:zs) = Just (C False Nothing x x (y:|zs))
toChomper _        = Nothing

-- reference implementaiton returning the actual runs. but
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
    go i _
      | i == n - 1 = pure 0
      | otherwise  = StateT $ \(T3 xs b p) -> do
        let l  = fromMaybe 0 $ xs VU.!? (i-1)
            r  = xs VU.! (i+1)
            x  = xs VU.! i
            x0 = xs0 VU.! i
        (rContrib, xs', b', xContrib, p') <-
          if i == (n-2)
            then pure
                  ( r, xs, b && x == x0 && r == 0, x
                  , p `div` factorial x `div` factorial r
                  )
            else
              [ ( rContrib
                , xs VU.// [(i, x-xContrib),(i+1, r-rContrib)]
                , b && xContrib == x0
                , xContrib
                , p `div` factorial xContrib `div` factorial rContrib
                )
              | totContrib <- [0..(x+r)]
              , xContrib <- [max 0 (totContrib-r)..min x totContrib]
              , let rContrib = totContrib - xContrib
              ]
        let p''
              | i == 1    = p' * (2^l)
              | otherwise = p' `div` factorial l
            res = l + rContrib + xContrib
        pure (res, T3 xs' b' p'')
    n = VU.length xs0

-- | directly return pascal coords
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
        -- we can ignore all of these since we don't ever check neighbors
        -- of t>=mx.  so rContrib must be completely r for the last item,
        -- to completely exhaust that point so that a 0 pick is forced.
        r:|[] -> pure
          let p' = p `div` factorial x `div` factorial r
              p''
                | ll' && not ll = p' * (2^l')
                | otherwise     = p' `div` factorial l'
              res  = l' + x + r
              c    = take res cs
              tot' = tot + sum (map head c)
          in  (allSame && x == x0 && r == 0, (tot', toNCount p''))
        r:|(r':rs') -> do
          (xContrib, rContrib, p') <-
              [ ( xContrib
                , rContrib
                , p `div` factorial xContrib `div` factorial rContrib
                )
              | totContrib <- [0..(x+r)]
              , xContrib  <- [max 0 (totContrib-r)..min x totContrib]
              , let rContrib = totContrib - xContrib
              ]
          let p''
                | ll' && not ll = p' * (2^l')
                | otherwise     = p' `div` factorial l'
              res = l' + xContrib + rContrib
              (c,cs') = splitAt res cs
              tot'    = tot + sum (map head c)
              chomp   = C ll' (Just (x - xContrib)) (r - rContrib) r (r':|rs')
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

neighborPairs
    :: Int    -- ^ dimension
    -> Int    -- ^ maximum
    -> [(Int, (Int, NCount))]
neighborPairs d mx =
    [ (pG, (pX, w))
    | x <- allVecRuns d mx
    , let pX = pascalVecRunIx x
    , (pG, w) <- vecRunNeighbs x
    ]

neighborWeights
    :: Int            -- ^ dimension
    -> Int            -- ^ maximum
    -> V.Vector (IntMap NCount)
neighborWeights d mx = runST $ do
    v <- MV.replicate n' IM.empty
    for_ (neighborPairs d mx) $ \(pG, (pX, w')) ->
      MV.unsafeModify v (IM.insertWith (flip (<>)) pX w') pG
    V.freeze v
  where
    n' = pascals !! d !! (mx-1)

toNCount :: Int -> NCount
toNCount = \case
    0 -> error "0 ncount"
    1 -> NOne
    2 -> NTwo
    3 -> NThree
    _ -> NMany

runDay17
    :: Bool               -- ^ cache thunk between runs
    -> Bool               -- ^ use sqlite3
    -> Int                -- ^ number of steps
    -> Int                -- ^ dimensions
    -> Set Point          -- ^ points
    -> [IntMap IntSet]    -- ^ steps
runDay17 cache sql3 mx d (S.toList -> x) =
          take (mx + 1)
        . strictIterate (force . stepper nxy (fmap toDead <$> wts))
        $ shifted
  where
    bounds  = maximum (concatMap toList x) + 1
    nxy     = bounds + mx*2
    shifted = IM.fromList $
        (\(V2 i j) -> (i + j * nxy, IS.singleton 0)) . (+ V2 mx mx) <$> x
    mx'
      | cache     = mx
      | otherwise = mx + length x - length x
    {-# INLINE mx' #-}
    wts
      | sql3      = loadNeighborWeights d mx'
      | otherwise = neighborWeights d mx'
{-# INLINE runDay17 #-}

day17
    :: Int
    -> Set Point :~> Int
day17 d = MkSol
    { sParse = Just . parseAsciiSet (== '#')
    , sShow  = show
    , sSolve = fmap (sum . map (sum . map (finalWeight d . ixPascal d) . IS.toList) . toList)
             . lastMay
             . runDay17 False False 6 d
    }
{-# INLINE day17 #-}

-- heatMap :: Int -> IntMap IntSet -> [[Int]]
-- heatMap nxy pts =
--     [ [ maybe 0 IS.size (IM.lookup (x + y * nxy) pts)
--       | y <- [0..nxy-1]
--       ]
--     | x <- [0..nxy-1]
--     ]

-- heatMap2 :: Int -> Int -> IntMap IntSet -> [[Int]]
-- heatMap2 d nxy pts =
--     [ [ maybe 0 (sum . map (finalWeight d . ixPascal d) . IS.toList) (IM.lookup (x + y * nxy) pts)
--       | y <- [0..nxy-1]
--       ]
--     | x <- [0..nxy-1]
--     ]

-- heatPoints :: Int -> Int -> IntMap IntSet -> Map [[Int]] [(Int, Int)]
-- heatPoints d nxy pts = fmap S.toList $ M.fromListWith (<>)
--     [ (p, S.singleton (x, y))
--     | x <- [0..nxy-1]
--     , y <- [0..nxy-1]
--     , let p = map (ixPascal d) . IS.toList $ IM.findWithDefault IS.empty (x + y*nxy) pts
--     , not (null p)
--     ]

encRun :: Int -> [Int] -> [Int]
encRun mx xs = map (\i -> M.findWithDefault 0 i occ) [0..mx]
  where
    occ = freqs xs

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
--                                      no-t=6 cache: 3.3s total
--                                      smarter t=6 cache: 3.0s total
--                                      unflatted step grid: 2.1s total
--                                      pure grid: 1.2s total
--                                      unique z-stacks: 120ms step
-- d=11: 93113856 / 309176832; 43m54s  -- with unboxed, 5m3s, with pre-neighb: 1m43s (no cache: 4.5s)
--                                      smallcache: 52s
--                                      8.8s v 7.7s
--                                      smarter t=6 cache: 5.8s
--                                      unique z-stacks: 172ms step
-- d=12: 424842240 / 1537981440 -- with unboxed, 22m10s, with pre-neighb: 8m30s (no cache: 7.4s)
--                                      smart cache: 21.5s total
--                                      21s vs 17s
--                                      no t=6 cache: 14s
--                                      unique z-stacks: 281ms step
-- d=13: 1932496896 / 7766482944 -- sqlite3 cache: 13.4s
--                                      smart cache: 1m10s total
--                                      new: 43s
--                                      unique z-stacks: 421ms step
-- d=14: 8778178560 / 39942504448 -- sqlite3 cache: 21.6s
--                                      new: 2m21s total
--                                      unique z-stacks: 647ms step
-- d=15: 39814275072 / 209681145856 -- sqlite3 cache: 32.5s, (including loading: 1m20s); smart cache: 4h35m
--    new method: total cache + run = 20m53s
--                                      unique z-stacks: 1.00s step
-- d=16: ? / 1125317394432 -- build sqlite cache + run = 62m44; run = 2m25s
--                                      unique z-stacks: 1.37s step
-- d=17: ? / 6178939535360 -- build sqlite cache + run = 24m
--                                      unique z-stacks: 2.08s step
-- d=18: ? / 34702568194048 -- build sqlite cache + run = 75m
--                                      unique z-stacks: 3.19s step

cacheNeighborWeights
    :: D.Connection
    -> Int    -- ^ dimension
    -> Int    -- ^ maximum
    -> IO ()
cacheNeighborWeights conn d mx = do
    dbsem <- newQSem 1
    threadsem <- newQSem 5
    done <- newEmptyMVar
    forEnd_ (chunkUp <$> chunksOf 5_000_000 (neighborPairs d mx)) $ \isLast pmap -> do
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
    V.generateM n' $ \src -> do
      fmap toNCount . IM.fromList <$> D.queryNamed conn
        "SELECT target,weight FROM cache WHERE dim = :d AND source = :src"
        [ ":d" D.:= d, ":src" D.:= src ]
  where
    n' = pascals !! d !! (mx-1)

loadNeighborWeights
    :: Int    -- ^ dimensions
    -> Int    -- ^ maximum
    -> V.Vector (IntMap NCount)
loadNeighborWeights d mx = unsafePerformIO $ loadNeighborWeights_ d mx

loadNeighborWeights_
    :: Int    -- ^ dimensions
    -> Int    -- ^ maximum
    -> IO (V.Vector (IntMap NCount))
loadNeighborWeights_ d mx = D.withConnection "cache/day17.db" $ \conn -> do
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
