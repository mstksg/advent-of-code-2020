{-# LANGUAGE CPP                                #-}
{-# LANGUAGE NoDeriveAnyClass                   #-}
{-# LANGUAGE QuantifiedConstraints              #-}
{-# LANGUAGE TypeFamilies                       #-}
{-# OPTIONS_GHC -Wno-orphans                    #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- |
-- Module      : AOC.Common
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Common functions for solutions
--

module AOC.Common (
    trace'
  -- * Loops and searches
  , iterateMaybe
  , loopMaybe
  , loopMaybeM
  , loopEither
  , firstJust
  , (!!!)
  , (!?)
  , drop'
  , dup
  , scanlT
  , scanrT
  , firstRepeated
  , firstRepeatedBy
  , fixedPoint
  , floodFill
  , floodFillCount
  , countTrue
  -- * Lists
  , freqs
  , lookupFreq
  , freqList
  , revFreq
  , perturbations
  , perturbationsBy
  , select
  , slidingWindows
  , sortedSlidingWindows
  , sortedSlidingWindowsInt
  , clearOut
  , foldMapPar
  , foldMapPar1
  , foldMapParChunk
  , meanVar
  , maximumVal
  , maximumValBy
  , minimumVal
  , minimumValBy
  , maximumValNE
  , maximumValByNE
  , minimumValNE
  , minimumValByNE
  , listTup
  , _ListTup
  , listTup3
  , _ListTup3
  , listTup4
  , _ListTup4
  , sortSizedBy
  , withAllSized
  -- * Simple type util
  , deleteFinite
  , Letter
  , charFinite
  , _CharFinite
  , hexDigit
  , decimalDigit
  , splitWord
  , digitToIntSafe
  , caeser
  , eitherItem
  , getDown
  , toNatural
  -- * Parsers
  , TokStream(..)
  , parseTokStream
  , parseTokStream_
  , parseTokStreamT
  , parseTokStreamT_
  , TokParser
  , parseWords
  , nextMatch
  , parseMaybeLenient
  , parseOrFail
  , CharParser
  , pWord
  , pHWord
  , pDecimal
  , parseLines
  -- * Graph
  , Graph
  , toFGL
  -- * Points
  , Point
  , cardinalNeighbs
  , cardinalNeighbsSet
  , fullNeighbs
  , fullNeighbsSet
  , mannDist
  , mulPoint
  , lineTo
  -- * Directions
  , Dir(..)
  , parseDir
  , dirPoint
  , dirPoint'
  , rotPoint
  , mulDir
  -- * 2D Maps
  , memoPoint
  , boundingBox
  , boundingBox'
  , inBoundingBox
  , parseAsciiMap
  , asciiGrid
  , parseAsciiSet
  , ScanPoint(..)
  , displayAsciiMap
  -- * Recursion Schemes
  , anaM
#if !MIN_VERSION_recursion_schemes(5,2,0)
  , TreeF(..), ForestF
#endif
  ) where

-- import qualified Data.Functor.Foldable.TH        as R
import           AOC.Util
import           Control.Applicative
import           Control.Comonad.Store
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import           Control.Parallel.Strategies
import           Data.Bifunctor
import           Data.Char
import           Data.Coerce
import           Data.Finite
import           Data.Finite.Internal
import           Data.Foldable
import           Data.Function
import           Data.Functor.Compose
import           Data.Group
import           Data.Hashable
import           Data.IntMap                        (IntMap)
import           Data.List
import           Data.List.NonEmpty                 (NonEmpty(..))
import           Data.List.Split
import           Data.Map                           (Map)
import           Data.Map.Lens
import           Data.Map.NonEmpty                  (NEMap)
import           Data.MemoCombinators               (Memo)
import           Data.Monoid                        (Ap(..))
import           Data.Ord
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Sequence                      (Seq(..))
import           Data.Set                           (Set)
import           Data.Set.Lens
import           Data.Set.NonEmpty                  (NESet)
import           Data.Tree                          (Tree(..))
import           Data.Tuple
import           Data.Tuple.Strict
import           Data.Void
import           Data.Word
import           Debug.Trace
import           GHC.Generics                       (Generic)
import           GHC.TypeNats
import           Linear                             (V2(..), V3(..), V4(..), R1(..), R2(..), R3(..), R4(..))
import           Linear.Vector
import           Numeric.Natural
import qualified Control.Foldl                      as F
import qualified Control.Monad.Combinators          as P
import qualified Data.Finitary                      as F
import qualified Data.Functor.Foldable              as R
import qualified Data.Graph.Inductive               as G
import qualified Data.IntMap                        as IM
import qualified Data.IntPSQ                        as IntPSQ
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Map                           as M
import qualified Data.Map.NonEmpty                  as NEM
import qualified Data.MemoCombinators               as Memo
import qualified Data.OrdPSQ                        as OrdPSQ
import qualified Data.Sequence                      as Seq
import qualified Data.Set                           as S
import qualified Data.Set.NonEmpty                  as NES
import qualified Data.Vector.Algorithms.Intro       as VAI
import qualified Data.Vector.Generic                as VG
import qualified Data.Vector.Generic.Sized          as SVG
import qualified Data.Vector.Generic.Sized.Internal as SVG
import qualified Text.Megaparsec                    as P
import qualified Text.Megaparsec.Char               as P
import qualified Text.Megaparsec.Char.Lexer         as PL

-- | trace but only after something has evaluated to WHNF
trace' :: String -> a -> a
trace' str x = trace (x `seq` str) x

-- | Strict (!!)
(!!!) :: [a] -> Int -> a
[] !!! _ = error "Out of range"
(x:_ ) !!! 0 = x
(x:xs) !!! n = x `seq` (xs !!! (n - 1))

-- | Strict drop
drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (x:xs) = x `seq` drop' (n - 1) xs

-- | Iterate until a 'Nothing' is produced
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = go
  where
    go !x = x : case f x of
      Nothing -> []
      Just y  -> go y

(!?) :: [a] -> Int -> Maybe a
[]     !? _ = Nothing
(x:_ ) !? 0 = Just x
(x:xs) !? n = x `seq` (xs !? (n - 1))

-- | Apply function until 'Nothing' is produced, and return last produced
-- value.
loopMaybe
    :: (a -> Maybe a)
    -> a
    -> a
loopMaybe f = go
  where
    go !x = case f x of
      Nothing -> x
      Just !y -> go y

-- | Apply function until a 'Left'.
loopEither
    :: (a -> Either r a)
    -> a
    -> r
loopEither f = go
  where
    go !x = case f x of
      Left  r  -> r
      Right !y -> go y


-- | Apply monadic function until 'Nothing' is produced, and return last produced
-- value.
loopMaybeM
    :: Monad m
    => (a -> m (Maybe a))
    -> a
    -> m a
loopMaybeM f = go
  where
    go !x = f x >>= \case
      Nothing -> pure x
      Just !y -> go y

-- | A tuple of the same item twice
dup :: a -> (a, a)
dup x = (x, x)

-- | 'scanl' generalized to all 'Traversable'.
scanlT :: Traversable t => (b -> a -> b) -> b -> t a -> t b
scanlT f z = snd . mapAccumL (\x -> dup . f x) z

-- | 'scanr' generalized to all 'Traversable'.
scanrT :: Traversable t => (a -> b -> b) -> b -> t a -> t b
scanrT f z = snd . mapAccumR (\x -> dup . flip f x) z

-- | Lazily find the first repeated item.
firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated = firstRepeatedBy id

-- | Lazily find the first repeated projection.
firstRepeatedBy :: Ord a => (b -> a) -> [b] -> Maybe b
firstRepeatedBy f = go S.empty
  where
    go seen (x:xs)
      | f x `S.member` seen = Just x
      | otherwise           = go (f x `S.insert` seen) xs
    go _ []     = Nothing


-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
        | x == y    = x
        | otherwise = go y
      where
        y = f x

-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

-- | each item paired with the list not including that item
select :: [a] -> [(a,[a])]
select = go []
  where
    go _  [] = []
    go xs (y:ys) = (y,xs++ys) : go (y:xs) ys

-- | Look up a count from a frequency map, defaulting to zero if item is
-- not foudn
lookupFreq :: Ord a => a -> Map a Int -> Int
lookupFreq = M.findWithDefault 0

-- | Build a reverse frequency map
revFreq :: (Foldable f, Ord a) => f a -> IntMap (NESet a)
revFreq = IM.fromListWith (<>)
        . map (swap . first NES.singleton)
        . M.toList
        . freqs

-- | Build a list of /descending/ frequencies.  Ties are sorted.
freqList :: (Foldable f, Ord a) => f a -> [(Int, a)]
freqList = concatMap (traverse toList) . IM.toDescList . revFreq

eitherItem :: Lens' (Either a a) a
eitherItem f (Left x) = Left <$> f x
eitherItem f (Right x) = Right <$> f x

getDown :: Down a -> a
getDown (Down x) = x

splitWord :: Word8 -> (Finite 16, Finite 16)
splitWord = swap . separateProduct . F.toFinite

decimalDigit :: Prism' Char (Finite 10)
decimalDigit = prism' _to _from
  where
    _to           = intToDigit . fromIntegral
    _from c
      | isDigit c = Just (Finite (fromIntegral (digitToInt c)))
      | otherwise = Nothing


hexDigit :: Prism' Char (Finite 16)
hexDigit = prism' _to _from
  where
    _to              = intToDigit . fromIntegral
    _from c
      | isHexDigit c = Just (Finite (fromIntegral (digitToInt c)))
      | otherwise    = Nothing

type Letter = Finite 26

-- | Parse a letter into a number 0 to 25.  Returns 'False' if lowercase
-- and 'True' if uppercase.
charFinite :: Char -> Maybe (Bool, Finite 26)
charFinite (ord->c) = asum
    [ (False,) <$> packFinite (fromIntegral (c - ord 'a'))
    , (True ,) <$> packFinite (fromIntegral (c - ord 'A'))
    ]

digitToIntSafe :: Char -> Maybe Int
digitToIntSafe c = digitToInt c <$ guard (isDigit c)

-- | Prism for a 'Char' as @('Bool', 'Finite' 26)@, where the 'Finite' is
-- the letter parsed as a number from 0 to 25, and the 'Bool' is lowercase
-- ('False') or uppercase ('True').
_CharFinite :: Prism' Char (Bool, Finite 26)
_CharFinite = prism' fromcf charFinite
  where
    fromcf (c, x) = chr $ fromIntegral x + ord b
      where
        b | c         = 'A'
          | otherwise = 'a'

-- | Caeser shift, preserving case.  If you have an 'Int' or 'Integer',
-- convert into 'Finite' using 'modulo'.
caeser :: Finite 26 -> Char -> Char
caeser i = over (_CharFinite . _2) (+ i)


-- | Collect all possible single-item perturbations from a given
-- perturbing function.
--
-- > perturbations (\i -> [i - 1, i + 1]) [0,10,100]
--      == [ [-1,10,100]
--         , [ 1,10,100]
--         , [ 0, 9,100]
--         , [ 0,11,100]
--         , [ 0,10, 99]
--         , [ 0,10,101]
--         ]
perturbations
    :: Traversable f
    => (a -> [a])
    -> f a
    -> [f a]
perturbations = perturbationsBy traverse

-- | Collect all possible single-item perturbations from a given
-- perturbing function.
--
-- > perturbations (\i -> [i - 1, i + 1]) [0,10,100]
--      == [ [-1,10,100]
--         , [ 1,10,100]
--         , [ 0, 9,100]
--         , [ 0,11,100]
--         , [ 0,10, 99]
--         , [ 0,10,101]
--         ]
perturbationsBy
    :: Conjoined p
    => Over p (Bazaar p a a) s t a a
    -> (a -> [a])
    -> s
    -> [t]
perturbationsBy p f = experiment f <=< holesOf p

-- | Clear out characters not matching a predicate
clearOut :: (Char -> Bool) -> String -> String
clearOut p = map $ \c -> if p c then ' '
                                else c

-- | sliding windows of a given length
slidingWindows :: Int -> [a] -> [Seq a]
slidingWindows n = uncurry go . first Seq.fromList . splitAt n
  where
    go ws@(_ :<| qs) = \case
      x:xs -> ws : go (qs :|> x) xs
      []   -> ws : []
    go _  = const []

-- | sorted windows of a given length
sortedSlidingWindows
    :: forall k v. Ord k
    => Int
    -> [(k,v)]
    -> [OrdPSQ.OrdPSQ k Int v]
sortedSlidingWindows n = uncurry go . first OrdPSQ.fromList . splitAt n . zipWith reIx [0..]
  where
    reIx i (j,k) = (j, i, k)
    go :: OrdPSQ.OrdPSQ k Int v -> [(k, Int, v)] -> [OrdPSQ.OrdPSQ k Int v]
    go ws = \case
      (k, i, x):xs -> ws : go (OrdPSQ.insert k i x (OrdPSQ.deleteMin ws)) xs
      _            -> ws : []

-- | sorted windows of a given length
sortedSlidingWindowsInt
    :: forall v. ()
    => Int
    -> [(Int,v)]
    -> [IntPSQ.IntPSQ Int v]
sortedSlidingWindowsInt n = uncurry go . first IntPSQ.fromList . splitAt n . zipWith reIx [0..]
  where
    reIx i (j,k) = (j, i, k)
    go :: IntPSQ.IntPSQ Int v -> [(Int, Int, v)] -> [IntPSQ.IntPSQ Int v]
    go ws = \case
      (k, i, x):xs -> ws : go (IntPSQ.insert k i x (IntPSQ.deleteMin ws)) xs
      _            -> ws : []

-- | Get the key-value pair corresponding to the maximum value in the map
maximumVal :: Ord b => Map a b -> Maybe (a, b)
maximumVal = maximumValBy compare

-- | Get the key-value pair corresponding to the maximum value in the map,
-- with a custom comparing function.
--
-- > 'maximumVal' == 'maximumValBy' 'compare'
maximumValBy :: (b -> b -> Ordering) -> Map a b -> Maybe (a, b)
maximumValBy c = fmap (maximumBy (c `on` snd))
               . NE.nonEmpty
               . M.toList

-- | Get the key-value pair corresponding to the minimum value in the map,
-- with a custom comparing function.
--
-- > 'minimumVal' == 'minimumValBy' 'compare'
minimumValBy :: (b -> b -> Ordering) -> Map a b -> Maybe (a, b)
minimumValBy c = fmap (minimumBy (c `on` snd))
               . NE.nonEmpty
               . M.toList

-- | Get the key-value pair corresponding to the minimum value in the map
minimumVal :: Ord b => Map a b -> Maybe (a, b)
minimumVal = minimumValBy compare

-- | Version of 'maximumValBy' for nonempty maps.
maximumValByNE :: (b -> b -> Ordering) -> NEMap a b -> (a, b)
maximumValByNE c = maximumBy (c `on` snd)
                 . NEM.toList

-- | Version of 'maximumVal' for nonempty maps.
maximumValNE :: Ord b => NEMap a b -> (a, b)
maximumValNE = maximumValByNE compare

-- | Version of 'minimumValBy' for nonempty maps.
minimumValByNE :: (b -> b -> Ordering) -> NEMap a b -> (a, b)
minimumValByNE c = minimumBy (c `on` snd)
                 . NEM.toList

-- | Version of 'minimumVal' for nonempty maps.
minimumValNE :: Ord b => NEMap a b -> (a, b)
minimumValNE = minimumValByNE compare

foldMapParChunk
    :: (NFData m, Monoid m)
    => Int      -- ^ chunk size
    -> (a -> m)
    -> [a]
    -> m
foldMapParChunk n f xs = fold $
  parMap rdeepseq (foldMap f) (chunksOf n xs)

listTup :: [a] -> Maybe (a,a)
listTup (x:y:_) = Just (x,y)
listTup _       = Nothing

_ListTup :: Prism' [a] (a, a)
_ListTup = prism' (\(x,y) -> [x,y]) $ \case
    [x,y] -> Just (x,y)
    _     -> Nothing

listTup3 :: [a] -> Maybe (a,a,a)
listTup3 (x:y:z:_) = Just (x,y,z)
listTup3 _         = Nothing

_ListTup3 :: Prism' [a] (a, a, a)
_ListTup3 = prism' (\(x,y,z) -> [x,y,z]) $ \case
    [x,y,z] -> Just (x,y,z)
    _       -> Nothing

listTup4 :: [a] -> Maybe (a,a,a,a)
listTup4 (x:y:z:k:_) = Just (x,y,z,k)
listTup4 _           = Nothing

_ListTup4 :: Prism' [a] (a, a, a, a)
_ListTup4 = prism' (\(x,y,z,k) -> [x,y,z,k]) $ \case
    [x,y,z,k] -> Just (x,y,z,k)
    _         -> Nothing

-- | Delete a potential value from a 'Finite'.
deleteFinite
    :: KnownNat n
    => Finite (n + 1)
    -> Finite (n + 1)
    -> Maybe (Finite n)
deleteFinite n m = case n `cmp` m of
    LT -> unshift m
    EQ -> Nothing
    GT -> strengthen m

-- | 'foldMap', but in parallel.
foldMapPar :: Monoid b => (a -> b) -> [a] -> b
foldMapPar f = runEval . fmap mconcat . traverse (rpar . f)

-- | 'foldMap1', but in parallel.
foldMapPar1 :: Semigroup b => (a -> b) -> NonEmpty a -> b
foldMapPar1 f = runEval . fmap sconcat . traverse (rpar . f)

-- | 'F.Fold' for computing mean and variance
meanVar :: Fractional a => F.Fold a (a, a)
meanVar = do
    n  <- fromIntegral <$> F.length
    x  <- F.sum
    x2 <- lmap (^ (2 :: Int)) F.sum
    pure $ let μ  = x / n
               σ2 = x2 / n - μ * μ
           in  (μ, σ2)

-- | Flood fill from a starting set
floodFill
    :: Ord a
    => (a -> Set a)     -- ^ Expansion (be sure to limit allowed points)
    -> Set a            -- ^ Start points
    -> Set a            -- ^ Flood filled
floodFill f = snd . floodFillCount f

-- | Flood fill from a starting set, counting the number of steps
floodFillCount
    :: Ord a
    => (a -> Set a)     -- ^ Expansion (be sure to limit allowed points)
    -> Set a            -- ^ Start points
    -> (Int, Set a)     -- ^ Flood filled, with count of number of steps
floodFillCount f = go 0 S.empty
  where
    go !n !innr !outr
        | S.null outr' = (n, innr')
        | otherwise    = go (n + 1) innr' outr'
      where
        innr' = S.union innr outr
        outr' = foldMap f outr `S.difference` innr'


type Graph v e = Map v (Map v e)

toFGL :: (G.Graph gr, Ord v) => Graph v e -> (gr v e, Set v)
toFGL gr = ( G.mkGraph (zip [0..] $ toList vertices)
                ((\(v,u,e) -> (ixOf v, ixOf u, e)) <$> edges)
           , vertices
           )
  where
    edges = do
      (v, es) <- M.toList gr
      (u, e ) <- M.toList es
      pure (v, u, e)
    vertices = foldMap (\(v,u,_) -> S.fromList [v,u]) edges
    ixOf     = (`S.findIndex` vertices)

-- data ExpGraph v e = ExpGraph (Map v )
-- data ExpGraph v e = ExpGraph e (Map v (ExpGraph v e))

-- data ExpGraph v e = ExpGraph (Map v (e, ExpGraph v e))
-- type ExpGraph v e = Map v (Map v (e, ExpGraph
-- data ExpGraph v e = ExpGraph v (Map v (e, ExpGraph v e))
                  -- { expGraphMap :: Map v [(v, e, ExpGraph v e)] }
-- -- newtype ExpGraph v e = ExpGraph { expGraphMap :: Map v [(v, e, ExpGraph v e)] }
--   deriving (Show, Eq, Ord, Functor)
-- R.makeBaseFunctor ''ExpGraph

-- -- Map v [(e, v)]

-- expandGraph :: forall v e. Ord v => Graph v e -> Map v (ExpGraph v e)
-- expandGraph gr = M.mapWithKey go gr
  -- where
  --   go :: v -> Map v e -> ExpGraph v e
  --   go
--   -- where
  --   go :: Map v e -> ExpandGrahF v e (Map v e)
  --   go vs = ExpandGraph

-- expandGraph :: forall v e. Ord v => Graph v e -> ExpGraph v e
-- expandGraph gr = go (M.keysSet gr)
--   where
--     go vs = ExpGraph $ M.fromSet (_ . flip M.lookup gr) vs


-- expandGraph gr = R.ana go (M.keysSet gr)
--   where
--     go :: Set v -> ExpGraphF v e (Set v)
--     go vs = ExpGraphF $
--       M.mapMaybe id $ M.fromSet (fmap () . flip M.lookup gr) vs
--       -- M.fromSet (_ . map swap . foldMap M.toList . flip M.lookup gr) vs
--     -- M.fromSet (_ . map swap . foldMap M.toList . flip M.lookup gr) vs

-- -- | Recursively fold up a monoid value for each vertex and all of its
-- -- children's monoid values.  You can transform the value in-transit before
-- -- it is accumulated if you want.
-- foldMapGraph
--     :: (Ord v, Monoid m)
--     => (v -> m)         -- ^ embed the vertex
--     -> (e -> m -> m)    -- ^ transform with edge before it is accumulated
--     -> Graph v e
--     -> Map v m
-- foldMapGraph f g gr = res
--   where
--     res = M.foldMapWithKey (\s v -> f s <> foldMap (g v) (M.lookup s res))
--        <$> gr

-- data ExpandGraph v e = ExpandGraph v e (ExpandGraph v e)

-- expandGraph :: Ord v => Graph v e -> Map v (v, [ExpandGraph v e])
-- expandGraph gr = M.mapWithKey
--   (\v es ->
--       ( v
--       , (\(u,e) -> ExpandGraph u e (go (gr M.! u)))
--         <$> M.toList es
--       )
--   )
--   gr

-- | 2D Coordinate
type Point = V2 Int

-- | Find the minimum and maximum x and y from a collection of points.
--
-- Returns @'V2' (V2 xMin yMin) (V2 xMax yMax)@.
boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> V2 (g a)
boundingBox = (\(T2 (Ap mn) (Ap mx)) -> V2 (getMin <$> mn) (getMax <$> mx))
            . foldMap1 (\p -> T2 (Ap (Min <$> p)) (Ap (Max <$> p)))

-- | A version of 'boundingBox' that works for normal possibly-empty lists.
boundingBox' :: (Foldable f, Applicative g, Ord a) => f (g a) -> Maybe (V2 (g a))
boundingBox' = fmap boundingBox . NE.nonEmpty . toList

inBoundingBox
    :: (Applicative g, Foldable g, Ord a)
    => V2 (g a)
    -> g a
    -> Bool
inBoundingBox (V2 mn mx) x = all id $ go <$> x <*> mn <*> mx
  where
    go x' mn' mx' = x' >= mn' && x' <= mx'


cardinalNeighbs :: Point -> [Point]
cardinalNeighbs p = (p +) <$> [ V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0 ]

cardinalNeighbsSet :: Point -> Set Point
cardinalNeighbsSet p = S.fromAscList . map (p +) $
  [ V2 (-1)   0
  , V2   0  (-1)
  , V2   0    1
  , V2   1    0
  ]

fullNeighbs
    :: (Applicative f, Num a, Traversable f)
    => f a
    -> [f a]
fullNeighbs p = tail
    [ liftA2 (+) p d
    | d <- sequence (pure [0,-1,1])
    ]
{-# INLINE fullNeighbs #-}

fullNeighbsSet
    :: (Applicative f, Num a, Ord (f a), Traversable f)
    => f a
    -> Set (f a)
fullNeighbsSet p = S.fromDistinctAscList $
    [ liftA2 (+) p d
    | d <- sequence (pure [-1,0,1])
    , d /= pure 0
    ]

memoPoint :: Memo Point
memoPoint = Memo.wrap (uncurry V2) (\(V2 x y) -> (x, y)) $
                Memo.pair Memo.integral Memo.integral

mannDist :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
mannDist x y = sum . abs $ x - y

-- | Treat as complex number multiplication. useful for rotations
mulPoint :: Num a => V2 a -> V2 a -> V2 a
mulPoint (V2 x y) (V2 u v) = V2 (x*u - y*v) (x*v + y*u)

data Dir = North | East | South | West
  deriving (Show, Eq, Ord, Generic, Enum)

instance Hashable Dir
instance NFData Dir

dirPoint :: Num a => Dir -> V2 a
dirPoint = \case
    North -> V2   0   1
    East  -> V2   1   0
    South -> V2   0 (-1)
    West  -> V2 (-1)  0

-- | 'dirPoint' but with inverted y axis
dirPoint' :: Num a => Dir -> V2 a
dirPoint' = \case
    North -> V2   0 (-1)
    East  -> V2   1   0
    South -> V2   0   1
    West  -> V2 (-1)  0

-- | Rotate a point by a direction
rotPoint :: Num a => Dir -> V2 a -> V2 a
rotPoint = \case
    North -> id
    East  -> \(V2 x y) -> V2   y  (-x)
    West  -> \(V2 x y) -> V2 (-y)   x
    South -> negate

parseDir :: Char -> Maybe Dir
parseDir = flip M.lookup dirMap . toUpper
  where
    dirMap = M.fromList [
        ('N', North) , ('E', East) , ('S', South) , ('W', West)
      , ('U', North) , ('R', East) , ('D', South) , ('L', West)
      ]

-- | Multiply headings, taking North as straight, East as clockwise turn,
-- West as counter-clockwise turn, and South as reverse.
--
-- Should be a commutative group; it's essentially complex number
-- multiplication like 'mulPoint', with North = 1, West = i.  The identity
-- is 'North' and the inverse is the opposite direction.
mulDir :: Dir -> Dir -> Dir
mulDir North = id
mulDir East  = \case North -> East
                     East  -> South
                     South -> West
                     West  -> North
mulDir South = \case North -> South
                     East  -> West
                     South -> North
                     West  -> East
mulDir West  = \case North -> West
                     East  -> North
                     South -> East
                     West  -> South

-- | '<>' is 'mulDir'.
instance Semigroup Dir where
    (<>) = mulDir
    stimes n x = case n `mod` 4 of
      1 -> x
      2 -> x <> x
      3 -> invert x
      _ -> North

instance Monoid Dir where
    mempty = North

instance Group Dir where
    invert = \case North -> South
                   East  -> West
                   South -> North
                   West  -> East
    pow = flip stimes

instance Abelian Dir


-- | It's 'Point', but with a newtype wrapper so we have an 'Ord' that
-- sorts by y first, then x
newtype ScanPoint = SP { _getSP :: Point }
  deriving (Eq, Show, Num, Generic)

instance Hashable ScanPoint
instance NFData ScanPoint

instance Ord ScanPoint where
    compare = comparing (view _y . _getSP)
           <> comparing (view _x . _getSP)

parseAsciiMap
    :: (Char -> Maybe a)
    -> String
    -> Map Point a
parseAsciiMap f = toMapOf (asciiGrid <. folding f)

parseAsciiSet
    :: (Char -> Bool)
    -> String
    -> Set Point
parseAsciiSet f = setOf (asciiGrid . filtered f . asIndex)

asciiGrid :: IndexedTraversal Point String [a] Char a
asciiGrid = conjoined traverse $ \pcfa ->
      sequenceA
    . concat
    . zipWith (\y -> zipWith (\x -> indexed pcfa (V2 x y :: Point)) [0..]) [0..]
    . lines

displayAsciiMap
    :: Char             -- ^ default tile
    -> Map Point Char   -- ^ tile map
    -> String
displayAsciiMap d (NEM.IsNonEmpty mp) = unlines
    [ [ NEM.findWithDefault d (V2 x y) mp
      | x <- [xMin .. xMax]
      ]
    | y <- [yMin .. yMax]
    ]
  where
    V2 xMin yMin `V2` V2 xMax yMax = boundingBox $ NEM.keysSet mp
displayAsciiMap _ _ = ""


sortSizedBy
    :: VG.Vector v a
    => (a -> a -> Ordering)
    -> SVG.Vector v n a
    -> SVG.Vector v n a
sortSizedBy f (SVG.Vector xs) = runST $ do
    ys <- VG.thaw xs
    VAI.sortBy f ys
    SVG.Vector <$> VG.unsafeFreeze ys
{-# INLINE sortSizedBy #-}

withAllSized
    :: VG.Vector v a
    => NonEmpty [a]
    -> (forall n. KnownNat n => NonEmpty (SVG.Vector v n a) -> Maybe r)
    -> Maybe r
withAllSized (x :| xs) f = SVG.withSizedList x $ \vx ->
    f . (vx :|) =<< traverse SVG.fromList xs
{-# INLINE withAllSized #-}

type instance Index   (SVG.Vector v n a) = Int
type instance IxValue (SVG.Vector v n a) = a

instance (Ixed (v a), Index (v a) ~ Int, IxValue (v a) ~ a) => Ixed (SVG.Vector v n a) where
    ix i f (SVG.Vector v) = SVG.Vector <$> ix i f v

instance (KnownNat n, forall a. VG.Vector v a, 1 <= n) => R1 (SVG.Vector v n) where
    _x = SVG.ix 0

instance (KnownNat n, forall a. VG.Vector v a, 2 <= n) => R2 (SVG.Vector v n) where
    _xy f v = (\(V2 x y) -> v SVG.// [(0, x), (1, y)]) <$> f (V2 (v `SVG.index` 0) (v `SVG.index` 1))
    _y = SVG.ix 1

instance (KnownNat n, forall a. VG.Vector v a, 3 <= n) => R3 (SVG.Vector v n) where
    _xyz f v = (\(V3 x y z) -> v SVG.// [(0, x), (1, y), (2, z)])
           <$> f (V3 (v `SVG.index` 0) (v `SVG.index` 1) (v `SVG.index` 2))
    _z = SVG.ix 2

instance (KnownNat n, forall a. VG.Vector v a, 4 <= n) => R4 (SVG.Vector v n) where
    _xyzw f v = (\(V4 x y z w) -> v SVG.// [(0, x), (1, y), (2, z), (3, w)])
           <$> f (V4 (v `SVG.index` 0) (v `SVG.index` 1) (v `SVG.index` 2) (v `SVG.index` 3))
    _w = SVG.ix 3

type instance Index   (OrdPSQ.OrdPSQ k p v) = k
type instance IxValue (OrdPSQ.OrdPSQ k p v) = v

instance (Ord k, Ord p) => Ixed (OrdPSQ.OrdPSQ k p v) where
    ix i f q = case OrdPSQ.lookup i q of
      Nothing    -> pure q
      Just (p,x) -> flip (OrdPSQ.insert i p) q <$> f x

-- | Use a stream of tokens @a@ as the underlying parser stream.  Note that
-- error messages for parser errors are going necessarily to be wonky.
newtype TokStream a = TokStream { getTokStream :: [a] }
  deriving (Ord, Eq, Show, Generic, Functor)

instance Hashable a => Hashable (TokStream a)
instance NFData a => NFData (TokStream a)



instance (Ord a, Show a) => P.Stream (TokStream a) where
    type Token  (TokStream a) = a
    type Tokens (TokStream a) = Seq a

    tokensToChunk _ = Seq.fromList
    chunkToTokens _ = toList
    chunkLength   _ = Seq.length
    take1_          = coerce . Data.List.uncons . getTokStream
    takeN_        n (TokStream xs) = bimap Seq.fromList TokStream (splitAt n xs)
                                  <$ guard (not (null xs))
    takeWhile_ p = bimap Seq.fromList TokStream . span p . getTokStream
    showTokens _ = show
    reachOffset o ps = ("<token stream>", ps')
      where
        step = o - P.pstateOffset ps
        ps' = ps { P.pstateOffset    = o
                 , P.pstateInput     = TokStream ys
                 , P.pstateSourcePos = (P.pstateSourcePos ps) {
                      P.sourceColumn = P.sourceColumn (P.pstateSourcePos ps)
                                    <> P.mkPos step
                    }
                 }
        ys = drop step (getTokStream (P.pstateInput ps))

-- | Parse a stream of tokens @s@ purely, returning 'Either'
parseTokStream
    :: Foldable t
    => P.Parsec e (TokStream s) a
    -> t s
    -> Either (P.ParseErrorBundle (TokStream s) e) a
parseTokStream p = runIdentity . parseTokStreamT p

-- | Parse a stream of tokens @s@ purely
parseTokStream_
    :: (Alternative m, Foldable t)
    => P.Parsec e (TokStream s) a
    -> t s
    -> m a
parseTokStream_ p = runIdentity . parseTokStreamT_ p

-- | Parse a stream of tokens @s@ over an underlying monad, returning 'Either'
parseTokStreamT
    :: (Foldable t, Monad m)
    => P.ParsecT e (TokStream s) m a
    -> t s
    -> m (Either (P.ParseErrorBundle (TokStream s) e) a)
parseTokStreamT p = P.runParserT p "" . TokStream . toList

-- | Parse a stream of tokens @s@ over an underlying monad
parseTokStreamT_
    :: (Alternative f, Foldable t, Monad m)
    => P.ParsecT e (TokStream s) m a
    -> t s
    -> m (f a)
parseTokStreamT_ p = fmap eitherToMaybe . parseTokStreamT p

type CharParser = P.Parsec Void String

pWord :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s String
pWord = P.many (P.satisfy (not . isSpace)) <* P.space

pHWord :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s String
pHWord = P.many (P.satisfy (not . isSpace)) <* P.many (P.satisfy (== ' '))

pDecimal :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s Int
pDecimal = PL.signed P.space PL.decimal

parseMaybeLenient :: P.Parsec Void s a -> s -> Maybe a
parseMaybeLenient p = eitherToMaybe . P.parse p "parseMaybeLenient"

parseOrFail :: (P.Stream s, P.ShowErrorComponent e) => P.Parsec e s a -> s -> a
parseOrFail p = either (error . P.errorBundlePretty) id . P.parse p "parseMaybeLenient"

parseLines :: P.Parsec Void String a -> String -> Maybe [a]
parseLines p = traverse (parseMaybeLenient p) . lines

parseWords :: P.Parsec Void (TokStream String) a -> String -> Maybe a
parseWords p = parseMaybeLenient p . TokStream . words

type TokParser s = P.Parsec Void (TokStream s)

-- | Skip every result until this token matches
nextMatch :: P.MonadParsec e s m => m a -> m a
nextMatch = P.try . fmap snd . P.manyTill_ (P.try P.anySingle)

toNatural :: Integral a => a -> Maybe Natural
toNatural x = fromIntegral x <$ guard (x >= 0)

-- | Lattice points for line between points, not including endpoints
lineTo :: Point -> Point -> [Point]
lineTo p0 p1 = [ p0 + t *^ step | t <- [1 .. gcf  - 1] ]
  where
    d@(V2 dx dy) = p1 - p0
    gcf          = gcd dx dy
    step         = (`div` gcf) <$> d

anaM
    :: (Monad m, R.Corecursive t, Traversable (R.Base t))
    => (a -> m (R.Base t a))
    -> a
    -> m t
anaM f = R.hylo (fmap R.embed . join . fmap sequenceA . getCompose) (Compose . f)

instance FunctorWithIndex k (NEMap k) where
    imap = NEM.mapWithKey
instance FoldableWithIndex k (NEMap k) where
    ifoldMap = NEM.foldMapWithKey
instance TraversableWithIndex k (NEMap k) where
    itraverse = NEM.traverseWithKey

#if !MIN_VERSION_recursion_schemes(5,2,0)
data TreeF a b = NodeF a (ForestF a b)
  deriving (Show, Functor, Generic)

instance (NFData a, NFData b) => NFData (TreeF a b)
type ForestF a b = [b]

type instance R.Base (Tree a) = TreeF a
instance R.Recursive (Tree a) where
    project (Node x xs) = NodeF x xs
instance R.Corecursive (Tree a) where
    embed (NodeF x xs) = Node x xs
#endif
