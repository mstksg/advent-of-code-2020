{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE TypeFamilies     #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

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
  -- * Loops and searches
    iterateMaybe
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
  , select
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
  , parseLines
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
  , mulDir
  -- * 2D Maps
  , memoPoint
  , boundingBox
  , boundingBox'
  , parseAsciiMap
  , parseAsciiSet
  , ScanPoint(..)
  , displayAsciiMap
  ) where

import           AOC.Util
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Bifunctor
import           Data.Char
import           Data.Coerce
import           Data.Finite
import           Data.Finite.Internal
import           Data.Foldable
import           Data.Function
import           Data.Group
import           Data.Hashable
import           Data.IntMap                        (IntMap)
import           Data.List
import           Data.List.NonEmpty                 (NonEmpty)
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
import           Data.Tuple
import           Data.Void
import           Data.Word
import           GHC.Generics                       (Generic)
import           GHC.TypeNats
import           Linear                             (V2(..), _x, _y)
import           Linear.Vector
import           Numeric.Natural
import qualified Control.Foldl                      as F
import qualified Control.Monad.Combinators          as P
import qualified Data.Finitary                      as F
import qualified Data.IntMap                        as IM
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Map                           as M
import qualified Data.Map.NonEmpty                  as NEM
import qualified Data.MemoCombinators               as Memo
import qualified Data.OrdPSQ                        as OrdPSQ
import qualified Data.Sequence                      as Seq
import qualified Data.Set                           as S
import qualified Data.Set.NonEmpty                  as NES
import qualified Data.Vector.Generic.Sized.Internal as SVG
import qualified Text.Megaparsec                    as P

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
iterateMaybe f x0 = x0 : unfoldr (fmap dup . f) x0

(!?) :: [a] -> Int -> Maybe a
[]     !? _ = Nothing
(x:_ ) !? 0 = Just x
(_:xs) !? n = xs !? (n - 1)

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
firstRepeated = go S.empty
  where
    go seen (x:xs)
      | x `S.member` seen = Just x
      | otherwise         = go (x `S.insert` seen) xs
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
    :: (a -> [a])
    -> [a]
    -> [[a]]
perturbations f xs = do
    i <- [0 .. length xs - 1]
    xs & ix i %%~ f

-- | Clear out characters not matching a predicate
clearOut :: (Char -> Bool) -> String -> String
clearOut p = map $ \c -> if p c then ' '
                                else c

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



-- | 2D Coordinate
type Point = V2 Int

-- | Find the minimum and maximum x and y from a collection of points.
--
-- Returns @'V2' (V2 xMin yMin) (V2 xMax yMax)@.
boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> V2 (g a)
boundingBox = (\(Ap mn, Ap mx) -> V2 (getMin <$> mn) (getMax <$> mx))
            . foldMap1 (\p -> (Ap (Min <$> p), Ap (Max <$> p)))

-- | A version of 'boundingBox' that works for normal possibly-empty lists.
boundingBox' :: Foldable f => f Point -> Maybe (V2 Point)
boundingBox' = fmap boundingBox . NE.nonEmpty . toList

cardinalNeighbs :: Point -> [Point]
cardinalNeighbs p = (p +) <$> [ V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0 ]

cardinalNeighbsSet :: Point -> Set Point
cardinalNeighbsSet p = S.fromAscList . map (p +) $
  [ V2 (-1)   0
  , V2   0  (-1)
  , V2   0    1
  , V2   1    0
  ]

fullNeighbs :: Point -> [Point]
fullNeighbs p = [ p + V2 dx dy
                | dx <- [-1 .. 1]
                , dy <- if dx == 0 then [-1,1] else [-1..1]
                ]

fullNeighbsSet :: Point -> Set Point
fullNeighbsSet = S.fromList . fullNeighbs

memoPoint :: Memo Point
memoPoint = Memo.wrap (uncurry V2) (\(V2 x y) -> (x, y)) $
                Memo.pair Memo.integral Memo.integral

mannDist :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
mannDist x y = sum . abs $ x - y

-- | Treat as complex number multiplication. useful for rotations
mulPoint :: Point -> Point -> Point
mulPoint (V2 x y) (V2 u v) = V2 (x*u - y*v) (x*v + y*u)

data Dir = North | East | South | West
  deriving (Show, Eq, Ord, Generic, Enum)

instance Hashable Dir
instance NFData Dir

dirPoint :: Dir -> Point
dirPoint = \case
    North -> V2   0   1
    East  -> V2   1   0
    South -> V2   0 (-1)
    West  -> V2 (-1)  0

-- | 'dirPoint' but with inverted y axis
dirPoint' :: Dir -> Point
dirPoint' = \case
    North -> V2   0 (-1)
    East  -> V2   1   0
    South -> V2   0   1
    West  -> V2 (-1)  0

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

instance Monoid Dir where
    mempty = North

instance Group Dir where
    invert = \case North -> South
                   East  -> West
                   South -> North
                   West  -> East

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




type instance Index   (SVG.Vector v n a) = Int
type instance IxValue (SVG.Vector v n a) = a

instance (Ixed (v a), Index (v a) ~ Int, IxValue (v a) ~ a) => Ixed (SVG.Vector v n a) where
    ix i f (SVG.Vector v) = SVG.Vector <$> ix i f v

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

parseMaybeLenient :: P.Parsec e s a -> s -> Maybe a
parseMaybeLenient p = eitherToMaybe . P.parse p "parseMaybeLenient"

parseOrFail :: (P.Stream s, P.ShowErrorComponent e) => P.Parsec e s a -> s -> a
parseOrFail p = either (error . P.errorBundlePretty) id . P.parse p "parseMaybeLenient"

parseLines :: P.Parsec e String a -> String -> Maybe [a]
parseLines p = traverse (parseMaybeLenient p) . lines

parseWords :: P.Parsec e (TokStream String) a -> String -> Maybe a
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

instance FunctorWithIndex k (NEMap k) where
    imap = NEM.mapWithKey
instance FoldableWithIndex k (NEMap k) where
    ifoldMap = NEM.foldMapWithKey
instance TraversableWithIndex k (NEMap k) where
    itraverse = NEM.traverseWithKey
