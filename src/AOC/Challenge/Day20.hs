-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day20 (
    day20a
  , day20b
  ) where

import           AOC.Common                (mapMaybeSet, countTrue)
import           AOC.Common.Point          (Point, FinPoint, Dir(..), allDir, rotFin, orientFin, rotPoint, orientPoint, shiftToZero, D8(..), allD8, boundingBox', parseAsciiSet, displayAsciiSet)
import           AOC.Solver                ((:~>)(..))
import           Control.Lens hiding       (uncons)
import           Control.Monad             (guard, (<=<))
import           Data.Bit                  (Bit(..))
import           Data.Char                 (isDigit)
import           Data.Finitary             (toFinite, fromFinite)
import           Data.Finite               (weaken, strengthen, shift, unshift, packFinite)
import           Data.Foldable             (toList, find)
import           Data.Group                (invert)
import           Data.IntMap               (IntMap)
import           Data.IntMap.NonEmpty      (NEIntMap)
import           Data.IntSet               (IntSet)
import           Data.Ix                   (range)
import           Data.List                 (foldl', uncons)
import           Data.List.NonEmpty        (NonEmpty(..))
import           Data.List.Split           (splitOn)
import           Data.Map                  (Map)
import           Data.Map.NonEmpty         (NEMap)
import           Data.Maybe                (mapMaybe, listToMaybe)
import           Data.Set                  (Set)
import           Data.Set.NonEmpty         (NESet)
import           Linear                    (V2(..))
import           Text.Read                 (readMaybe)
import qualified Data.IntMap.NonEmpty      as NEIM
import qualified Data.IntMap.Strict        as IM
import qualified Data.IntSet               as IS
import qualified Data.Map                  as M
import qualified Data.Map.NonEmpty         as NEM
import qualified Data.Set                  as S
import qualified Data.Set.NonEmpty         as NES
import qualified Data.Vector.Sized         as V
import qualified Data.Vector.Unboxed.Sized as VU

type Edge = VU.Vector 10 Bit

data Tile = Tile
    { tPoints :: Set (FinPoint 8)
    , tEdges  :: !(V.Vector 8 Edge)
    }
  deriving (Show)

cutTile :: NESet (FinPoint 10) -> Tile
cutTile ps = Tile
    { tPoints = S.fromDistinctAscList . mapMaybe (traverse (strengthen <=< unshift)) . toList $ ps
    , tEdges  = V.generate $ \i ->
        let ps' = orientFin (invert (fromFinite i)) `NES.map` ps
        in  VU.generate $ \j -> Bit $ V2 j 0 `NES.member` ps'
    }

-- | Convert a set of points into all the orientations of tiles it could
-- be, indexed by the north edge of that orientation.
toTiles :: NESet (FinPoint 10) -> NEMap Edge Tile
toTiles ps = NEM.fromList $
    allD8 <&> \o ->
      let tile = cutTile $ orientFin o `NES.map` ps
      in  (tEdges tile `V.index` 0, tile)

orientTile :: D8 -> Tile -> Tile
orientTile o Tile{..} = Tile
    { tPoints = S.map (orientFin o) tPoints
    , tEdges  = V.generate $ \i ->
        tEdges `V.index` toFinite (invert o <> fromFinite i)
    }

countNeighbors :: IntMap (Set Edge) -> IntMap Int
countNeighbors im0 = flip IM.mapWithKey im0 $ \i es ->
              countTrue (\(k, ds) ->
                  k /= i && not (S.null (S.intersection es ds))
                ) im0List
  where
    im0List = IM.toList im0

-- | shift the corner point by a direction
topPointOf :: Dir -> Point
topPointOf = (`rotPoint` V2 0 (-8))

findKey
    :: (k -> Bool)
    -> NEMap k a
    -> Maybe (k, a)
findKey p = find (p . fst) . NEM.toList

-- | For a given image, add the given edges into the queue
toQueue
    :: Foldable f
    => Point            -- ^ location of corner
    -> Tile             -- ^ image to extract edges from
    -> f Dir            -- ^ edges to insert
    -> Map Edge (Point, Dir)
toQueue p0 tile ds = M.fromList
    [ ( tEdges tile `V.index` toFinite (D8 d False)
      , (p0 + topPointOf d, d)
      )
    | d <- toList ds
    ]

assembleMap
    :: NEIntMap (NEMap Edge Tile)
    -> Set Point
assembleMap tiles0 = go (toQueue 0 tile0 allDir)
                        (IM.keysSet tiles1)
                        (S.mapMonotonic (fmap fromIntegral) (tPoints tile0))
  where
    ((_, t0Map), tiles1) = NEIM.deleteFindMin tiles0
    ((_, tile0), _     ) = NEM.deleteFindMin  t0Map
    tileCache :: NEMap Edge (NEMap Int Tile)
    tileCache = NEM.fromListWith (<>)
      [ (edge, NEM.singleton tileId tile)
      | (tileId, tileEdges) <- NEIM.toList tiles0
      , (edge  , tile     ) <- NEM.toList  tileEdges
      ]
    go  :: Map Edge (Point, Dir)     -- ^ queue: edge -> top corner, orientation
        -> IntSet                    -- ^ leftover points
        -> Set Point                 -- ^ current map
        -> Set Point                 -- ^ sweet tail rescursion
    go !queue !tiles !mp = case M.minViewWithKey queue of
      Nothing                            -> mp
      Just ((edge, (corner, d)), queue') ->
        case findKey (`IS.member` tiles) (tileCache NEM.! edge) of
          Nothing             -> go queue' tiles mp
          Just (tileId, tile) ->
            let rotated  = orientTile (D8 (d <> South) True) tile
                toBlit   = ((+ corner) . fmap fromIntegral) `S.map` tPoints rotated
                newQueue = toQueue corner rotated allDir
            in  go (newQueue <> queue)
                   (IS.delete tileId tiles)
                   (toBlit <> mp)


day20a :: IntMap (NESet (FinPoint 10)) :~> Int
day20a = MkSol
    { sParse = parseTiles
    , sShow  = show
    , sSolve = fmap product
             . V.fromList @4
             . take 4
             . mapMaybe (\(k, i) -> k <$ guard (i == 2)) . IM.toList
             . countNeighbors
             . fmap (NES.toSet . NEM.keysSet . toTiles)
    }

day20b :: IntMap (NESet (FinPoint 10)) :~> _
day20b = MkSol
    { sParse = parseTiles
    , sShow  = show
    , sSolve = \pp -> do
        mp <- assembleMap <$> NEIM.nonEmptyMap (toTiles <$> pp)
        listToMaybe
          [ res
          | drgn <- toList dragons
          , let res = S.size $ pokePattern (NES.toSet drgn) mp
          , res /= S.size mp
          ]
    }

pokePattern
    :: Set Point
    -> Set Point
    -> Set Point
pokePattern pat ps0 = foldl' go ps0 (range (mn, mx))
  where
    Just (V2 mn mx) = boundingBox' ps0
    go ps d
        | pat' `S.isSubsetOf` ps = ps S.\\ pat'
        | otherwise              = ps
      where
        pat' = S.mapMonotonic (+ d) pat

dragons :: NonEmpty (NESet Point)
dragons = allD8 <&> \o -> shiftToZero $ NES.map (orientPoint o) dragon

dragon :: NESet Point
Just dragon = NES.nonEmptySet . parseAsciiSet (== '#') $ unlines
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "
  ]

parseTiles :: String -> Maybe (IntMap (NESet (FinPoint 10)))
parseTiles = fmap IM.fromList
           . traverse (uncurry go <=< uncons . lines)
           . splitOn "\n\n"
  where
    go tname tiles =
      (,) <$> readMaybe (filter isDigit tname)
          <*> NES.nonEmptySet (mapMaybeSet (traverse (packFinite . fromIntegral)) tileset)
      where
        tileset = parseAsciiSet (== '#') (unlines tiles)

displayTile :: (Foldable f, Integral a) => f (V2 a) -> String
displayTile = displayAsciiSet '.' '#' . S.fromList . map (fmap fromIntegral) . toList

_testTile :: NESet (FinPoint 10)
Just _testTile = NES.nonEmptySet . mapMaybeSet (traverse (packFinite . fromIntegral)) . parseAsciiSet (== '#') $ unlines
  [ "..##.#..#."
  , "##..#....."
  , "#...##..#."
  , "####.#...#"
  , "##.##.###."
  , "##...#.###"
  , ".#.#.#..##"
  , "..#....#.."
  , "###...#.#."
  , "..###..###"
  ]

_testOrient
    :: D8
    -> NESet (FinPoint 10)
    -> IO ()
_testOrient o ps = do
    putStrLn "Direct"
    putStrLn . displayTile $ NES.map (orientFin o) ps
    putStrLn "Direct + Cut"
    putStrLn . displayTile . _fromTile . cutTile $ NES.map (orientFin o) ps
    putStrLn "Cut + Orient"
    putStrLn . displayTile . _fromTile . orientTile o . cutTile $ ps

_fromTile :: Tile -> Set (FinPoint 10)
_fromTile Tile{..} = S.mapMonotonic (fmap (weaken . shift)) tPoints
                  <> foldMap (\d ->
                               S.fromList
                             . map (rotFin d)
                             . mapMaybe (\(j, Bit b) -> V2 j 0 <$ guard b)
                             . zip [0..]
                             . VU.toList
                             $ tEdges `V.index` toFinite (D8 d False)
                         )
                       allDir
