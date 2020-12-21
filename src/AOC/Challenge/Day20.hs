{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day20 (
    day20a
  , day20b
  ) where

import           AOC.Prelude

-- import qualified Data.Vector                 as V
import           Data.Bitraversable
import           Data.Group
import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntMap.NonEmpty           as NEIM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.Map.NonEmpty              as NEM
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Set.NonEmpty              as NES
import qualified Data.Text                      as T
import qualified Data.Vector.Sized              as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

edges
    :: NESet Point
    -> NEMap IntSet (NESet Point) -- edge and map after edge
edges ps = NEM.fromList $ do
    o <- allD8
    let ps'   = shiftToZero . NES.map (orientPoint o) $ ps
        tbord = IS.fromList . mapMaybe (\(V2 x y) -> x <$ guard (y == 0)) $ toList ps'
    pure (tbord, ps')

-- | Get the x positions of the minimal ("top") y line
topBorder :: NESet Point -> IntSet
topBorder ps = IS.fromDistinctAscList
             . mapMaybe (\(V2 x y) -> (x - xmn) <$ guard (y == ymn))
             . toList
             $ ps
  where
    V2 xmn ymn = minCorner ps

-- | From a map of id's to edges of that id, return a map of id's to the
-- id's of all neighbors.
pairUp :: IntMap (Set IntSet) -> IntMap IntSet
pairUp im0 = flip IM.mapWithKey im0 $ \i es ->
        IM.keysSet
      . IM.filter (\ei -> not $ S.null $ es `S.intersection` ei)
      $ IM.delete i im0

-- | shift the corner point by a direction
topPointOf :: Dir -> Point
topPointOf = \case
    North -> V2 0 (-8)
    East  -> V2 8 0
    South -> V2 0  8
    West  -> V2 (-8) 0

-- | assume corner at 0,0
removeBorders :: NESet Point -> Set Point
removeBorders = S.fromDistinctAscList . mapMaybe go . toList
  where
    go p = do
      guard . and $ (/=) <$> p <*> V2 0 0
      guard . and $ (/=) <$> p <*> V2 9 9
      pure $ p - 1


toQueue
    :: Foldable f
    => Point            -- ^ corner
    -> NESet Point
    -> f Dir
    -> Map IntSet (Point, Dir)
toQueue p0 pts ds = M.fromList
    [ (topBorder pts', (p0 + topPointOf d, d))
    | d <- toList ds
    , let pts' = rotPoint d `NES.map` pts
    ]

assembleMap
    :: NEIntMap (NEMap IntSet (NESet Point))
    -> Set Point
assembleMap pts0 = case NEIM.deleteFindMin pts0 of
    ((_, s0), pts1) -> case NEM.deleteFindMin s0 of
      ((_, mp1), _) -> go (toQueue 0 mp1 allDir) pts1 (removeBorders mp1)
  where
    go  :: Map IntSet (Point, Dir)            -- queue: edge -> top corner, orientation
        -> IntMap (NEMap IntSet (NESet Point))  -- leftover
        -> Set Point                          -- current map
        -> Set Point
    go q pts mp = case M.minViewWithKey q of
      Nothing -> mp
      Just ((e, (mnpt, d)), q') ->
        let nxt = firstJust (traverse (NEM.lookup e)) (IM.toList pts)
        in  case nxt of
              Nothing        -> go q' pts mp
              Just (k, npts) ->
                let rotated  = shiftToZero $ orientPoint (D8 (invert d <> South) True)
                                   `NES.map` npts
                    shifted  = (+ mnpt) `S.map` removeBorders rotated
                    newQueue = toQueue mnpt rotated (NE.filter (/= invert d) allDir)
                in  go (newQueue <> q')
                       (IM.delete k pts)
                       (shifted <> mp)

parseTiles :: String -> Maybe (IntMap (NESet Point))
parseTiles = fmap IM.fromList
           . traverse (uncurry go <=< uncons . lines)
           . splitOn "\n\n"
  where
    go tname tiles =
      (,) <$> readMaybe (filter isDigit tname)
          <*> NES.nonEmptySet (parseAsciiSet (== '#') (unlines tiles))


day20a :: IntMap (NESet Point) :~> Int
day20a = MkSol
    { sParse = parseTiles
    , sShow  = show
    , sSolve = fmap product
             . V.fromList @4
             . mapMaybe (\(k, xs) -> k <$ guard (IS.size xs == 2)) . IM.toList
             . pairUp
             . fmap (NES.toSet . NEM.keysSet . edges)
    }

day20b :: IntMap (NESet Point) :~> Int
day20b = MkSol
    { sParse = parseTiles
    , sShow  = show
    , sSolve = \pp -> do
        mp <- assembleMap <$> NEIM.nonEmptyMap (edges <$> pp)
        listToMaybe
          [ S.size mp - numdrag * NES.size dragon
          | o   <- toList allD8
          , let dragon' = NES.map (orientPoint o) dragon
                numdrag = findPattern (NES.toSet dragon') mp
          , numdrag /= 0
          ]
    }

findPattern :: Set Point -> Set Point -> Int
findPattern pat ps = countTrue (S.null . (`S.difference` ps)) candidates
  where
    Just (V2 mn mx) = boundingBox' ps
    candidates =
      [ S.map (+ d) pat
      | d <- range (mn, mx)
      ]

dragon :: NESet Point
Just dragon = NES.nonEmptySet . parseAsciiSet (== '#') $ unlines
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "
  ]
