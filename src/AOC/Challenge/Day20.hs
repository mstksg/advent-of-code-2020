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

import           Data.Group
import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import qualified Data.Set.NonEmpty as NES

edges
    :: NESet Point
    -> Map IntSet (NESet Point) -- edge and map after edge
edges ps = M.fromList
    [ (tbord, ps')
    | r <- [ North ..  ]
    , flp <- [ False, True ]
    , let flpfunc = if flp then id else over _x negate
          ps' = shiftToZero . NES.map (flpfunc . mulPoint (dirPoint r)) $ ps
          tbord = IS.fromList . mapMaybe (\(V2 x y) -> x <$ guard (y == 0)) $ toList ps'
    ]

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

assembleMap
    :: IntMap (Map IntSet (NESet Point))
    -> Set Point
assembleMap pts0 = case IM.minViewWithKey pts0 of
    Nothing -> S.empty
    Just ((_, s0), pts1) -> case M.minViewWithKey s0 of
      Nothing -> error "hm"
      Just ((_, mp1), _) ->
        let q0 = M.fromList
                [ (topBorder mp1', (topPointOf dd, dd))
                | dd <- [North ..]
                , let mp1' = mulPoint (dirPoint (dd <> East)) `NES.map` mp1
                ]
        in  go q0 pts1 (removeBorders mp1)
  where
    go  :: Map IntSet (Point, Dir)          -- queue: edge -> top corner, orientation
        -> IntMap (Map IntSet (NESet Point))  -- leftover
        -> Set Point                        -- current map
        -> Set Point
    go q pts mp = case M.minViewWithKey q of
      Nothing -> mp
      Just ((e, (mnpt, d)), q') ->
        let nxt = firstJust (\(i, ls) ->
                    case M.lookup e ls of
                      Nothing   -> Nothing
                      Just npts -> Just (i, npts)
                ) $ IM.toList pts
        in  case nxt of
              Nothing -> go q' pts mp
              Just (k, npts) ->
                let rotated = shiftToZero $ mulPoint (dirPoint (invert $ horizDirFlip d <> East)) `NES.map` NES.map (over _x negate) npts
                    shifted = S.map (+ mnpt) (removeBorders rotated)
                    mp'     = shifted <> mp
                    newQueue = M.fromList
                        [ (topBorder npts', (topPointOf dd + mnpt, dd))
                        | dd <- [North ..]
                        , dd /= invert d
                        , let npts' = mulPoint (dirPoint (dd <> East)) `NES.map` rotated
                        ]
                in  go (newQueue <> q') (IM.delete k pts) mp'

day20a :: IntMap (NESet Point) :~> Int
day20a = MkSol
    { sParse = traverse NES.nonEmptySet
             . IM.fromList
             . mapMaybe (fmap (bimap (read . filter isDigit) (parseAsciiSet (== '#') . unlines)) . uncons . lines)
             . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just
             . product
             . mapMaybe (\(k, xs) -> k <$ guard (IS.size xs == 2))
             . IM.toList
             . pairUp
             . fmap (M.keysSet . edges)
    }

day20b :: IntMap (NESet Point) :~> Int
day20b = MkSol
    { sParse = sParse day20a
    , sShow  = show
    , sSolve = \pp ->
        let mp = assembleMap $ edges <$> pp
        in  listToMaybe
              [ S.size mp - numdrag * NES.size dragon
              | r <- [ North ..  ]
              , flp <- [ False, True ]
              , let flpfunc = if flp then id else over _x negate
                    dragon' = shiftToZero $ NES.map (flpfunc . mulPoint (dirPoint r)) dragon
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
