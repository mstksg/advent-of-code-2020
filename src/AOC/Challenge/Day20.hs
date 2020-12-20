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
  , horizFlip
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

-- rotSet :: Set Point -> Set Point
-- rotSet = mulPloint

-- mulPoint :: Num a => V2 a -> V2 a -> V2 a
-- mulPoint (V2 x y) (V2 u v) = V2 (x*u - y*v) (x*v + y*u)

edges :: Set Point -> Map (Dir, Bool) IntSet
edges ps = M.fromList 
    [ ((r, flp), topBorder $ S.map (flpfunc . mulPoint (dirPoint r)) ps)
    | r <- [ North ..  ]
    , flp <- [ False, True ]
    , let flpfunc = if flp then id else over _x negate
    ]
  where
    Just (V2 (V2 xmn ymn) (V2 xmx ymx)) = boundingBox' ps

edges2
    :: Set Point
    -> Map IntSet (Set Point) -- edge and map after edge
edges2 ps = M.fromList 
    [ (tbord, ps')
    -- ((r, flp), IS.fromList . topBorder $ S.map (flpfunc . mulPoint (dirPoint r)) ps)
    | r <- [ North ..  ]
    , flp <- [ False, True ]
    , let flpfunc = if flp then id else over _x negate
          ps' = shiftToZero . S.map (flpfunc . mulPoint (dirPoint r)) $ ps
          tbord = IS.fromList .mapMaybe (\(V2 x y) -> x <$ guard (y == 0)) $  S.toList ps'
    ]
  -- where
  --   Just (V2 (V2 xmn ymn) _) = boundingBox' ps

shiftToZero :: Set Point -> Set Point
shiftToZero ps = S.map (subtract mn) ps
  where
    Just (V2 mn _) = boundingBox' ps


topBorder :: Set Point -> IntSet
topBorder ps = IS.fromList . mapMaybe (\(V2 x y) -> (x - xmn) <$ guard (y == ymn)) . S.toList $ ps
  where
    Just (V2 (V2 xmn ymn) (V2 xmx ymx)) = boundingBox' ps

-- inOut :: Map (Dir, Bool) IntSet -> []

-- assemble :: [Inpointt]
--          -> Map IntSet ( Int, (Dir, Bool) )
--          -> IntMap Point
-- assemble (i:is) edges = go (IM.singletion i 0)
--   -- where
--   --   go seen newPoint

-- assemble :: IntMap (Map (Dir, Bool) IntSet)
--          -> Map IntSet ( Int, (Dir, Bool) )
--          -> IntMap Point
-- assemble (IM.deleteFindMin->((i0,e0),is)) edgs = go 

pairUp :: IntMap (Set IntSet) -> IntMap IntSet
pairUp im0 = flip IM.mapWithKey im0 $ \i es ->
    let im = IM.delete i im0
    in  IM.keysSet $ IM.filter (\ei -> not $ S.null $ es `S.intersection` ei) im
    -- countTrue (\e -> e `S.member` im) es
  where

-- pairUp :: IntMap (Set IntSet) -> IntMap IntSet
-- pairUp im0 = flip IM.mapWithKey im0 $ \i es ->
--     let im = IM.delete i im0
--     in  IM.filter (\ei -> not $ S.null $ es `S.intersection` ei) im
--     -- countTrue (\e -> e `S.member` im) es
--   where

-- pairUp2 :: IntMap (Map (Dir, Bool) IntSet) -> IntMap ((Dir, Bool), Map 
-- pairUp2 im0 = flip IM.mapWithKey im0 $ \i es ->
--     let im = fold $ IM.delete i im0
--     in  countTrue (\e -> e `S.member` im) es
--   where


 -- (IM.singleton i0 0) 0 e0
 --  where
 --    go seen currPoint currEdge = case firstJust

-- -- Returns @'V3' (V2 xMin yMin) (V2 xMax yMax)@.
-- boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> V2 (g a)
-- boundingBox = (\(T2 (Ap mn) (Ap mx)) -> V2 (getMin <$> mn) (getMax <$> mx))
--             . foldMap1 (\p -> T2 (Ap (Min <$> p)) (Ap (Max <$> p)))

-- -- | A version of 'boundingBox' that works for normal possibly-empty lists.
-- boundingBox' :: (Foldable f, Applicative g, Ord a) => f (g a) -> Maybe (V2 (g a))
-- boundingBox' = fmap boundingBox . NE.nonEmpty . toList

-- growRow
--     :: IntMap 

topPointOf :: Dir -> Point
topPointOf = \case
    North -> V2 0 (-9)
    East  -> V2 9 0
    South -> V2 0  9
    West  -> V2 (-9) 0
    -- North -> V2 0 (-11)
    -- East  -> V2 11 0
    -- South -> V2 0  11
    -- West  -> V2 (-11) 0

horizFlip :: Dir -> Dir
horizFlip = \case
    North -> North
    East  -> West
    West  -> East
    South -> South



assembleMap
    :: IntMap (Map IntSet (Set Point))
    -> Set Point
assembleMap pts0 = case IM.minViewWithKey pts0 of
    Nothing -> S.empty
    Just ((_, s0), pts1) -> case M.minViewWithKey s0 of
      Nothing -> error "hm"
      Just ((_, mp1), _) ->
        let q0 = M.fromList
                [ (topBorder mp1', (topPointOf dd, dd))
                | dd <- [North ..]
                , let mp1' = mulPoint (dirPoint (dd <> East)) `S.map` mp1
                ]
        -- in  trace "hi" $ traceShow q0 mp1
        in  go q0 pts1 mp1
-- go M.empty pts0 S.empty

  where
    -- go  :: IntMap (Map Dir IntSet)
    -- -- queue: edge -> top corner, orientation
    go  :: Map IntSet (Point, Dir)
        -> IntMap (Map IntSet (Set Point))  -- leftover
        -> Set Point    -- current map
        -> Set Point
    go q pts mp = case M.minViewWithKey q of
      Nothing -> mp
      Just ((e, (mnpt, d)), q') ->
        let nxtcand = mapMaybe (\(i, ls) ->
                    case M.lookup e ls of
                      Nothing   -> Nothing
                      Just npts -> Just (i, npts)
                ) $ IM.toList pts
            nxt = case nxtcand of
              [] -> Nothing
              [x] -> Just x
              x:_ -> error "more than one candidate"
        in  case nxt of
              Nothing -> go q' pts mp
              Just (k, npts) ->
              -- Just (trace "hi".traceShowId->k, npts) ->
                -- let rotated = shiftToZero npts
                let rotated = shiftToZero $ mulPoint (dirPoint (invert $ horizFlip d <> East)) `S.map` S.map (over _x negate) npts
                    shifted = S.map (+ mnpt) rotated
                    mp'     = shifted <> mp
                    newQueue = M.fromList
                        [ (topBorder npts', (topPointOf dd + mnpt, dd))
                        | dd <- [North ..]
                        , dd /= invert d
                        , let npts' = mulPoint (dirPoint (dd <> East)) `S.map` rotated
                        ]
                in  go (newQueue <> q') (IM.delete k pts) mp'

    -- -> Map IntSet (Set Point) -- edge and map after edge

day20a :: _ :~> _
day20a = MkSol
    { sParse = fmap IM.fromList
              . traverse (fmap  (bimap (read . filter isDigit) (parseAsciiSet (== '#') . unlines)) . uncons . lines)
             . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . product . IM.keys . IM.filter ((== 2) . IS.size)
                . pairUp . fmap (S.fromList . toList . edges)
    }

day20b :: _ :~> _
day20b = MkSol
    { sParse = sParse day20a
    , sShow  = ('\n':) . displayAsciiMap '.' . M.fromSet (\_ -> '#')
    , sSolve = Just . assembleMap . fmap edges2
    }
