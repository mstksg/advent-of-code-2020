Ah, the infamous Day 20 :)  I actually went through a few different possible
solutions for this before settling on the one I have now.  It also pushed me to
flesh out my "direction manipulation" mini-library (that I used [Day
12](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day12.md))
to be a full "orientation manipulation" mini-library.  With it, I get to
enumerate, manipulate, and combine the eight possible orientations of a 2d
square grid in a nice way.

```haskell
data Dir = North | East | South | West

-- | Rotate a point by a direction
rotPoint :: Num a => Dir -> V2 a -> V2 a

allDir :: NonEmpty Dir
allDir = North :| [ East .. ]

-- All of these instances are described in my day 12 writeup
instance Semigroup Dir where
instance Monoid Dir where
instance Group Dir where
instance Abelian Dir

-- | A possible orientation (flip and rotate) of a 2d square grid
data D8 = D8 { d8Rot :: Dir, d8Flip :: Bool }

instance Semigroup D8 where
    D8 x1 False <> D8 x2 y2 = D8 (x1 <> x2) y2
    D8 x1 True  <> D8 x2 y2 = D8 (x1 <> invert x2) (not y2)

instance Monoid D8 where
    mempty = D8 North False

instance Group D8 where
    invert (D8 x False) = D8 (invert x) False
    invert (D8 x True ) = D8 x          True

allD8 :: NonEmpty D8
allD8 = D8 <$> allDir <*> (False :| [ True ])

-- | Rotate and flip a point by a 'D8'
orientPoint :: Num a => D8 -> V2 a -> V2 a
orientPoint = \case
    D8 North False -> id
    D8 East  False -> \(V2 x y) -> V2   y  (-x)
    D8 West  False -> \(V2 x y) -> V2 (-y)   x
    D8 South False -> \(V2 x y) -> V2 (-x) (-y)
    D8 North True  -> \(V2 x y) -> V2 (-x)   y
    D8 East  True  -> \(V2 x y) -> V2   y    x
    D8 West  True  -> \(V2 x y) -> V2 (-y) (-x)
    D8 South True  -> \(V2 x y) -> V2   x  (-y)
```

Having orientations as a data type I can manipulate as first-class values
helped me "think" my way through everything a little easier.

First things first, we can break apart a 10x10 tile into the parts that
actually matter: its eight edges (which we can represent as a set of `Finite
10`s) and its core (which we can represent as a set of `V2 (Finite 8)`, 8x8
points).  I'm using `Finite` from
*[finite-typelits](https://hackage.haskell.org/package/finite-typelits)* mostly
as a way for me to keep track of what I have at each stage --- remember that
`Finite 8`, for instance, is one of 0,1,2,3,4,5,6, or 7.  This is also handy
because the library gives us `strengthen <=< unshift :: Finite 10 -> Maybe
(Finite 8)`, that lets us "chop off" the outer edges of a `Set (Finite 10)` to
get the `Set (Finite 8)` core.

```haskell
type Edge = Set (Finite 10)
type Core = Set (V2 (Finite 8))

-- | Shift corner to (0,0)
shiftToZero :: (Applicative f, Num a, Ord a) => Set (V2 a) -> Set (V2 a)

-- | mapMaybe but for sets
mapMaybeSet :: Ord b => (a -> Maybe b) -> Set a -> Set b

toTiles
    :: Set (V2 (Finite 10))
    -> ((Core, D8 -> Edge), Map Edge D8)
toTiles ps = ((core, getEdge), M.toList (map swap oToEdge))
  where
    core      = mapMaybeSet (traverse (strengthen <=< unshift)) ps
    getEdge o = oMap M.! o
    oMap      = M.fromList oToEdge
    oToEdge   =
        [ (o, mapMaybeSet (\(V2 x y) -> x <$ guard (y == 0)) ps')
        | o <- toList allD8
        , let ps' = shiftToZero $ orientPoint (invert o) `S.map` ps
        ]
```

Both "orientation to edge at that orientation" (`D8 -> Edge`) and "edge to the
orientation that that edge exists at" (`Map Edge D8`) are useful things to
have, so we can generate them both here.

Once we do this we can get three separate `IntMap`s after parsing the file:

```haskell
IntMap Core          -- a map of tile id's to their cores (for drawing)
IntMap (D8 -> Edge)  -- a map of tile id's to their edges at each orientation
IntMap (Map Edge D8) -- a map of tile id's to all of their edges and the orientations they are at
```

Now for the actual solve --- we're going to build up a `Map Point (Int, D8)`
one at a time, where the point (`V2 Int`) is going to contain the tile id at
that point, as well as the orientation that tile has to be at.

To do that, we're going to use a queue of "open edges": the location that the
open edge is facing, and the direction (north/south/east/west) of that open
edge -- a `Map Edge (Point, Dir)`.  We'll also keep a set of tile id's that
have not been placed yet.  And then at each step:

1.  Pop an edge off of that queue -- `(Edge, (Point, Dir))`
2.  Search to see if any non-used tiles have any matching edge
    a.  If there is not any, it means that that edge is at the edge of the
        overall map, so just skip.
    b.  If there is a tile, place that tile at the indicated `(Point, Dir)` and
        place all of *its* edges into the queue.
3.  Repeat until the queue is empty.


```haskell
-- | A placement is a Tile ID and the orientation of that tile
type Placement = (Int, D8)
type Point     = V2 Int

assembleMap
    :: IntMap (D8 -> Edge)              -- ^ tile id to the edge at each orientation
    -> IntMap (Map Edge Placement)      -- ^ tile id to the map of edges to what tile id, orientation that edge is at
    -> Map Point Placement              -- ^ map of points to the tile id, orientation at each point
assembleMap tileMap tiles0 =
        go (toQueue 0 mempty t0id allDir)
           (IM.keysSet tiles1)
           (M.singleton 0 (t0id, mempty))
  where
    -- populate the initial tile and the initial queue
    ((_   , t0Map), tiles1)  = IM.deleteFindMin tiles0
    ((_, (t0id, _)), _     ) = M.deleteFindMin  t0Map

    -- a cache of edges to tiles ID's (and orientations) that have that edge.
    tileCache :: Map Edge [Placement]
    tileCache = M.fromListWith (++)
      [ (edge, [placement])
      | (_   , tileEdges) <- IM.toList tiles0
      , (edge, placement) <- M.toList tileEdges
      ]

    go  :: Map Edge (Point, Dir)     -- ^ queue: edge -> place, orientation
        -> IntSet                    -- ^ leftover points
        -> Map Point Placement       -- ^ current map
        -> Map Point Placement       -- ^ sweet tail rescursion
    go queue tiles mp = case M.minViewWithKey queue of
      Nothing -> mp
      Just ((edge, (pos, d)), queue') ->
        case find ((`IS.member` tiles) . fst) (tileCache NEM.! edge) of
          Nothing          -> go queue' tiles mp
          Just (tileId, o) ->
                -- If we're adding a North edge, then it's the new tile's South
                -- edge; if we are adding a East edge, it's the new tile's West
                -- edge, etc; (d <> South) is the right relationship to properly
                -- flip
            let o'       = o <> D8 (d <> South) True
                newQueue = toQueue pos o'
                    tileId
                    (filter (/= d <> South) allDir)
            in  go  (newQueue <> queue)
                    (IS.delete tileId tiles)
                    (M.insert pos (tileId, invert o') mp)

    -- | For a given image, add the given edges into the queue
    toQueue
        :: Foldable f
        => Point            -- ^ location of corner
        -> D8               -- ^ orientation to insert
        -> Int              -- ^ tile id
        -> f Dir            -- ^ edges to insert
        -> Map Edge (Point, Dir)
    toQueue p0 o tileId ds = M.fromList $ toList ds <&> \d ->   -- for each dir
        ( (tileMap IM.! tileId) (o <> D8 d False)   -- the edge
        , ( p0 + rotPoint d (V2 0 (-1))             -- the new point
          , d
          )
        )
```

We can wrap this all up in a solver to extract the `Map Point Placement` (using
`assembleMap`) and the `Set Point` --- the "actual" pixel map that represents
all of the points themselves in 2d space.

```haskell
solve
    :: IntMap (Set (V2 (Finite 10)))
    -> (Map Point Placement, Set Point)
solve ts = (shiftToZero mp, blitted)
  where
    info    = toTiles <$> ts
    edgeMap = IM.mapWithKey (\i (_, e) -> (i,) <$> e) info
    edges   = snd . fst <$> info
    mp      = assembleMap edges edgeMap
    blitted = flip M.foldMapWithKey mp $ \p (tileId, o) ->
      let core = fst . fst $ info IM.! tileId
      in  S.map ((+ (p * 8)) . shiftToZero . orientPoint o) core
```

We can use the `Map Point Placement` to get the answer to part 1: just look at
the tile id's at the corners of the map.  Since we `shiftToZero`, we can just
look up `mp M.! V2 0 0`, `mp M.! V2 0 12`, `mp M.! V2 12 0`, and `mp M.! V2 12
12`, and multiply them all together.

For part 2, after we assemble the actual `Point`, we can do a search for all
dragons at all orientations.

```haskell
-- | given a pattern and a map of points, poke out all points matching that
-- pattern.
pokePattern
    :: Set Point    -- ^ pattern
    -> Set Point    -- ^ map
    -> Set Point
pokePattern pat ps0 = foldl' go ps0 (range (V2 0 0, V2 96 96))
  where
    go ps d
        | pat' `S.isSubsetOf` ps = ps S.\\ pat'
        | otherwise              = ps
      where
        pat' = S.mapMonotonic (+ d) pat
```

And now we try `pokePattern` with the dragon at all orientations until we find
one that gets any pokes:

```haskell
allDragons :: [Set Point]   -- the dragon image at all orientations

dragonCount
    :: Set Point
    -> Maybe Int
dragonCount fullMap = listToMaybe
    [ res
    | drgn <- toList allDragons
    , let res = S.size $ pokePattern (NES.toSet drgn) fullMap
    , res /= S.size fullMap
    ]
```

And that concludes my solve of what was probably the most complex challenge of
the month!  Overall a lot of moving parts, but I was at least very happy to be
able to use some knowledge of group theory (in particular, how the orientations
of a square compose and interact) to break the puzzle down into pieces that
were much easier to think about.
