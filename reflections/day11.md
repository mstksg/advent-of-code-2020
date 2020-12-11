My first day on the leaderboard! :D  21 / 352.  Had a big dip on my second part
because I had some silly typos that were difficult to catch in the moment D:

After refactoring things, I realized that part 1 and part 2 are really the
same, with only two differences:

1.  Each point as a different neighborhood set (in part 1, it's the immediate
    neighbors; in part 2, it's all of the line-of-sights in each direction).
2.  Threshold for seats unseating is 4 for part 1 and 5 for part 2.

So let's write our function parameterized on those two.  We'll be storing our
world as a `Map Point Bool`, where `False` represents an empty seat and `True`
represents a full one.  Floor points are not included in the map.

```haskell
-- | A 2-vector type from the linear library, with a very convenient Num
-- instance.
data V2 a = V2 a a

type Point = V2 Int

-- | A useful utility function I keep around that counts the number of items in
-- a container matching a predicate
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

seatRule
    :: Int                       -- ^ exit seat threshold
    -> Map Point (Set Point)     -- ^ neighbors for each point
    -> Map Point Bool
    -> Map Point Bool
seatRule thr nmp mp = M.intersectionWith go nmp mp
  where
    go neighbs = \case
      Empty -> not (all (mp M.!) neighbs)
      Full  ->
        let onNeighbs = countTrue (mp M.!) neighbs
        in  not (onNeighbs >= thr)
```

Now we just need to create our neighborhood maps.

```haskell
-- | The eight immediate neighbors around 0,0
immediateNeighbs :: [Point]
immediateNeighbs =
    [ V2 dx dy
    | dx <- [-1 .. 1]
    , dy <- if dx == 0 then [-1,1] else [-1..1]
    ]

-- | From a set of seat locations, get a map of points to all of those points'
-- neighbors where there is a seat. Should only need to be computed once.
lineOfSights1
    :: Set Point
    -> Map Set (Set Point)
lineOfSeights1 pts = M.fromSet go mp
  where
    go p _ = S.fromList
           . filter (`S.member` pts)
           . (+ p)
           $ immediateNeighbs

-- | From a set of seat locations, Get a map of points to all of those points'
-- visible neighbors. Should only need to be computed once.
lineOfSights2
    :: Set Point
    -> Map Point (Set Point)
lineOfSights2 bb pts = M.mapWithKey go pts
  where
    go p _ = S.fromList
           . mapMaybe (los p)
           $ immediateNeighbs
    los p d = find (`S.member` pts)
            . takeWhile inBoundingBox
            . tail
            $ iterate (+ d) p
    inBoundingBox = all (inRange (0, 99))
        -- inRange from Data.Ix
        -- all from Data.Foldable and V2's Foldable instance
```

(I hard-coded the bounds here, but in my actual solution I inferred it from the
input.)

Now to solve!

```haskell
-- | Handy utility function I have; repeat a function until you get the same
-- result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
        | x == y    = x
        | otherwise = go y
      where
        y = f x

solveWith
    :: Int                      -- ^ exit seat threshold
    -> Map Point (Set Point)    -- ^ neighbors for each point
    -> Map Point Bool           -- ^ initial state
    -> Int                      -- ^ equilibrium size
solveWith thr neighbs = countTrue id . fixedPoint (seatRule thr neighbs)

part1
    :: Map Point Bool
    -> Int
part1 mp = solveWith 4 los mp
  where
    los = lineOfSight1 (M.keysSet mp)

part2
    :: Map Point Bool
    -> Int
part2 mp = solveWith 5 los mp
  where
    los = lineOfSight2 (M.keysSet mp)
```
