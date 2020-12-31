Day 24 brings us our third cellular automata puzzle of the year! :D  The other
ones were [Day
11](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day11.md)
and [Day
17](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md).
In fact, I was able to mostly copy and paste my stepper code for Day 17 :)

The main twist here is that we'd have to use hexy stepping and hexy neighbors.
My initial solve used the *[grid](https://hackage.haskell.org/package/grid)*
library to get the hexy steps neighbors, but I did go back and [implement the
tiling
myself](https://github.com/mhwombat/grid/wiki/Implementation%3A-Hexagonal-tiles) because
it wasn't too bad :)

For part 1, it can be nice to have some intermediate data types

```haskell
data HexDirection = West
                  | Northwest
                  | Northeast
                  | East
                  | Southeast
                  | Southwest

toDirs :: String -> Maybe [HexDirection]
toDirs = \case
    [] -> Just []
    'w':ds -> (West:) <$> toDirs ds
    'e':ds -> (East:) <$> toDirs ds
    'n':'e':ds -> (Northeast:) <$> toDirs ds
    'n':'w':ds -> (Northwest:) <$> toDirs ds
    's':'e':ds -> (Southeast:) <$> toDirs ds
    's':'w':ds -> (Southwest:) <$> toDirs ds
    _ -> Nothing

hexOffset :: HexDirection -> Point
hexOffset = \case
    West      -> V2 (-1)  0
    Northwest -> V2 (-1)  1
    Northeast -> V2   0   1
    East      -> V2   1   0
    Southeast -> V2   1 (-1)
    Southwest -> V2   0 (-1)
```

So we can parse into a list of `[HexDirection]` paths, and then we can get our
starting points by xoring all of the final points:

```haskell
import Data.Bits

initialize :: [[HexDirection]] -> Set Point
initialize = M.keysSet . M.filter id . M.fromListWith xor
           . map (\steps -> (sum (map hexOffset steps), True))
```

And this gives us the set of all active points, which we can use to answer part
one.  But now, on to the simulation!

First, we can expand the neighbors of a given point in our hexy coords:

```haskell
neighbors :: Point -> Set Point
neighbors (V2 x y) = S.fromDistinctAscList
    [ V2 (x-1) y
    , V2 (x-1) (y+1)
    , V2 x     (y-1)
    , V2 x     (y+1)
    , V2 (x+1) (y-1)
    , V2 (x+1) y
    ]
```

And our step function looks more or less the same as day 17:

```haskell
step :: Set Point -> Set Point
step ps = stayAlive <> comeAlive
  where
    neighborCounts :: Map Point Int
    neighborCounts = M.unionsWith (+)
      [ M.fromSet (const 1) (neighbors p)
      | p <- S.toList ps
      ]
    stayAlive = M.keysSet . M.filter (\n -> n == 1 || n == 2) $
                  neighborCounts `M.restrictKeys` ps
    comeAlive = M.keysSet . M.filter (== 2) $
                  neighborCounts `M.withoutKeys`  ps
```

First we collect a `Map Point Int` of each point to how many live neighbors it
has.  Then the *live* points (``neighborCounts `M.restrictKeys` ps``) are
filtered for only the ones with 1 or 2 live neighbors, and the *dead* points
(``neighborCounts `M.withoutKeys` ps``) are filtered for only the ones with 2
live neighbors.  And the resulting new set of live points is `stayAlive <>
comeAlive`.

```haskell
part1 :: [[HexDirection]] -> Int
part1 = S.size . initialize

part2 :: [[HexDirection]] -> Int
part2 paths = S.size (iterate step pts !!! 100)
  where
    pts = initialize paths
```
