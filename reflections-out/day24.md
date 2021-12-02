Day 24
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day24.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[19][day19]* / *[20][day20]* / *[21][day21]* / *[22][day22]* / *[23][day23]* / *24* / *[25][day25]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md
[day06]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day06.md
[day07]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md
[day08]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day08.md
[day09]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day09.md
[day10]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day10.md
[day11]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day11.md
[day12]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day12.md
[day13]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day13.md
[day14]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day14.md
[day15]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day15.md
[day16]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day16.md
[day17]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md
[day18]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day18.md
[day19]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day19.md
[day20]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day20.md
[day21]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day21.md
[day22]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day22.md
[day23]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day23.md
[day25]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day25.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d24p]* / *[Code][d24g]* / *[Rendered][d24h]*

[d24p]: https://adventofcode.com/2020/day/24
[d24g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day24.hs
[d24h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day24.html

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


*[Back to all reflections for 2020][reflections]*

## Day 24 Benchmarks

```
>> Day 24a
benchmarking...
time                 2.597 ms   (2.551 ms .. 2.639 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 2.579 ms   (2.545 ms .. 2.614 ms)
std dev              111.4 μs   (82.30 μs .. 141.5 μs)
variance introduced by outliers: 28% (moderately inflated)

>> Day 24b
benchmarking...
time                 272.1 ms   (247.2 ms .. 296.5 ms)
                     0.996 R²   (0.996 R² .. 1.000 R²)
mean                 273.7 ms   (264.8 ms .. 286.5 ms)
std dev              13.87 ms   (1.266 ms .. 18.02 ms)
variance introduced by outliers: 16% (moderately inflated)
```

