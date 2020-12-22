Day 11
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day11.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *11* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[19][day19]* / *[22][day22]*

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
[day12]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day12.md
[day13]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day13.md
[day14]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day14.md
[day15]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day15.md
[day16]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day16.md
[day17]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md
[day18]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day18.md
[day19]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day19.md
[day22]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day22.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d11p]* / *[Code][d11g]* / *[Rendered][d11h]*

[d11p]: https://adventofcode.com/2020/day/11
[d11g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day11.hs
[d11h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day11.html

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


*[Back to all reflections for 2020][reflections]*

## Day 11 Benchmarks

```
>> Day 11a
benchmarking...
time                 131.5 ms   (126.6 ms .. 138.6 ms)
                     0.996 R²   (0.988 R² .. 1.000 R²)
mean                 135.8 ms   (133.3 ms .. 139.1 ms)
std dev              4.926 ms   (3.075 ms .. 7.107 ms)
variance introduced by outliers: 11% (moderately inflated)

* parsing and formatting times excluded

>> Day 11b
benchmarking...
time                 136.4 ms   (129.4 ms .. 142.6 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 137.5 ms   (134.5 ms .. 141.0 ms)
std dev              5.265 ms   (3.297 ms .. 7.107 ms)
variance introduced by outliers: 11% (moderately inflated)

* parsing and formatting times excluded
```

