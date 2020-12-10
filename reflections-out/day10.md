Day 10
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day10.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *10*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md
[day06]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day06.md
[day07]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md
[day08]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day08.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d10p]* / *[Code][d10g]* / *[Rendered][d10h]*

[d10p]: https://adventofcode.com/2020/day/10
[d10g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day10.hs
[d10h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day10.html

Today is another day where the "automatically build a memoized recursive map"
in Haskell really shines :)  It's essentially the same problem as Day 7.

For the first part, once you sort the list, you can compute the differences and
then build a frequency map

```haskell
-- | Build a frequency map
freqs :: Ord a => [a] -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (drop 1 xs) xs
```

```haskell
ghci> diffs [1,3,4,7]
[2,1,3]
```

And so part 1 can be done with:

```haskell
part1 :: [Int] -> Int
part1 xs = (stepFreqs M.! 1) * (stepFreqs M.! 3)
  where
    xs' = 0 : xs ++ [maximum xs + 3]
    stepFreqs = freqs (diffs (sort xs))
```


For part 2, if we get an `IntSet` of all of your numbers (and adding the zero,
and the goal, the maximum + 3), then we can use it to build our `IntMap` of all
the number of paths from a given number.

```haskell
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

-- | A map of numbers to the count of how many paths from that number to
-- the goal
pathsToGoal :: IntSet -> IntMap Int
pathsToGoal xs = res
  where
    res = flip IM.fromSet xs $ \i ->
      if i == goal
        then 1
        else sum [ IM.findWithDefault 0 (i + j) res
                 | j <- [1,2,3]
                 ]
    goal = IS.findMax is
```

Our answer is `res`, the map of numbers to the count of how many paths exist
from that number to the goal.  To generate the count for a given number `i`, we
add the number of paths from `i+1`, `i+2`, and `i+3`.  We get that count by
looking it up in `res`!

```haskell
part2 :: [Int] -> Int
part2 xs = pathsToGoal xs IM.! 0
  where
    xs' = IS.fromList (0 : xs ++ [maximum xs + 3])
```


*[Back to all reflections for 2020][reflections]*

## Day 10 Benchmarks

```
>> Day 10a
benchmarking...
time                 7.379 μs   (6.799 μs .. 7.864 μs)
                     0.969 R²   (0.957 R² .. 0.980 R²)
mean                 7.338 μs   (6.925 μs .. 7.743 μs)
std dev              1.302 μs   (1.178 μs .. 1.470 μs)
variance introduced by outliers: 96% (severely inflated)

* parsing and formatting times excluded

>> Day 10b
benchmarking...
time                 9.736 μs   (9.534 μs .. 9.874 μs)
                     0.994 R²   (0.990 R² .. 0.998 R²)
mean                 10.07 μs   (9.800 μs .. 10.71 μs)
std dev              1.506 μs   (988.1 ns .. 2.439 μs)
variance introduced by outliers: 94% (severely inflated)

* parsing and formatting times excluded
```

