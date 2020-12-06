Day 6
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day06.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *6*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d06p]* / *[Code][d06g]* / *[Rendered][d06h]*

[d06p]: https://adventofcode.com/2020/day/6
[d06g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day06.hs
[d06h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day06.html

Another day that is fairly straightforward in Haskell, I feel!  But in other
languages that support functional approaches, it should be straightforward as
well.

The answer involves lists of groups of responses:

```haskell
import           Data.List.NonEmpty
import           Data.Set
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as S

type Response = Set Char
type Group    = NonEmpty Response

parseAnswers :: Set Char -> [Group]
parseAnswers = mapMaybe ((fmap . fmap) S.fromList . NE.nonEmpty . lines)
             . splitOn "\n\n"
```

And now we just need to decide how to aggregate each group.  For part 1, this
requires a set union between every `Response` in a `Group`:

```haskell
part1 :: [Group] -> Int
part1 = sum . map (S.size . foldr1 S.union)
```

(`foldr1` here is safe because we have a non-empty container)

And for part 2, this requires a set intersection between every `Response` in a
`Group`:


```haskell
part2 :: [Group] -> Int
part2 = sum . map (S.size . foldr1 S.intersection)
```

That's it!


*[Back to all reflections for 2020][reflections]*

## Day 6 Benchmarks

```
>> Day 06a
benchmarking...
time                 21.36 μs   (20.45 μs .. 22.05 μs)
                     0.988 R²   (0.983 R² .. 0.993 R²)
mean                 20.87 μs   (20.16 μs .. 21.50 μs)
std dev              2.235 μs   (1.787 μs .. 2.498 μs)
variance introduced by outliers: 87% (severely inflated)

* parsing and formatting times excluded

>> Day 06b
benchmarking...
time                 21.42 μs   (20.80 μs .. 22.26 μs)
                     0.991 R²   (0.985 R² .. 0.996 R²)
mean                 22.43 μs   (21.93 μs .. 23.20 μs)
std dev              2.279 μs   (1.655 μs .. 3.401 μs)
variance introduced by outliers: 86% (severely inflated)

* parsing and formatting times excluded
```

