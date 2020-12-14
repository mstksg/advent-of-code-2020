Day 6
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day06.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *6* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md
[day07]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md
[day08]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day08.md
[day09]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day09.md
[day10]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day10.md
[day11]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day11.md
[day12]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day12.md
[day13]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day13.md
[day14]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day14.md

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
time                 145.0 μs   (137.0 μs .. 152.1 μs)
                     0.985 R²   (0.980 R² .. 0.993 R²)
mean                 142.7 μs   (139.0 μs .. 146.0 μs)
std dev              12.22 μs   (10.47 μs .. 14.09 μs)
variance introduced by outliers: 75% (severely inflated)

* parsing and formatting times excluded

>> Day 06b
benchmarking...
time                 154.7 μs   (152.5 μs .. 157.4 μs)
                     0.993 R²   (0.978 R² .. 0.999 R²)
mean                 153.4 μs   (149.9 μs .. 161.0 μs)
std dev              15.76 μs   (8.020 μs .. 28.24 μs)
variance introduced by outliers: 81% (severely inflated)

* parsing and formatting times excluded
```

