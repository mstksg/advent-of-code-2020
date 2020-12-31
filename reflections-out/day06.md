Day 6
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day06.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *6* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[19][day19]* / *[20][day20]* / *[21][day21]* / *[22][day22]* / *[23][day23]* / *[24][day24]* / *[25][day25]*

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
[day15]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day15.md
[day16]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day16.md
[day17]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md
[day18]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day18.md
[day19]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day19.md
[day20]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day20.md
[day21]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day21.md
[day22]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day22.md
[day23]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day23.md
[day24]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day24.md
[day25]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day25.md

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
time                 143.3 μs   (135.9 μs .. 148.1 μs)
                     0.986 R²   (0.979 R² .. 0.993 R²)
mean                 148.7 μs   (145.5 μs .. 152.0 μs)
std dev              13.11 μs   (10.75 μs .. 17.27 μs)
variance introduced by outliers: 76% (severely inflated)

* parsing and formatting times excluded

>> Day 06b
benchmarking...
time                 151.2 μs   (145.6 μs .. 157.2 μs)
                     0.981 R²   (0.962 R² .. 0.994 R²)
mean                 178.9 μs   (157.5 μs .. 231.7 μs)
std dev              119.8 μs   (56.72 μs .. 202.3 μs)
variance introduced by outliers: 99% (severely inflated)

* parsing and formatting times excluded
```

