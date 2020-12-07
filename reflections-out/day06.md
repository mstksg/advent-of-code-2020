Day 6
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day06.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *6* / *[7][day07]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md
[day07]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md

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
time                 155.1 μs   (152.2 μs .. 157.4 μs)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 151.8 μs   (147.8 μs .. 153.9 μs)
std dev              10.48 μs   (8.453 μs .. 12.96 μs)
variance introduced by outliers: 66% (severely inflated)

* parsing and formatting times excluded

>> Day 06b
benchmarking...
time                 149.4 μs   (142.5 μs .. 156.2 μs)
                     0.990 R²   (0.983 R² .. 0.997 R²)
mean                 154.1 μs   (151.1 μs .. 157.2 μs)
std dev              10.23 μs   (7.084 μs .. 16.01 μs)
variance introduced by outliers: 64% (severely inflated)

* parsing and formatting times excluded
```

