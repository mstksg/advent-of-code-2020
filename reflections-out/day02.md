Day 2
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day02.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *2* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[19][day19]* / *[20][day20]* / *[21][day21]* / *[22][day22]* / *[23][day23]* / *[24][day24]* / *[25][day25]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
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
[day24]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day24.md
[day25]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day25.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d02p]* / *[Code][d02g]* / *[Rendered][d02h]*

[d02p]: https://adventofcode.com/2020/day/2
[d02g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day02.hs
[d02h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day02.html

Day 2, not too bad for Haskell either :)

There is some fun in parsing here:

```haskell
data Policy = P
    { pIx1  :: Int
    , pIx2  :: Int
    , pChar :: Char
    , pPass :: String
    }

parsePolicy :: String -> Maybe Policy
parsePolicy str = do
    [ixes,c:_,pwd] <- pure $ words str
    [ix1,ix2]      <- pure $ splitOn "-" ixes
    P <$> readMaybe ix1
      <*> readMaybe ix2
      <*> pure c
      <*> pure pwd
```

I used one of my more regular do-block tricks: if you pattern match in a
`Maybe` do-block, then failed pattern matches will turn the whole thing into a
`Nothing`.  So if any of those list literal pattern matches failed, the whole
block will return `Nothing`.

In any case, we just need to write a function to check if a given policy is
valid for either criteria:

```haskell
countTrue :: (a -> Bool) -> [a] -> Int
countTrue p = length . filter p

validate1 :: Policy -> Bool
validate1 P{..} = n >= pIx1 && n <= pIx2
  where
    n = countTrue (== pChar) pPass

validate2 :: Policy -> Bool
validate2 P{..} = n == 1
  where
    n = countTrue (== pChar) [pPass !! (pIx1 - 1), pPass !! (pIx2 - 1)]
```

And so parts 1 and 2 are just a count of how many policies are true :)

```haskell
part1 :: [Policy] -> Int
part1 = countTrue validate1

part2 :: [Policy] -> Int
part2 = countTrue validate2
```


*[Back to all reflections for 2020][reflections]*

## Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 55.69 μs   (55.61 μs .. 55.78 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 55.89 μs   (55.82 μs .. 56.03 μs)
std dev              323.1 ns   (232.5 ns .. 422.3 ns)

* parsing and formatting times excluded

>> Day 02b
benchmarking...
time                 42.96 μs   (42.88 μs .. 43.06 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 43.11 μs   (43.07 μs .. 43.19 μs)
std dev              196.8 ns   (94.94 ns .. 332.4 ns)

* parsing and formatting times excluded
```

