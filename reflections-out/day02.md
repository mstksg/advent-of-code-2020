Day 2
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day02.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *2* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day03]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md
[day06]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day06.md
[day07]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md

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
time                 67.91 μs   (67.76 μs .. 68.04 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 68.07 μs   (68.04 μs .. 68.14 μs)
std dev              138.2 ns   (75.99 ns .. 232.7 ns)

* parsing and formatting times excluded

>> Day 02b
benchmarking...
time                 82.17 μs   (80.65 μs .. 83.32 μs)
                     0.997 R²   (0.992 R² .. 0.999 R²)
mean                 81.85 μs   (80.70 μs .. 83.46 μs)
std dev              5.091 μs   (2.562 μs .. 8.513 μs)
variance introduced by outliers: 64% (severely inflated)

* parsing and formatting times excluded
```

