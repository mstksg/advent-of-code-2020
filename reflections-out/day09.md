Day 9
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day09.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *9* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[19][day19]* / *[20][day20]* / *[21][day21]* / *[22][day22]* / *[23][day23]* / *[24][day24]* / *[25][day25]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md
[day06]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day06.md
[day07]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md
[day08]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day08.md
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

*[Prompt][d09p]* / *[Code][d09g]* / *[Rendered][d09h]*

[d09p]: https://adventofcode.com/2020/day/9
[d09g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day09.hs
[d09h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day09.html

Let's tackle day 9!

A good way to check if a sequence of 25 numbers can add to the 26th number is
to just iterate over everything, like we might have done in day 1:

```haskell
-- | check if, for ([x,y,z] ++ [a]), no pair in xyz can add to 'a'.  If it's
-- bad, it returns 'Just a'.
isBad :: [Int] -> Maybe Int
isBad xs0 = do
    x : xs <- Just $ reverse xs0
    let badCheck = null do
          y:ys <- tails (toList xs)
          z    <- ys
          guard $ (y + z) == x
    x <$ guard badCheck
```

I use my favorite `Maybe` do-notation trick of pattern matching within the
block to take advantage of do block short circuiting for `Maybe` with its
`MonadFail` instance.  If you reverse `xs0` then you can get the last item as
the head, and the rest as the tail :)

In `badCheck` we do a list-monad powered search (see my [Day 1
Reflections](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md))
for more details on how it works.  `badCheck` will return `True` if the search
is empty (with `null`).  `guard badCheck` will be Nothing if `badCheck` fails
(and our list is good) and `Just x` if `badCheck` succeeds (and our list is
bad).

Part 1 is then just finding the first bad sequence:

```haskell
part1 :: [Int] -> Maybe Int
part1 xs = listToMaybe
    [ y
    | ys     <- tails xs
    , Just y <- [isBad (take 26 ys)]
    ]
```

For part 2, there's a nice-ish way to do it in constant-time.  First, we can
generate a cumulative sum `cumSum` for the *entire* list.  Then we know that
`sumFrom(i,j)` in our original list is just `cumSum(j) - cumSum(i)`.  This is
similar to how definite integrals work, or also how you can find the area under
a probability density function by subtracting two points from its cumulative
distribution function.

So now the problem just becomes finding `i,j` where `cumSum(j) - cumSum(i) ==
goal`.  There's a clean imperative-ish way to do this that involves just
"sliding" your window `i,j` up from `0,1`.  If `cumSum(j) - cumSum(i)` is too
small, increase `j` by 1 to open the window up a bit.  If it's too big,
increase `i` by 1 to close the window up a bit.

```haskell
findBounds :: V.Vector Int -> Int -> Maybe (Int, Int)
findBounds ns goal = go 0 1
  where
    go !i !j = do
      x <- ns V.!? i
      y <- ns V.!? j
      case compare (y - x) goal of
        LT -> go i (j + 1)
        EQ -> pure (i, j)
        GT -> go (i + 1) j
```

And there you go!

```haskell
part2 :: [Int] -> Maybe Int
part2 xs = do
    goal <- part1 xs
    let cumSum = V.fromList (scanl' (+) 0 xs)       -- cumulative sum
    (i, j) <- findBounds cumSum goal
    let xs = take (j - i) . drop i $ ns
    pure $ minimum xs + maximum xs
```

If anything, maybe the implementation of `findBounds` shows how one might
directly translate a tight mutable loop in an imperative language into a
tail-recursive function in Haskell!

We do often like to avoid explicitly writing recursive functions when we can,
but in this case I'm not sure if there's a way to get around it other than
switching to a full on mutable answer, or in a very complex way that is
extremely specific to the situation.  If you think of one, let me know! :D


*[Back to all reflections for 2020][reflections]*

## Day 9 Benchmarks

```
>> Day 09a
benchmarking...
time                 164.4 μs   (154.6 μs .. 179.6 μs)
                     0.973 R²   (0.965 R² .. 0.984 R²)
mean                 178.5 μs   (170.9 μs .. 184.6 μs)
std dev              22.16 μs   (19.45 μs .. 24.65 μs)
variance introduced by outliers: 86% (severely inflated)

* parsing and formatting times excluded

>> Day 09b
benchmarking...
time                 215.2 μs   (208.9 μs .. 219.5 μs)
                     0.987 R²   (0.977 R² .. 0.995 R²)
mean                 214.1 μs   (207.6 μs .. 224.2 μs)
std dev              28.57 μs   (19.93 μs .. 40.26 μs)
variance introduced by outliers: 87% (severely inflated)

* parsing and formatting times excluded
```

