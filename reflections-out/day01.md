Day 1
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day01.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *1* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
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

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d01p]* / *[Code][d01g]* / *[Rendered][d01h]*

[d01p]: https://adventofcode.com/2020/day/1
[d01g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day01.hs
[d01h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day01.html

So there's a simple-ish Haskell solution for these problems,

`tails` lets you separate out each item in a list with the list of items after
it:

```haskell
ghci> tails [1,2,3,4]
[1:[2,3,4], 2:[3,4], 3:[4], 4:[]]
```

```haskell
findPair :: [Int] -> Maybe Int
findPair xs = listToMaybe $ do
    x:ys <- tails xs
    y    <- ys
    guard (x + y == 2020)
    pure (x*y)

findTriple :: [Int] -> Maybe Int
findTriple xs = listToMaybe $ do
    x:ys <- tails xs
    y:zs <- tails ys
    z    <- zs
    guard (x + y + z == 2020)
    pure (x*y*z)
```

But this method is a little bit "extra", since we actually don't need to search
all of `ys` for the proper sum...if we pick `x` as `500`, then we really only
need to check if `1520` is a part of `ys`.

So we really only need to check for set inclusion:

```haskell
import qualified Data.IntSet as IS

findPair :: Int -> IS.IntSet -> Maybe Int
findPair goal xs = listToMaybe $ do
    x <- IS.toList xs
    let y = goal - x
    guard (y `IS.member` xs)
    pure (x * y)
```

And our first part will be `findPair 2020`!

You could even implement `findTriple` in terms of `findPair`, using `IS.split`
to partition a set into all items smaller than and larger than a number.
Splitting is a very efficient operation on a binary search tree like `IntSet`:

```haskell
findTriple :: Int -> IS.IntSet -> Maybe Int
findTriple goal xs = listToMaybe $ do
    x <- IS.toList xs
    let (_, ys) = IS.split x xs
        goal'   = goal - x
    case findPair goal' ys of
      Nothing -> empty
      Just yz -> pure (x*yz)
```

But hey...this recursive descent is kind of neat.  We could write a general
function to find any goal in any number of items!

```haskell
-- | Given a number n of items and a goal sum and a set of numbers to
-- pick from, finds the n numbers in the set that add to the goal sum.
knapsack
    :: Int              -- ^ number of items n to pick
    -> Int              -- ^ goal sum
    -> IS.IntSet        -- ^ set of options
    -> Maybe [Int]      -- ^ resulting n items that sum to the goal
knapsack 0 _    _  = Nothing
knapsack 1 goal xs
    | goal `IS.member` xs = Just [goal]
    | otherwise           = Nothing
knapsack n goal xs = listToMaybe $ do
    x <- IS.toList xs
    let goal'   = goal - x
        (_, ys) = IS.split x xs
    case knapsack (n - 1) goal' ys of
      Nothing -> empty
      Just rs -> pure (x:rs)
```

And so we have:

```haskell
part1 :: [Int] -> Maybe Int
part1 = knapsack 2 2020 . IS.fromList

part2 :: [Int] -> Maybe Int
part2 = knapsack 3 2020 . IS.fromList
```

And we could go on, and on, and on!

Definitely very unnecessary, but it does shave my time on Part 2 down from
around 2ms to around 20μs :)


*[Back to all reflections for 2020][reflections]*

## Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 7.149 μs   (6.779 μs .. 7.486 μs)
                     0.978 R²   (0.971 R² .. 0.987 R²)
mean                 7.196 μs   (6.812 μs .. 7.467 μs)
std dev              1.024 μs   (877.4 ns .. 1.230 μs)
variance introduced by outliers: 93% (severely inflated)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 60.32 μs   (56.08 μs .. 65.27 μs)
                     0.977 R²   (0.962 R² .. 0.993 R²)
mean                 61.98 μs   (60.08 μs .. 64.45 μs)
std dev              6.926 μs   (5.163 μs .. 10.18 μs)
variance introduced by outliers: 86% (severely inflated)

* parsing and formatting times excluded
```

