Day 3
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day03.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *3* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md
[day04]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md
[day06]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day06.md
[day07]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md
[day08]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day08.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d03p]* / *[Code][d03g]* / *[Rendered][d03h]*

[d03p]: https://adventofcode.com/2020/day/3
[d03g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day03.hs
[d03h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day03.html

Here I'm going to list two methods --- one that involves pre-building a set to
check if a tree is at a given point, and the other involves just a single
direct traversal checking all valid points for trees!

First of all, I'm going to reveal one of my favorite secrets for parsing 2D
ASCII maps!

```haskell
asciiGrid :: IndexedFold (Int, Int) String Char
asciiGrid = reindexed swap (lined <.> folded)
```

This gives you an indexed fold (from the *[lens][]* package) iterating over
each character in a string, indexed by `(x,y)`!

[lens]: https://hackage.haskell.org/package/lens

This lets us parse today's ASCII forest pretty easily into a `Set (Int, Int)`:

```haskell
parseForest :: String -> Set (Int, Int)
parseForest = ifoldMapOf asciiGrid $ \xy c -> case c of
    '#' -> S.singleton xy
    _   -> S.empty
```

This folds over the input string, giving us the `(x,y)` index and the character
at that index.  We accumulate with a monoid, so we can use a `Set (Int, Int)`
to collect the coordinates where the character is `'#'` and ignore all other
coordinates.

Admittedly, `Set (Int, Int)` is sliiiightly overkill, since you could probably
use `Vector (Vector Bool)` or something with `V.fromList . map (V.fromList .
(== '#')) . lines`, and check for membership with double-indexing.  But I was
bracing for something a little more demanding, like having to iterate over all
the trees or something.  Still, sparse grids are usually my go-to data
structure for Advent of Code ASCII maps.

Anyway, now we need to be able to traverse the ray.  We can write a function to
check all points in our line, given the slope (delta x and delta y):

```haskell
countTrue :: (a -> Bool) -> [a] -> Int
countTrue p = length . filter p

countLine :: Int -> Int -> Set (Int, Int) -> Int
countLine dx dy pts = countTrue valid [0..322]
  where
    valid i = (x, y) `S.member` pts
      where
        x = (i * dx) `mod` 31
        y = i * dy
```

And there we go :)

```haskell
part1 :: Set (Int, Int) -> Int
part1 = countLine 1 3

part2 :: Set (Int, Int) -> Int
part2 pts = product $
    [ countLine 1 1
    , countLine 3 1
    , countLine 5 1
    , countLine 7 1
    , countLine 1 2
    ] <*> [pts]
```

Note that this checks a lot of points we wouldn't normally need to check: any y
points out of range (322) for `dy > 1`.  We could add a minor optimization to
only check for membership if `y` is in range, but because our check is a set
lookup, it isn't too inefficient and it always returns `False` anyway.  So a
small price to pay for slightly more clean code :)

So this was the solution I used to submit my original answers, but I started
thinking the possible optimizations.  I realized that we could actually do the
whole thing in a single traversal...since we could associate each of the points
with coordinates as we go along, and reject any coordinates that would not be
on the line!

We can write a function to check if a coordinate is on a line:

```haskell
validCoord
    :: Int      -- ^ dx
    -> Int      -- ^ dy
    -> (Int, Int)
    -> Bool
validCoord dx dy = \(x,y) ->
    let (i,r) = y `divMod` dy
    in  r == 0 && (dx * i) `mod` 31 == x
```

And now we can use `lengthOf` with the coordinate fold up there, which counts
how many traversed items match our fold:

```haskell
countLineDirect :: Int -> Int -> String -> Int
countLineDirect dx dy = lengthOf (asciiGrid . ifiltered tree)
  where
    checkCoord = validCoord dx dy
    tree pt c = c == '#' && checkCoord pt
```

And this gives the same answer, with the same interface!

```haskell
part1 :: String -> Int
part1 = countLineDirect 1 3

part2 :: String -> Int
part2 pts = product $
    [ countLineDirect 1 1
    , countLineDirect 3 1
    , countLineDirect 5 1
    , countLineDirect 7 1
    , countLineDirect 1 2
    ] <*> [pts]
```

Is the direct single-traversal method any faster?

Well, it's complicated, slightly.  There's a clear benefit in the pre-built set
method for part 2, since we essentially build up an efficient structure (`Set`)
that we re-use for all five lines.  We get the most benefit if we build the set
once and re-use it many times, since we only have to do the actual coordinate
folding once.

So, directly comparing the two methods, we see the single-traversal as
faster for part 1 and slower for part 2.

However, we can do a little better for the single-traversal method.  As it
turns out, the lens indexed fold is kind of slow.  I was able to write the
single-traversal one a much faster way by directly just using `zip [0..]`,
without losing too much readability.  And with this *direct* single traversal
and computing the indices manually, we get a much faster time for part 1 (about
ten times faster!) and a slightly faster time for part 2 (about 5 times
faster).  The benchmarks for this optimized version are what is presented
below.


*[Back to all reflections for 2020][reflections]*

## Day 3 Benchmarks

```
>> Day 03a
benchmarking...
time                 319.0 μs   (303.8 μs .. 334.5 μs)
                     0.985 R²   (0.979 R² .. 0.991 R²)
mean                 337.7 μs   (329.6 μs .. 343.8 μs)
std dev              23.33 μs   (17.11 μs .. 30.25 μs)
variance introduced by outliers: 63% (severely inflated)

* parsing and formatting times excluded

>> Day 03b
benchmarking...
time                 1.540 ms   (1.452 ms .. 1.628 ms)
                     0.982 R²   (0.973 R² .. 0.992 R²)
mean                 1.599 ms   (1.551 ms .. 1.643 ms)
std dev              155.1 μs   (116.6 μs .. 195.2 μs)
variance introduced by outliers: 70% (severely inflated)

* parsing and formatting times excluded
```

