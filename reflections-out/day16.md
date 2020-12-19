Day 16
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day16.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *16* / *[17][day17]* / *[18][day18]* / *[19][day19]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
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
[day15]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day15.md
[day17]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md
[day18]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day18.md
[day19]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day19.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d16p]* / *[Code][d16g]* / *[Rendered][d16h]*

[d16p]: https://adventofcode.com/2020/day/16
[d16g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day16.hs
[d16h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day16.html

Today was a nice little self-contained constraint satisfaction problem!  Well,
it didn't have to be (apparently), but it was fun as one :)

First, our data type:

```haskell
type Passport = [Int]

data Info = Info
      { iFields :: IntervalMap Int (Set Text)
      , iYours  :: Passport
      , iTheirs :: [Passport]
      }
```

Here we're using `IntervalMap` from the *[data-interval][]* package, which
makes it easy to store data at different intervals with easy lookups.  For
example, if we have `["class"]` at interval `(1,5)`, and we had `["row"]` at
interval `(3,7)`, `IntervalMap` will merge them together (with `<>`, if we
choose) to get `["class"]` at `(1,3)`, `["class","row"]` at `(3,5)`, and
`["row"]` at `(5,7)`.

[data-interval]: https://hackage.haskell.org/package/data-interval

If we have this `IntervalMap`, part 1 becomes straightforward enough with the
efficient `IM.notMember`:

```haskell
import qualified Data.IntervalMap.Lazy as IM

part1 :: Info -> Int
part1 info = sum
    [ n
    | ns <- iTheirs info
    , n  <- ns
    , n `IM.notMember` iFields info
    ]
```

So now let's move on to the search for part 2!

Our goal is to get a list `[(Int, Set Text)]` of a column number (in the
passport) with the set of all valid field names for that position.  And because
we are going to be doing a search, we want this list in order of smallest to
largest valid-name sets.

First, we can replace the `Int`s in each passport instead with the set of
fields they are valid for

```haskell
validate :: IntervalMap Int (Set Text) -> [Int] -> Maybe [Set Text]
validate flds = traverse (`IM.lookup` flds)

validateAll :: IntervalMap Int (Set Text) -> [Passport] -> [[Set Text]]
validateAll flds = mapMaybe (validate flds)
```

Here ``(`IM.lookup` flds)`` is `Int -> Set Text`: it'll look up the `Set Text`
corresponding to the interval that the `Int` falls under in the `IntervalMap`.
It'll return `Nothing` if *any* of the `Int`s are invalid, and `Just` if *all*
of the `Int`s are valid.

Next we want to build our `[(Int, Set Text)]`.  The `Set Text` is a set of what
is valid for that column number, so to get the `Set Text` for `0`, for
instance, we need to `S.intersection` all of the first `Set Text`s in our list,;
to get the `Set Text` for `1`, we need to `S.intersection` all of the second
`Set Text`s in our lists, etc.  This can be done succinctly with a `transpose`
(`transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]`).  Then we can use
`sortOn` to sort by the size of the valids set.

```haskell
columnSets :: [[Set Text]] -> [(Int, Set Text)]
columnSets = sortOn (S.size . snd)
           . zip [0..]
           . map (foldl1' S.intersection)
           . transpose
```

Now we're ready for our search!  We'll be using `StateT` over list, to get a
backtracking search with backtracking state (I described this technique in [a
constraint solving blog
post](https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html)).
Our state will be the `Set Text` of all the "committed" fields so far.

```haskell
search :: [(Int, Set Text)] -> Maybe [(Int, Text)]
search candidateMap = listToMaybe . flip evalStateT S.empty $ do
    for candidates $ \(i, cands) -> do              -- for each (Int, Set Text):
      soFar <- get                                  -- get the seen candidates
      pick  <- lift . toList $ cands S.\\ soFar     -- pick from the Set Text not including seens
      (i, pick) <$ modify (S.insert pick)           -- propose this index/pick, inserting into seens
```

And that should be it for our search!  In the end this gets the first `[(Int,
Text)]` that is valid, matching a column ID to the field at that column.  Our
search supports backtracking through the list monad, but it should be noted
that we actually don't end up needing it for the way the puzzle input is
structured.  But, because we sort our lists first from smallest to largest
valid-sets, our solution ends up being equivalent to the non-backtracking
method and backtracking is never actually triggered.

And we can wrap it all up:

```haskell
part2 :: Info -> Int
part2 = product
    [ iYours info !! i
    | (i, fld) <- res
    , "departure" `isPrefixOf` fld
    ]
  where
    cSets    = columnSets $ validateAll (iFields info) (iTheirs info)
    Just res = search cSets
```


*[Back to all reflections for 2020][reflections]*

## Day 16 Benchmarks

```
>> Day 16a
benchmarking...
time                 821.6 μs   (817.5 μs .. 826.3 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 813.7 μs   (806.6 μs .. 821.2 μs)
std dev              22.69 μs   (16.70 μs .. 29.36 μs)
variance introduced by outliers: 18% (moderately inflated)

* parsing and formatting times excluded

>> Day 16b
benchmarking...
time                 886.2 μs   (875.7 μs .. 905.0 μs)
                     0.997 R²   (0.992 R² .. 1.000 R²)
mean                 894.0 μs   (887.9 μs .. 906.7 μs)
std dev              30.87 μs   (19.73 μs .. 54.15 μs)
variance introduced by outliers: 25% (moderately inflated)

* parsing and formatting times excluded
```

