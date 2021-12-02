Day 21
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day21.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[19][day19]* / *[20][day20]* / *21* / *[22][day22]* / *[23][day23]* / *[24][day24]* / *[25][day25]*

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
[day16]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day16.md
[day17]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md
[day18]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day18.md
[day19]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day19.md
[day20]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day20.md
[day22]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day22.md
[day23]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day23.md
[day24]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day24.md
[day25]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day25.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d21p]* / *[Code][d21g]* / *[Rendered][d21h]*

[d21p]: https://adventofcode.com/2020/day/21
[d21g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day21.hs
[d21h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day21.html

Another nice self-contained constraint satisfaction problem, along the lines of
[Day
16](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day16.md)
:)  Actually, after solving this one, I went back and rewrote my day 16
solution in terms of a common solver function that works for both!

```haskell
-- | Given a map of @k@ to possible @a@s for that @k@, find possible
-- configurations where each @k@ is given its own unique @a@.
pickUnique :: (Ord k, Ord a) => [(k, Set a)] -> [Map k a]
pickUnique mp = flip evalStateT S.empty $ do
    fmap M.fromList . for opts . traverse $ \poss -> do
      seen <- get
      pick <- lift $ S.toList (poss `S.difference` seen)
      pick <$ modify (S.insert pick)
  where
    opts = sortOn (S.size . snd) mp
```

It uses `StateT` over list, like I described in [a constraint solving blog
post](https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html).
Basically it explores all of the possibilities of drawing from a state of
"items left-over to assign".  The state is a `Set a` of items not yet picked,
and at every step we non-deterministically `pick` an `a` out of the given `(k,
Set a)` of options that hasn't already been chosen.  We use that pick and
add that picked item to the picked item set along that branch.

We also sort by the size of the possibility set for each `k`, because starting
with smaller possibilities keeps our tree tight at the top, instead of wide ---
we can eliminate options much more quickly.

Now all we need to do is to get our information into a `[(k, Set a)]`.  In our
case, this is `[(String, Set String)]` -- with each allergen, associate a set
of possible foods they might be associated with.

We can do this by just taking an intersection of all the possibilities on each
line:

```haskell
assembleOptions
    :: (Ord k, Ord a)
    => [(Set a, Set k)] -- set of foods, set of allergens
    -> Map k (Set a)    -- each allergen with the foods they were seen with in all occurrences
assembleOptions info = M.unionsWith S.intersection $
    [ M.fromSet (const igr) alg   -- a map of allergens to all foods they were seen with in this item
    | (igr, alg) <- info
    ]
```

We generate a list of allergens to all foods they were seen with on each item,
and then `intersect` all of those foods within an allergen, so that our final
`Map k (Set a)` matches each `k` allergen with a set ofall foods that were
present in *all* of the occurrences of each allergen.


Now part 2 is basically just reading off the results of `pickUnique`

```haskell
part2 :: [(Set String, Set String)] -> Maybe [String]
part2 = fmap M.elems . listToMaybe . pickUnique . assembleOptions
```

We definitely have a nice advantage here in that the `Map String String` (the
result map of allergens to foods) already is sorted in order of allergens
(alphabetically), so no need to do anything other than just `M.elems` :)

Part 1 is definitely slightly more complicated: not only do we need to find the
allergenic foods, we have to count the occurrences of non-allergenic foods in
all the items:

```haskell
part2 :: [(Set String, Set String)] -> Maybe Int
part2 info = do
    allergenicFoods <- fmap (S.fromList . M.elems)
                     . listToMaybe
                     . pickUnique
                     . assembleOptions
                     $ info
    pure . sum $
      [ length $ filter (`S.notMember` allergenicFoods) foods
      | (foods, _) <- info
      ]
  where
    allFoodOccurrences :: [String]
    allFoodOccurrences = concatMap (S.toList . fst) info
```


*[Back to all reflections for 2020][reflections]*

## Day 21 Benchmarks

```
>> Day 21a
benchmarking...
time                 270.6 μs   (267.0 μs .. 277.0 μs)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 273.1 μs   (269.2 μs .. 283.4 μs)
std dev              22.37 μs   (8.162 μs .. 40.92 μs)
variance introduced by outliers: 71% (severely inflated)

* parsing and formatting times excluded

>> Day 21b
benchmarking...
time                 162.9 μs   (160.4 μs .. 165.9 μs)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 160.2 μs   (158.4 μs .. 165.3 μs)
std dev              9.685 μs   (3.385 μs .. 17.84 μs)
variance introduced by outliers: 59% (severely inflated)

* parsing and formatting times excluded
```

