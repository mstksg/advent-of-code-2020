Day 7
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day07.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *7* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[19][day19]* / *[20][day20]* / *[21][day21]* / *[22][day22]* / *[23][day23]* / *[24][day24]* / *[25][day25]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md
[day06]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day06.md
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

*[Prompt][d07p]* / *[Code][d07g]* / *[Rendered][d07h]*

[d07p]: https://adventofcode.com/2020/day/7
[d07g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day07.hs
[d07h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day07.html

Another AoC staple, a graph search that can be solved with recursive knot
tying!  The last one I remember off the top of my head was [2019 Day
6][2019d06].

[2019d06]: https://github.com/mstksg/advent-of-code-2019/blob/master/reflections.md#day-6

Here we can represent a graph as a map of vertices to other vertices, with an
edge value:

```haskell
type Graph v e = Map v (Map v e)
```

Exercise is left to the reader to parse our dataset into a `Graph String Int`,
a graph of bags to bags with `Int` edges.

Because our map has no cycles, we can take advantage of recursive knot tying to
"fold up" all children and sub-children.

For example, part 1 can be written as:

```haskell
allDescendants :: Ord v => Graph v e -> Map v (Set v)
allDescendants gr = descendantMap
  where
    descendantMap = gr <&>
      M.foldMapWithKey (\v _ -> S.insert v (M.findWithDefault S.empty v descendantMap))

-- note: (<&>) is flip fmap
```

Here we "assume" we already have a fully-featured `Map v (Set v)` map of
vertices to all their descendants, and then build `descendantMap` in terms of
it.  For every vertex `v` in the `Map v e` directly underneath a given vertex,
`v` is a descendant, and also all of `v`'s descendants (which we find by
looking things up in `descendantMap`, the map of all descendants).

Oh, um...oops, this found all the descendants, but we want all of the
ancestors.  So we have to flip the graph if we want to use this.

```haskell
flipGraph :: Ord v => Graph v e -> Graph v e
flipGraph mp = M.fromListWith M.union
    [ (m, M.singleton n e)
    | (n, ms) <- M.toList mp
    , (m, e ) <- M.toList ms
    ]

allAncestors :: Ord v => Graph v e -> Map v (Set v)
allAncestors = allDescendants . flipGraph
```

And so that leaves Part 1 as:

```haskell
part1 :: Graph String (String Int) -> Maybe (Set String)
part1 = M.lookup "shiny gold" . allAncestors
```

Part 2 we can do a similar way, by "assuming" we have a map of all vertices to
their "usage count", and looking things up to build it:

```haskell
usageCounts :: Ord v => Graph v Int -> Map v Int
usageCounts gr = usageMap
  where
    usageMap = gr <&> \neighbors -> sum
      [ n * (M.findWithDefault 0 v usageMap + 1)
      | (v, n) <- M.toList neighbors
      ]
```

So to find the total usage of each bag, we look under each `(v, Int)` pair in the
`Map v Int` underneath a given vertex, look up the usage of that `v` (by
looking it up in `usageMap`), add 1 (because the bag itself is used), and
multiply by `n`, the number of times the full contents of the bag is used.

And so Part 2 is:

```haskell
part2 :: Graph String (String Int) -> Maybe Int
part2 = M.lookup "shiny gold" . usageCounts
```

If we stare at the two implementations, we note that both are pretty much the
same overall structure: we are accumulating some sort of fold over all
descendants of a given node.  If we "outsource" this accumulation as a monoidal
one (for part 1, it's `Set` union, and for part 2, it's `Sum Int` addition), we
can needlessly hyper-generalize this to fold over any `Monoid` instance.

```haskell
-- | Recursively fold up a monoid value for each vertex and all of its
-- children's monoid values.  You can transform the value in-transit before it
-- is accumulated if you want.
foldMapGraph
    :: (Ord v, Monoid m)
    => (v -> m)         -- ^ embed the vertex
    -> (e -> m -> m)    -- ^ transform with edge before it is accumulated
    -> Graph v e
    -> Map v m
foldMapGraph f g gr = res
  where
    res = gr <&>
      M.foldMapWithKey (\s v -> f s <> foldMap (g v) (M.lookup s res))

allDescendants :: Ord v => Graph v e -> Map v (Set v)
allDescendants = foldMapGraph
    S.singleton     -- the node is embedded as itself
    (\_ -> id)      -- ignore the edge

usageCounts :: Ord v => Graph v Int -> Map v (Sum Int)
usageCounts = foldMapGraph
    (const 0)                   -- ignore the nodes
    (\n x -> Sum n * (x + 1))   -- the edge multiplies the accumulator plus one
```

That's the curse of Haskell, I guess?  If you write these things you can't help
but notice the common patterns, and you somehow wind up trying to figure out
the higher-order function that can abstract over them, even though you know you
don't need to :)


*[Back to all reflections for 2020][reflections]*

## Day 7 Benchmarks

```
>> Day 07a
benchmarking...
time                 2.423 ms   (2.265 ms .. 2.631 ms)
                     0.980 R²   (0.967 R² .. 1.000 R²)
mean                 2.271 ms   (2.245 ms .. 2.334 ms)
std dev              136.8 μs   (48.17 μs .. 231.7 μs)
variance introduced by outliers: 42% (moderately inflated)

* parsing and formatting times excluded

>> Day 07b
benchmarking...
time                 12.11 μs   (11.77 μs .. 12.51 μs)
                     0.991 R²   (0.987 R² .. 0.995 R²)
mean                 12.23 μs   (11.88 μs .. 12.69 μs)
std dev              1.266 μs   (913.5 ns .. 1.695 μs)
variance introduced by outliers: 87% (severely inflated)

* parsing and formatting times excluded
```

