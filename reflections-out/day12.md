Day 12
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day12.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *12* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[19][day19]* / *[20][day20]* / *[21][day21]* / *[22][day22]* / *[23][day23]*

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

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d12p]* / *[Code][d12g]* / *[Rendered][d12h]*

[d12p]: https://adventofcode.com/2020/day/12
[d12g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day12.hs
[d12h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day12.html

Hello!  Today's puzzle for me ended up a neat exercise in fitting together
simple parts into something fun.

To preface this, I do usually represent all my coordinates using `V2 Int` from
the *[linear](https://hackage.haskell.org/package/linear)* library, which
supports addition and scaling:

```haskell
data V2 a = V2 !a !a

type Point = V2 Int

-- | You can add points using the Num instance
(+) :: Point -> Point -> Point

-- | You can do scaling
(*^) :: Int -> Point -> Point
```

And I have a utility type that represents a compass direction:

```haskell
data Dir = North | East | South | West

dirPoint :: Dir -> Point
dirPoint = \case
    North -> V2   0   1
    East  -> V2   1   0
    South -> V2   0 (-1)
    West  -> V2 (-1)  0

rotPoint :: Num a => Dir -> V2 a -> V2 a
rotPoint = \case
    North -> id
    East  -> \(V2 x y) -> V2   y  (-x)
    West  -> \(V2 x y) -> V2 (-y)   x
    South -> negate
```

And I do like to define a `Group` interface for my `Dir` type, just for fun.

```haskell
-- | If you consider a Dir as a turn, then `mulDir a b` is like turning a, then
-- turning b.
mulDir :: Dir -> Dir -> Dir
mulDir North = id
mulDir East  = \case North -> East
                     East  -> South
                     South -> West
                     West  -> North
mulDir South = \case North -> South
                     East  -> West
                     South -> North
                     West  -> East
mulDir West  = \case North -> West
                     East  -> North
                     South -> East
                     West  -> South

-- | '<>' is 'mulDir'.
instance Semigroup Dir where
    (<>) = mulDir

-- | If you consider Dir as a turn, then turning by North is the same as not
-- turning at all.
instance Monoid Dir where
    mempty = North

-- | Reverse a turn.  Not needed for this puzzle, but still useful in general.
instance Group Dir where
    invert = \case North -> South
                   East  -> West
                   South -> North
                   West  -> East
```

I did not write any of this for the puzzle --- this is just a nice way I like
to think about directions and points in my head :)

One major advantage of defining a `Semigroup` instance for `Dir` is that you can
take advantage of the `pow` function from
[Data.Group](https://hackage.haskell.org/package/groups-0.5.2/docs/Data-Group.html):

```haskell
pow :: Group m => m -> Int -> m
```

which is like `stimes`, but supporting negative numbers.  `pow x 3` is `x <> x
<> x`, and `pow x (-3)` is `invert x <> invert x <> invert x`, or `invert (x <>
x <> x)` (same thing, 'cause Group theory).  We don't actually need the support
for negative numbers in this puzzle, so we could just use `stimes`, but it's
nice that we can just use `pow` and not think about our input range.  And,
though it doesn't matter for this challenge, it also uses [repeated
squaring](https://en.wikipedia.org/wiki/Exponentiation_by_squaring) so it can
do these operations in log-n time (`pow x 1000000000` only takes 30
operations), which is pretty neat for a lot of different applications (like [in
my writeup for 2019 Day
22](https://blog.jle.im/entry/shuffling-things-up.html)).

Anyway I think that's enough preamble...now let's use it! :D  Each instruction
seems to be one of three forms: "go forward", "turn", or "move an absolute
vector".  So I represented these three as a data type, parameterized by the
amount to go forward, the direction to turn, and the vector to move by,
respectively.

And each first character gives us a different way to process the `Int`
argument, so I stored those instructions in a `Map`.  Then we can parse it by
just using `readMaybe :: Read a => String -> Maybe a` on a pattern match.

```haskell
data Instr = Forward Int
           | Turn Dir
           | Move Point
  deriving Show

-- | A map of a Char to the way to interpret the Int argument
mkInstr :: Map Char (Int -> Instr)
mkInstr = M.fromList
    [ ('F', Forward)
    , ('L', Turn . pow West . (`div` 90))
    , ('R', Turn . pow East . (`div` 90))
    , ('N', Move . (*^ dirPoint North))
    , ('S', Move . (*^ dirPoint South))
    , ('E', Move . (*^ dirPoint East ))
    , ('W', Move . (*^ dirPoint West ))
    ]

parseInstr :: String -> Maybe Instr
parseInstr []    = Nothing
parseInstr (c:n) = M.lookup c mkInstr <*> readMaybe n
```

```haskell
ghci> parseInstr "F30"
Forward 30
ghci> parseInstr "L270"
Turn East
ghci> parseInstr "N15"
Move (V2 0 15)
```

And now part 1, part 2 are basically just different ways of folding through a
list of instructions:

```haskell
toInstrs :: String -> [Instr]
toInstrs = traverse parseInstr . lines

-- | Use (ship heading, position) as the state
part1 :: [Instr] -> (Dir, Point)
part1 = foldl' go (East, V2 0 0)
  where
    go :: (Dir, Point) -> Instr -> (Dir, Point)
    go (!dir, !p) = \case
      Forward n -> (dir     , p + n *^ dirPoint dir)
      Turn d    -> (dir <> d, p                    )
      Move r    -> (dir     , p + r                )

-- | Use (ship position, waypoint vector from ship) as the state
part2 :: [Instr] -> (Point, Point)
part2 = foldl' go (V2 0 0, V2 10 1)
  where
    go :: (Point, Point) -> Instr -> (Point, Point)
    go (!shp, !wp) = \case
      Forward n -> (shp + n *^ wp, wp           )
      Turn d    -> (shp          , rotPoint d wp)
      Move r    -> (shp          , wp + r       )
```

And that's it!  For `part1`, we want the mannhattan distance of the ship's
final position (the second item in the tuple), and for part2, we want the
manhattan distance of the ship's final position (the first item in the tuple).

```haskell
mannDist :: Point -> Int
mannDist (V2 x y) = abs x + abs y
````


*[Back to all reflections for 2020][reflections]*

## Day 12 Benchmarks

```
>> Day 12a
benchmarking...
time                 3.561 μs   (3.512 μs .. 3.619 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 3.604 μs   (3.571 μs .. 3.631 μs)
std dev              111.7 ns   (95.06 ns .. 130.5 ns)
variance introduced by outliers: 40% (moderately inflated)

* parsing and formatting times excluded

>> Day 12b
benchmarking...
time                 10.26 μs   (9.800 μs .. 10.94 μs)
                     0.984 R²   (0.976 R² .. 0.995 R²)
mean                 10.55 μs   (10.22 μs .. 10.87 μs)
std dev              1.077 μs   (784.8 ns .. 1.359 μs)
variance introduced by outliers: 86% (severely inflated)

* parsing and formatting times excluded
```

