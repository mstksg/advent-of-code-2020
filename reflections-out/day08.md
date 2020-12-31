Day 8
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day08.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *8* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[19][day19]* / *[20][day20]* / *[21][day21]* / *[22][day22]* / *[23][day23]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md
[day06]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day06.md
[day07]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md
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

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d08p]* / *[Code][d08g]* / *[Rendered][d08h]*

[d08p]: https://adventofcode.com/2020/day/8
[d08g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day08.hs
[d08h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day08.html

Nothing tooooo complicated about today's, I feel: it is another staple of
AoC --- simulating a virtual machine! :)  Only this time our program is
separate from our memory, so we don't have any actual self-modifying code.
However, my guard is up: this might turn into one of those soon in another day.

At least, there are some interesting things we can do to prepare for a
potential switch to different requirements in a later day (with the `Ixed`)
typeclass, and also a nice way to handle the perturbations in Part 2 using
`holesOf` and lens traversal composition.

My main program was a sequence of `Command`:

```haskell
data Instr = NOP | ACC | JMP

type Command = (Instr, Int)
```

But, what container should we use for these?

1.  `[Command]`: Nope, bad, literally no reason to ever use this except for
    O(1) push and pop.  The main operation here is indexing, and it's O(i) on
    the index.
2.  `Vector Command`: Very fast indexing (O(1) on the index), but very bad for
    any sort of addition of new instructions in-flight if that comes up in the
    future.  But good enough for now.
3.  `Seq Command`: Efficient indexing (O(1) on the index), and very good for
    adding new instructions to either end (or even in the middle) in-flight if
    it comes to that.
4.  `IntMap Command`: Efficient indexing (O(1) on the index), very good for
    adding new instructions to either end, and also good for a sparse program
    bank if it ever comes to that.

*Luckily*, we can get a common interface for all four of these options by using
the `Ixed` typeclass from the *lens* library, which abstracts over different
"indexable" things.  You'd get a safe index with `xs ^? ix i`.  So whenever
possible, I've written all my code to work generally over all four of these in
case I have to swap quickly in the future.

One theoretical nice container would actually be the `PointedList` data type
(one implementation is in the *[pointedlist][]* library).  This is because all
of our addressing is relative, so instead of storing a "current index", we
could just always point towards the focus of the tape, and shift the tape left
or right for `JMP`.

[pointedlist]: https://hackage.haskell.org/package/pointedlist-0.6.1/docs/Data-List-PointedList.html

However, this is kind of difficult to adapt to work in a uniform interface to
the other four types...so, goodbye theoretical nicety, sacrificed in the name
of adaptivity :'(

So for my solution I used `Vector`, which has just the API necessary without
the extra flexibility that `Seq` and `IntMap` offer, since we don't need it!
But, just know that things could be swapped at any time, thanks to the magic
(or horror, depending on your point of view) of typeclasses.

On the other hand, if we separate out the index from a fixed container, it does
make the state a lot simpler.  It means that our state is really only the
current pointer and the accumulator:

```haskell
data CState = CS { csPtr :: !Int, csAcc :: !Int }

initialCS :: CState
initialCS = CS 0 0

runCommand :: Vector Command -> CState -> Maybe CState
```

So our actual program becomes a very tight `CState -> Maybe CState` loop --
very efficient because the state is only a tuple!  That means that we can
simply chain things using `iterateMaybe` go get a list of all successive
states:

```haskell
-- | A handy utility function I keep around
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = go
  where
    go x = x : case f x of
      Nothing -> []
      Just y  -> go y

allStates :: Vector Command -> [CState]
allStates cmd = iterateMaybe (runCommand cmd) initialCS
```

So now we have a generator of all the states a given program bank will ever
output.  For part 1, we just need to find a loop.  Luckily I have another handy
utility function that scans a list and reports the first time a projection
function's result is repeated

```haskell
-- | Lazily find the first repeated projection.
firstRepeatedBy :: Ord a => (b -> a) -> [b] -> Maybe b
firstRepeatedBy f = go S.empty
  where
    go seen (x:xs)
      | f x `S.member` seen = Just x
      | otherwise           = go (f x `S.insert` seen) xs
    go _ []     = Nothing

part1 :: Vector Command -> Maybe CState
part1 cmd = firstRepititionBy csPtr states
  where
    states = iterateMaybe (runCommand cmd) inititialCS
```

Now all that's left is to actually implement `runCommand`!

```haskell
runCommand
    :: Vector Command
    -> CState
    -> Maybe CState
runCommand cmds cs = (cmds ^? ix (csPtr cs)) <&> \case
    (NOP, _) -> cs { csPtr = csPtr cs + 1 }
    (ACC, i) -> cs { csPtr = csPtr cs + 1, csAcc = csAcc cs + i }
    (JMP, i) -> cs { csPtr = csPtr cs + i }

-- note: <&> is flip fmap
```

And the nice thing about it is that if we leave off the type annotation of
`runCommand`, we actually get a really nice polymorphic type if we ask GHC what
it expects:

```haskell
runCommand
    :: (Ixed t, Index t ~ Int, IxValue t ~ (Instr, Int))
    => t
    -> CState
    -> Maybe CState
```

This is the fully polymorphic signature that you get just from using `cmds ^?
ix (csPtr cs)`.  It says that you can use this on *any* program bank `t` that's
an instance of `Ixed`, as long as its index type is `Int` and the value at that
index is a `(Instr, Int)`.  Nothing about the typeclasses here is inherently
lensy, it's just a typeclass (like any other) to abstract over common
interfaces that many types might have.  In this fully polymorphic signature, we
can use this on `Vector Command`, `[Command]`, `Seq Command`, and `IntMap
Command`, as we wish to in the future if the need comes up.

For part 2 we can take advantage of some *actual* lens/optics magic, by using
`holesOf`:

```haskell
holesOf
    :: Traversal' s a
    -> s
    -> [Pretext (->) a a s]
```

The type is definitely scary, but `holesOf` is saying:

1.  Give me a specification of which holes you want to poke (`Traversal' s a`,
    a value `s` with holes `a`)
2.  ... and an item you want to poke the holes in (`s`)
3.  ... and I'll return to you a list of continuations (`Pretext (->) a a (t
    a)`), each one allowing you to edit a different hole in `s`.

`Pretext` is a bit of a complicated type, but the main interface you would use
it with is:

```haskell
peeks :: (a -> a) -> Pretext (->) a a s -> s
```

`peeks` as for a function you would want to run on a hole (the `a -> a`), the
continuation you got from `holesOf`, and then returns the "modified" `s`,
modified according to that transformation you ran on that hole.

(thanks to *mniip* on freenode IRC for pointing out how these two work together
to me!)

Every item in the list returned by `holesOf` corresponds to a different hole,
so for example:

```haskell
ghci> map (peeks negate) (holesOf traverse [1,2,3])
  [ [-1, 2, 3]
  , [ 1,-2, 3]
  , [ 1, 2,-3]
  ]
```

The `traverse :: Traversal' [a] a` is a `Traversal` that specifies the "holes"
of a list `[a]` to be each item `a` in that list.  And so `holesOf traverse
[1,2,3]` will return three `Pretext`s: one corresponding to modifying each item
in the list individually.

`peeks negate` on each of the three items returned by `holesOf traverse
[1,2,3]` will return the modified list, each with a single hole edited by
`negate`.

In our case, instead of `negate`, we can use a `flipInstr` that flips `NOP` to
`JMP` and `JMP` to `NOP`:

```haskell
flipInstr :: Command -> Command
flipInstr = \case
    NOP -> JMP
    ACC -> ACC
    JMP -> NOP
```

And now `peeks flipInstr` will do the right thing:

```haskell
ghci> map (peeks flipInstr) (holesOf traverse [NOP,ACC,JMP,JMP])
[ [JMP,ACC,JMP,JMP]
, [NOP,ACC,JMP,JMP]
, [NOP,ACC,NOP,JMP]
, [NOP,ACC,JMP,NOP]
]
```

An extra coolio thing is that traversals compose with `.`, so we can actually
use a traversal `_1` (here, `Traversal' (a,b) a`, which says the single "hole"
in an `(a,b)` is the first item in the tuple) to be more nuanced with our hole
selection:

```haskell
ghci> map (peeks flipInstr)
        (holesOf (traverse . _1) [(NOP,1),(ACC,2),(JMP,3),(JMP,4)])
  [ [(JMP,1),(ACC,2),(JMP,3),(JMP,4)]
  , [(NOP,1),(ACC,2),(JMP,3),(JMP,4)]
  , [(NOP,1),(ACC,2),(NOP,3),(JMP,4)]
  , [(NOP,1),(ACC,2),(JMP,3),(NOP,4)]
  ]
```

Neat!

With that we can fully write `part2`: for each perturbation, check if there is
a loop.  If there is a loop, this ain't it.  If there isn't a loop, then we hit
the jackpot: return the last item in our list of seen states, as that's the
last state before termination.

```haskell
part2 :: Vector Command -> Maybe CState
part2 cmds0 = listToMaybe
    [ res
    | cmds <- peeks flipInstr <$> holesOf (traverse . _1) cmds0
    , let states = iterateMaybe (runCommand cmds) initialCS
    , res  <- case firstRepeatedBy csPtr stats of
        Nothing -> [last states]    -- loop found
        Just _  -> []               -- no loop found
    ]
```

In my actual code, I actually use the `experiment` function instead of `peeks`
-- it's like a "peeksM", in a way:

```haskell
peeks      :: (a ->   a) -> Pretext (->) a a s ->   a
experiment :: (a -> f a) -> Pretext (->) a a s -> f a
```

So instead of giving it a `Instr -> Instr`, you could give it an `Instr ->
Maybe Instr`, and "cancel out" any branches that don't need to be addressed:

```haskell
experiment :: (a -> Maybe a) -> Pretext (->) a a s -> Maybe a   -- in our case

flipInstrs :: Command -> Maybe Command
flipInstrs = \case
    NOP -> Just JMP
    ACC -> Nothing  -- for ACC indices, don't do anything
    JMP -> Just JMP
```

```haskell
ghci> map (experiment flipInstrs)
        (holesOf (traverse . _1) [(NOP,1),(ACC,2),(JMP,3),(JMP,4)])
[ Just [(JMP,1),(ACC,2),(JMP,3),(JMP,4)]
, Nothing
, Just [(NOP,1),(ACC,2),(NOP,3),(JMP,4)]
, Just [(NOP,1),(ACC,2),(JMP,3),(NOP,4)]
]
```

```haskell
part2 :: Vector Command -> Maybe CState
part2 cmds0 = listToMaybe
    [ res
    | Just cmds <- experiment flipInstr <$> holesOf (traverse . _1) cmds0
    , let states = iterateMaybe (runCommand cmds) initialCS
    , res  <- case firstRepeatedBy csPtr stats of
        Nothing -> [last states]    -- loop found
        Just _  -> []               -- no loop found
    ]
```

Not a super huge improvement, but maybe more theoretically nice because we can
skip over the possible trials where we are permuting an `ACC`.  By my
reckoning, 52% of my input file instructions were ACC instructions, so this
small thing actually shaves off a decent amount of time.


*[Back to all reflections for 2020][reflections]*

## Day 8 Benchmarks

```
>> Day 08a
benchmarking...
time                 8.055 μs   (7.625 μs .. 8.507 μs)
                     0.988 R²   (0.978 R² .. 0.996 R²)
mean                 8.153 μs   (7.856 μs .. 8.438 μs)
std dev              848.3 ns   (634.4 ns .. 1.081 μs)
variance introduced by outliers: 88% (severely inflated)

* parsing and formatting times excluded

>> Day 08b
benchmarking...
time                 2.550 ms   (2.475 ms .. 2.637 ms)
                     0.991 R²   (0.987 R² .. 0.995 R²)
mean                 2.541 ms   (2.504 ms .. 2.571 ms)
std dev              125.0 μs   (101.9 μs .. 156.3 μs)
variance introduced by outliers: 33% (moderately inflated)

* parsing and formatting times excluded
```

