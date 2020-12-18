Day 15
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day15.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *15* / *[16][day16]* / *[17][day17]* / *[18][day18]*

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
[day16]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day16.md
[day17]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md
[day18]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day18.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d15p]* / *[Code][d15g]* / *[Rendered][d15h]*

[d15p]: https://adventofcode.com/2020/day/15
[d15g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day15.hs
[d15h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day15.html

So it is yet another "here's the algorithm, implement it" days again!  Only the
challenge this time is...you should probably implement it to be really fast!

I don't think there is *too* much wiggle room in how to implement things here;
my original solution basically kept an `IntMap` to the last seen time of any
value, and just repeatedly looked things up and modified the (current time,
last said) tuple.

My original solution took around 70 seconds to run, and that was what I used to
submit things originally.  But let's see if we can get it down to something a
little less...perceptible :)  This reflection can be a deep dive into writing
tight, performant Haskell.

The data type we'll be using is an *[unboxed mutable
array](https://hackage.haskell.org/package/vector/docs/Data-Vector-Unboxed-Mutable.html)*.
There's a trick we can use because we have a map from integers to values, we
can just use the integer keys as the index to an array.  This is usually a bad
idea but for the fact that the keys we'll be using are bounded within a
decently small range (we won't ever say a number that is greater than 30
million), so we can definitely accumulate 30 million-item array into memory
without any major problems.  We'll also store our last-said times as `Int32` to
be a little bit more efficient since we're trying to eek out every last bit of
perf.

So overall we still keep some state: the current time and the last said item.
Since those are just integers, we can keep that as pure in memory using
`StateT` running over `ST s` (the mutable state monad, where our mutable
vectors will live).

```haskell
import           Control.Monad.ST
import           Control.Monad.State
import           GHC.Int (Int32)
import qualified Data.Vector.Unboxed.Mutable as MV

data LoopState = LS
    { lsLastSaid :: !Int
    , lsCurrTime :: !Int32
    }

sayNext
    :: MV.MVector s Int32                   -- ^ the mutable vector of last-seen times
    -> StateT (T2 Int32 Int) (ST s) ()      -- ^ an 'ST s' action with some pure (T2 Int32 Int) state
sayNext v = do
    L s i <- get                        -- get the current pure state
    lst <- MV.read v x                  -- our last said is x, so look up the last time we saw it
    MV.write v x i                      -- update the last-time-seen
    let j | lst == 0  = 0               -- we haven't seen it
          | otherwise = i - lst         -- we have seen it
    put (LS (fromIntegral j) (i + 1))   -- update last seen and current time
{-# INLINE sayNext #-}
```

We will want to INLINE this so that it gets inlined directly into our main loop
code.

Oh, let's also write a function to initialize our sequence with starting
inputs:

```haskell
saySomething
    :: MV.MVector s Int32                   -- ^ the mutable vector of last-seen times
    -> Int                                  -- ^ a number to "say"
    -> StateT (T2 Int32 Int) (ST s) ()      -- ^ an 'ST s' action with some pure (T2 Int32 Int) state
saySomething v y = do
    LS x i <- get
    MV.unsafeWrite v x i          -- write the last seen number with the right time
    put (LS y (i + 1))            -- queue up the write of the number to say
{-# INLINE saySomething #-}
```

And now we're good to go to put it all together!  We can use `whileM_` from
*[Control.Monad.Loops](https://hackage.haskell.org/package/monad-loops/docs/Control-Monad-Loops.html)*
to emulate a while loop, where our condition is whenever `lsCurrTime` reaches
the maximum value.

```haskell
-- | Returns 'True' until we need to stop
stopCond :: Int32 -> StateT (T2 Int32 Int) m Bool
stopCond n = gets $ \(LS _ i) -> i < n
{-# INLINE stopCond #-}
-- gets f = f <$> get, it maps a function on top of a get

looper :: Int -> [Int] -> Int
looper n xs = runST $ flip evalStateT (LS 0 0) $ do
    v <- MV.replicate n 0       -- initialize our vector with zeros
    traverse_ (saySomething v) xs
    whileM_ (stopCond n) (sayNext v)
    gets lsLastSaid
```

On my machine (with some minor optimizations, like using
`unsafeRead`/`unsafeWrite`), this runs in 230ms for part 2...a much more
reasonable improvement over my original 70 seconds! :)

```haskell
part1 :: [Int] -> Int
part1 = looper 2020

part2 :: [Int] -> Int
part2 = looper 30000000
```


*[Back to all reflections for 2020][reflections]*

## Day 15 Benchmarks

```
>> Day 15a
benchmarking...
time                 2.770 μs   (2.721 μs .. 2.819 μs)
                     0.993 R²   (0.987 R² .. 0.997 R²)
mean                 2.804 μs   (2.731 μs .. 2.980 μs)
std dev              410.5 ns   (218.1 ns .. 710.5 ns)
variance introduced by outliers: 94% (severely inflated)

* parsing and formatting times excluded

>> Day 15b
benchmarking...
time                 227.2 ms   (221.4 ms .. 232.6 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 231.2 ms   (228.0 ms .. 239.5 ms)
std dev              6.796 ms   (587.6 μs .. 9.527 ms)
variance introduced by outliers: 14% (moderately inflated)

* parsing and formatting times excluded
```

