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
