I guess today is a "here's the algorithm, now implement it" puzzle, to
contrast/take a break from yesterday's "here's the goal, figure out the
algorithm" :)

First, let's start with an intermediate data type representing the actions
possible on each line:

```haskell
data Instr =
      Mask [Maybe Bool]
    | Write Int Int
```

The mask will be a list of `Maybe Bool`, where `X` is `Nothing`, `0` is `Just
False`, and `1` is `Just True`.  However, it's important to reverse the string
when parsing it from the input, because we want index `0` to correspond to bit
`0`, index `1` to correspond to bit `1`, etc., to make our lives easier.

That's because we can implement the application of a mask (for part 1) using
[`ifoldl'`](https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Indexed.html#v:ifoldl-39-),
a version of `foldl'` that gives you an item's index as you are folding it:

```haskell
import           Data.Bits (clearBit, setBit)
import           Control.Lens.Indexed (ifoldl')

applyMask1 :: Int -> [Maybe Bool] -> Int
applyMask1 = ifoldl' $ \i x -> \case
    Nothing    -> x
    Just False -> clearBit x i
    Just True  -> setBit   x i
```

If the bit list contains a `Nothing` in a given index, leave the item
unchanged.  If it contains a `Just False`, clear that index's bit (set it to
zero).  If it contains a `Just Nothing`, set that index's bit (set it to one).

And that leaves part 1 as a foldl through all the instructions, keeping the
current map and mask as state:

```haskell
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

part1 :: [Instr] -> (IntMap Int, [Maybe Bool])
part1 = foldl' go (IM.empty, [])
  where
    go :: (IntMap Int, [Maybe Bool]) -> Instr -> (IntMap Int, [Maybe Bool])
    go (!mp, !msk) = \case
      Mask  msk'   -> (mp, msk')
      Write addr n ->
        let mp' = IM.insert addr (applyMask1 n msk) mp
        in  (mp', msk)
```

Part 2's mask application is interesting, because it lives in
"non-determinancy".  Basically, each bit mask bit application could potentially
yield multiple possibilities.  We have to accumulate every nested possibility.
This feature is given to us by list's `Monad` instance, so we can swap
`ifoldl'` for
[`ifoldM`](https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Indexed.html#v:ifoldlM):

```haskell
ifoldl' :: (Int -> b -> a ->   b) -> b -> [a] ->   b
ifoldlM :: (Int -> b -> a -> m b) -> b -> [a] -> m b
```

For `ifoldlM`, each result lives in monad `m`, so the semantics of "proceeding
along the fold" are deferred to the `Monad` instance for `m`.  If `m` is
`Maybe`, it means that you only proceed if you get a `Just`, or else
short-circuit with `Nothing`.  If `m` is `IO`, it means that proceeding
involves chaining the IO action's execution and binding the result to give it
to the function's next iteration.  If `m` is `[]` (list), it means that
subsequent chaining will run the function on every *possibility* returned by
the function's previous call, accumulating every possible way of choosing every
possible choice. (I talked about this in more depth in [one of my first ever
Haskell blog
posts](https://blog.jle.im/entries/series/+monadplus-success-failure-monads.html)).

```haskell
import           Control.Lens.Indexed (ifoldlM)

applyMask2 :: Int -> [Maybe Bool] -> [Int]
applyMask2 = ifoldlM $ \i x -> \case
    Nothing    -> [clearBit x i, setBit x i]
    Just False -> [x]
    Just True  -> [setBit x i]
```

For these, we return a list of every possible change from a given bit mask bit.
For the `Nothing` "floating" case, there are two possibilities; for the other
two, there is only one.  We trust list's `Monad` instance to properly thread
over all possible results into a list of all possible changes that that `Int`
could have been subjected to.

And so, part 2 looks a lot like part 1!

```haskell
part2 :: [Instr] -> (IntMap Int, [Maybe Bool])
part2 = foldl' go (IM.empty, [])
  where
    go :: (IntMap Int, [Maybe Bool]) -> Instr -> (IntMap Int, [Maybe Bool])
    go (!mp, !msk) = \case
      Mask  msk'   -> (mp, msk')
      Write addr n ->
        let newMp = IM.fromList ((,n) <$> applyMask2 addr msk)
        in  (newMp <> mp, msk)
```

`(<>)` here is a left-biased merger, so it merges in all of the newly seen
indices into the existing ones.
