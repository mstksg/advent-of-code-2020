Day 23 -- this one definitely stumped me a while, and it was the first one to
take me more than 24 hours!

Part 1 was straightforward enough with the circular [Pointed
List](https://hackage.haskell.org/package/pointedlist), and was pretty fun
indeed.  But the main problem with extrapolating this to part 2 was the crucial
"slow part": finding the index of the "preceding" cup.  Using a circular
pointed list makes it very fast to do things like take 3 cups and insert 3 cups
where you want them, but the tough thing is finding *where* you want to re-insert
them: if you pick up cup #3, then where is cup #2?  My circular pointed list
(and my later mutable vector based one, among other attempts) all suffered from
that same problem: re-arranging the cups is fast, but I couldn't figure out a
way to know where to place them without doing a full linear search.  And this
was tractable for 10 cups, but pretty much impossible for 1 million cups --
especially since the location of the 'preceding cup' soon became very far from
the current cup (it goes to the full 500k pretty quickly!)

In frustration, I implemented a mutable circularly linked list library...but
found the same problem: I could easily take and insert, but no easy way to find
out where the preceding cup was without doing an item-by-item traversal.

The breakthrough finally came when I thought about attaching a pointer to the
preceding cup's cell to each linked list cell --- a "backdoor" pointer that
skips across the circularly linked list.  This should be doable because the
structure of "preceding cup" is fixed -- it won't ever change, and so this
pointer should also be fixed as well as you shuffle everything over it.  I had
the visual imagery of "pulling" the three taken cups up back "through" the
backdoor pointer, and everything seemed very efficient, since the main
inefficiency (finding the preceding cup) was fixed.

Unfortunately I am not skilled enough in pointer manipulation and other
imperative programming intricacies to be able to implement this in a nice way.
So I stepped back and thought about just "reifying" this pointer structure into
an array of indices (pointers), where the addresses were indices.

Each cell would have to contain:

1.  The index of the cup to the right
2.  The index of the preceding cup

Only...#2 doesn't need to actually be a part of the cell, because it's fixed
and never mutates.  So we only need to have each cell hold #1, and use some
sort of scheme to get #2.

And then that's when it hit me --- if I simply stored Cup #1 at index 0, Cup #2
at index 1, Cup #3 at index 2, etc...then #2 is simply "the previous index"!
So in the end we only need an array of indices, where each index corresponds to
that cup.  The "preceding-cup" structure is fixed, and we only need to update
the "cup to the right" pointers!

```haskell
import           Data.Finite
import qualified Data.Vector.Mutable.Sized as MV
import qualified Data.Vector.Sized         as V

type CrabState n s = MV.MVector n s (Finite n)
```

Our data structure will be a million-sized mutable vector where index `i` stores
the index (cup number, essentially) of the cup labeled `i` (technically,
`i+1`).  We can use `Finite n` (`Finite 1000000` in our case) for our index
size because it is constrained to be between 0 and 999999, and subtracting past
0 wraps back up to 999999 like we'd want it to.

```haskell
step
    :: forall n m s. (KnownNat n, PrimMonad m, PrimState m ~ s)
    => CrabState n s
    -> Finite n       -- ^ current pointer
    -> m (Finite n)   -- ^ next pointer
step cs lab = do
    -- pull out the next three cups, and the cup fourth to the right
    (gs@[g1,_,g3],lab') <- pull3 lab

    -- update the "cup-to-the-right" of the pointer cup
    MV.write cs lab lab'

    -- find the first valid "preceding cup"
    let target = until (`notElem` gs) (subtract 1) (lab - 1)

    -- what cup is to the right of the target cup?
    aftertarg <- MV.read cs target

    -- pointer shuffling: the target cup should point to the pulled cups
    MV.write cs target g1
    -- .. and the final pulled cup should point to where the target cup pointed to originally
    MV.write cs g3 aftertarg

    pure lab'
  where
    pull3 :: Finite n -> m ([Finite n], Finite n)
    pull3 i0 = do
      i1 <- MV.read cs i0
      i2 <- MV.read cs i1
      i3 <- MV.read cs i2
      i4 <- MV.read cs i3
      pure ([i1,i2,i3],i4)
```

Now we just need to initialize from a fully allocated vector by writing at each
index the value of the previous cell:

```haskell
initialize
    :: forall n m s. (KnownNat n, PrimMonad m, PrimState m ~ s)
    => V.Vector n (Finite n)            -- ^ vector, organized left-to-right
    -> m (Finite n, CrabState n s)      -- ^ initial pointer
initialize v0 = do
    cs <- MV.new
    for_ finites $ \i ->        -- iterate over each index
      MV.write cs (v0 V.! (i - 1)) (v0 V.! i)
    let i0 = v0 `V.index` 0
    pure (i0, cs)
```

And now a function to mutate our crab state a given number of points, from an
initial pointer index:

```haskell
run :: (KnownNat n, PrimMonad m, PrimState m ~ s)
    => Int                  -- ^ number of steps
    -> Finite n             -- ^ initial index
    -> CrabState n s
    -> m ()
run n i0 cs = go 0 i0
  where
    go m i
      | m == n    = pure ()
      | otherwise = go (m + 1) =<< step cs i
```

And maybe some functions to read out the actual answers:

```haskell
numbersFrom1
    :: Int                  -- ^ how many numbers to pull
    -> CrabState n s
    -> m [Finite n]
numbersFrom1 n cs = go 0 0
  where
    go m i
      | m == n    = pure []
      | otherwise = do
          nxt <- MV.read cs i
          (nxt:) <$> go (m+1) nxt
```

And we have our full pipeline, remembering that we have to subtract 1 to get
the index of a cup from the cup number:

```haskell
part1 :: [Int] -> [Int]
part1 cs0 = runST $ do
    cs <- initialize v0
    run 100 0 cs
    (+ 1) . fromIntegral <$> numbersFrom1 9 cs
  where
    v0 :: V.Vector 10 (Finite 10)
    Just v0 = V.fromList $
        fromIntegral . subtract 1 <$> cs0

part2 :: [Int] -> Int
part2 cs0 = runST $ do
    cs <- initialize v0
    run 10000000 0 cs
    [x,y] <- (+ 1) . fromIntegral <$> numbersFrom1 2 cs
    pure (x * y)
  where
    v0 :: V.Vector 1000000 (Finite 1000000)
    Just v0 = V.fromList $
        (fromIntegral . subtract 1 <$> cs0)
        ++ [9..]
```


Overall, a very fun puzzle that required a bunch of interesting data structure
and representation breakthroughs to tackle :)
