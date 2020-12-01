Day 1 is usually a fun one to do in Haskell! :D  Today we can do something nice
with `tails`:

```haskell
ghci> tails [1,2,3,4]
[1:[2,3,4], 2:[3,4], 3:[4], 4:[]]
```

It lets you separate out each item in a list with the list of items after it.

Part 1 then becomes, with the list monad to simulate searches:

```haskell
findPair :: [Int] -> Maybe Int
findPair xs = listToMaybe $ do
    x:ys <- tails xs
    y    <- ys
    guard (x + y == 2020)
    pure (x*y)
```

And Part 2 is not much more complicated:

```haskell
findTriple :: [Int] -> Maybe Int
findTriple xs = listToMaybe $ do
    x:ys <- tails xs
    y:zs <- tails ys
    z    <- zs
    guard (x + y + z == 2020)
    pure (x*y*z)
```

The simpler way would be to just `do x <- xs; y <- xs; z <- xs; ...`, and
either hope that you don't accidentally select a duplicate, or validate that
you didn't draw any duplicates.  But it's always fun to do selecty constrainty
searches when you have the opportunity :D
