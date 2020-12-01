Day 1 is usually a fun one to do in Haskell! :D  Today I used one of my more
favorite little list utility functions, `select`:

```haskell
select :: [a] -> [(a,[a])]
select = go []
  where
    go _  [] = []
    go xs (y:ys) = (y,xs++ys) : go (y:xs) ys
```

Not strictly necessary to solve this, but it returns a list of every element in
a list paired with the list *excluding* that element.  (I used it in a [blog
post][sendmoremoney] in the past).

[sendmoremoney]: https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html

```haskell
ghci> select [1,2,3]
[(1,[2,3]),(2,[1,3]),(3,[2,1])]
```

Part 1 then becomes, with the list monad to simulate searches:

```haskell
findPair :: [Int] -> Maybe Int
findPair xs = listToMaybe $ do
    (x, ys) <- select xs
    y       <- ys
    guard $ x + y == 2020
    pure (x*y)
```

And Part 2 is not much more complicated:

```haskell
findTriple :: [Int] -> Maybe Int
findTriple xs = listToMaybe $ do
    (x, ys) <- select xs
    (y, zs) <- select ys
    z       <- zs
    guard $ x + y + z == 2020
    pure (x*y*z)
```

The simpler way would be to just `do x <- xs; y <- xs; z <- xs; ...`, and
either hope that you don't accidentally select a duplicate, or validate that
you didn't draw any duplicates.  But it's always fun to do selecty constrainty
searches when you have the opportunity :D
