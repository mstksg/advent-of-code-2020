
I'm going to reveal one of my secrets for parsing 2D ASCII maps!

```haskell
asciiGrid :: IndexedFold (Int, Int) String Char
asciiGrid = reindexed swap (lined <.> folded)
```

This gives you an indexed fold (from the *[lens][]* package) iterating over
each character in a string, indexed by `(x,y)`!

[lens]: https://hackage.haskell.org/package/lens

This lets us parse today's ASCII forest pretty easily into a `Set (Int, Int)`:

```haskell
parseForest :: String -> Set (Int, Int)
parseForest = ifoldMapOf asciiGrid $ \xy c -> case c of
    '#' -> S.singleton xy
    _   -> S.empty
```

This folds over the input string, giving us the `(x,y)` index and the character
at that index.  We accumulate with a monoid, so we can use a `Set (Int, Int)`
to collect the coordinates where the character is `'#'` and ignore all other
coordinates.

Admittedly, `Set (Int, Int)` is sliiiightly overkill, since you could probably
use `Vector (Vector Bool)` or something with `V.fromList . map (V.fromList .
(== '#')) . lines`, and check for membership with double-indexing.  But I was
bracing for something a little more demanding, like having to iterate over all
the trees or something.  Still, sparse grids are usually my go-to data
structure for Advent of Code ASCII maps.

Anyway, now we need to be able to traverse the ray.  We can write a function to
check all points in our line, given the slope (delta x and delta y):

```haskell
countLine :: Int -> Int -> Set (Int, Int) -> Int
countLine dx dy pts = length
    [ ()
    | i <- [0..322]
    , let x = (i * dx) `mod` 31
          y = i * dy
    , (x, y) `S.member` pts
    ]
```

And there we go :)

```haskell
part1 :: Set (Int, Int) -> Int
part1 = countLine 1 3

part2 :: Set (Int, Int) -> Int
part2 pts = product $
    [ countLine 1 1
    , countLine 3 1
    , countLine 5 1
    , countLine 7 1
    , countLine 1 2
    ] <*> [pts]
```

Note that this checks a lot of points we wouldn't normally need to check: any y
points out of range (322) for `dy > 1`.  We could add a minor optimization to
only check for membership if `y` is in range, but because our check is a set
lookup, it isn't too inefficient and it always returns `False` anyway.  So a
small price to pay for slightly more clean code :)
