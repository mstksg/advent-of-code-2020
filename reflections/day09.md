Let's tackle day 9!

A good way to check if a sequence of 25 numbers can add to the 26th number is
to just iterate over everything, like we might have done in day 1:

```haskell
-- | check if, for ([x,y,z] ++ [a]), no pair in xyz can add to 'a'.  If it's
-- bad, it returns 'Just a'.
isBad :: [Int] -> Maybe Int
isBad xs0 = do
    x : xs <- Just $ reverse xs0
    let badCheck = null do
          y:ys <- tails (toList xs)
          z    <- ys
          guard $ (y + z) == x
    x <$ guard badCheck
```

I use my favorite `Maybe` do-notation trick of pattern matching within the
block to take advantage of do block short circuiting for `Maybe` with its
`MonadFail` instance.  If you reverse `xs0` then you can get the last item as
the head, and the rest as the tail :)

In `badCheck` we do a list-monad powered search (see my [Day 1
Reflections](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md))
for more details on how it works.  `badCheck` will return `True` if the search
is empty (with `null`).  `guard badCheck` will be Nothing if `badCheck` fails
(and our list is good) and `Just x` if `badCheck` succeeds (and our list is
bad).

Part 1 is then just finding the first bad sequence:

```haskell
part1 :: [Int] -> Maybe Int
part1 xs = listToMaybe
    [ y
    | ys     <- tails xs
    , Just y <- [isBad (take 26 ys)]
    ]
```

For part 2, there's a nice-ish way to do it in constant-time.  First, we can
generate a cumulative sum `cumSum` for the *entire* list.  Then we know that
`sumFrom(i,j)` in our original list is just `cumSum(j) - cumSum(i)`.  This is
similar to how definite integrals work, or also how you can find the area under
a probability density function by subtracting two points from its cumulative
distribution function.

So now the problem just becomes finding `i,j` where `cumSum(j) - cumSum(i) ==
goal`.  There's a clean imperative-ish way to do this that involves just
"sliding" your window `i,j` up from `0,1`.  If `cumSum(j) - cumSum(i)` is too
small, increase `j` by 1 to open the window up a bit.  If it's too big,
increase `i` by 1 to close the window up a bit.

```haskell
findBounds :: V.Vector Int -> Int -> Maybe (Int, Int)
findBounds ns goal = go 0 1
  where
    go !i !j = do
      x <- ns V.!? i
      y <- ns V.!? j
      case compare (y - x) goal of
        LT -> go i (j + 1)
        EQ -> pure (i, j)
        GT -> go (i + 1) j
```

And there you go!

```haskell
part2 :: [Int] -> Maybe Int
part2 xs = do
    goal <- part1 xs
    let cumSum = V.fromList (scanl' (+) 0 xs)       -- cumulative sum
    (i, j) <- findBounds cumSum goal
    let xs = take (j - i) . drop i $ ns
    pure $ minimum xs + maximum xs
```

If anything, maybe the implementation of `findBounds` shows how one might
directly translate a tight mutable loop in an imperative language into a
tail-recursive function in Haskell!

We do often like to avoid explicitly writing recursive functions when we can,
but in this case I'm not sure if there's a way to get around it other than
switching to a full on mutable answer, or in a very complex way that is
extremely specific to the situation.  If you think of one, let me know! :D
