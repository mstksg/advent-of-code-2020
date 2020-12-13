Aw man, I feel like I would have leaderboarded today had I not been busy :'(
These type of number theory problems are the ones I usually do well on.

Oh well!  Silly internet points, right?

For part 1, you just need to minimize a function on each bus ID:

```haskell
part1 :: Int -> [Int] -> (Int, Int)
part1 t0 xs = minimumBy (comparing snd)
    [ (x, waitTime)
    | x <- xs
    , let waitTime = x - (t0 `mod` x)
    ]
```

Part 2 is where things get interesting!  Let's try to think of things
inductively: start with small lists, and see how we would "add one more".

Let's say we had `(offset, id)` pairs `(0,7)` and `(1,13)`, like in the
example.  This means that we want to find times where ``t `mod` 7 == 0`` and
``(t + 1) `mod` 13 == 0``.

We can sort of do a manual search by hand to get `14` as our lowest candidate.
But also, note that `14 + (7*13)n` for any integer `n` would preserve the offset
property.  `14`, `14 + 91`, `14 + 182`, etc.  So the family of all "valid"
numbers are `14 + (7*13)n`.

Next, what if we wanted to find the situation for pairs `(0,7)`, `(1,13)`, and
`(4,15)`?  Well, we already know that any solution that includes `(0,7)` and
`(1,13)` will be of the form `14 + (7*13)n`.  So now we just need to find the
*first* one of those that also matches `(4,15)`

```haskell
-- 'until' repeatedly applies a function until it finds a value that matches a
-- predicate
ghci> until (\t -> (t + 4) `mod` 15 == 0) (+ (7*13)) 14
1106
```

Ah hah, good ol' `1106`.  Well, `1106` isn't the only number that works.
We can see that `1106 + (7*13*15)n` for any integer n would *also* work, since
it preserves that mod property.

And so, we can repeat this process over and over again for each new number we
see.

1.  Keep track of the current "lowest match" (`14`) and the current "search
    step" (`7*13`).
2.  When you see a number, search that family until you find a new lowest match
    that includes the new number.
3.  Use that new number as the next lowest match, and multiply it to get the
    new search step.
4.  Rinse and repeat.

Overall, this works pretty well as a `foldl`, where we keep this `(lowest
match, search step)` pair as an accumulator, and update it as we see each new
value in our list.

```haskell
part2 :: [(Int, Int)] -> Int
part2 = fst . foldl' go (0, 1)
  where
    go (!base, !step) (offset, i) = (base', step * i)
      where
        base' = iterateFind (\n -> (n + offset) `mod` i == 0)
                            (+ step)
                            base
```
