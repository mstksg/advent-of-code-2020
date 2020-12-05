So, compared to yesterday's, this was decently chill :)

The main insight here probably is that the puzzle is just describing that the
seat ID's are straight up binary notation for numerals, with F/L representing
what is traditionally 0, and B/R representing what is traditionally 1.  So we
can use any of our binary parsers from the standard libraries, or we can just
directly pull it into binary.

```haskell
seatId :: String -> Int
seatId = foldl' iGuessWe'reDoingThis 0
  where
    iGuessWe'reDoingThis n = \case
      'B' -> 2*n+1
      'R' -> 2*n+1
      _   -> 2*n
```

A nice one-pass way to find the missing seat ID is to realize that if we sum
all the numbers from min to max, and sum all of our lists's seat id's, then the
difference is the missing number.  Luckily there's a nice closed-form solution
for the sum of all numbers in a given range (the sum of numbers from `a` to `b`
is ``b*(b+1)`div`2 - a*(a-1)`div`2``), so we can do all of this in a single
pass using the *[foldl][]* library

[foldl]: https://hackage.haskell.org/package/foldl

```haskell
{-# LANGUAGE ApplicativeDo #-}
import qualified Control.Foldl as F

findHole :: F.Fold Int (Maybe Int)
findHole = do
    mn <- F.minimum
    mx <- F.maximum
    sm <- F.sum
    pure $
      missingItem <$> mn <*> mx <*> pure sm
  where
    missingItem mn mx sm = totalSum - sm
      where
        totalSum = mx*(mx+1)`div`2 - mn*(mn-1)`div`2
```

A `F.Fold Int (Maybe Int)` folds a list of `Int`s into a `Maybe Int`.  You can
run it with `F.fold :: F.Fold a b -> [a] -> b`.

I really like the *foldl* library because it lets you build a complex
single-pass fold by combining multiple simple single-pass folds (like
`F.minimum`, `F.maximum`, `F.sum`) using an Applicative interface.  We need to
do a bit of wrangling with the `Maybe`s because `F.minimum` and `F.maximum`
each return `Maybe Int`.

And that's more or less it!  We can actually represent the entire thing as a
fold if we use `F.premap`, to pre-map a fold...


```haskell
F.premap                 :: (c -> a) -> F.Fold a b -> F.Fold c b

-- "pre-apply" `setId` so we fold over a string instead
F.premap seatId findHole :: F.Fold String (Maybe Int)
```

And...that's enough to do it all in a single pass!

```haskell
part1 :: [String] -> Maybe Int
part1 = F.fold $ F.premap seatId F.maximum

part2 :: [String] -> Maybe Int
part2 = F.fold $ F.premap seatId findHole
```
