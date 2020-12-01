So there's a simple-ish Haskell solution for these problems,

`tails` lets you separate out each item in a list with the list of items after
it:

```haskell
ghci> tails [1,2,3,4]
[1:[2,3,4], 2:[3,4], 3:[4], 4:[]]
```

```haskell
findPair :: [Int] -> Maybe Int
findPair xs = listToMaybe $ do
    x:ys <- tails xs
    y    <- ys
    guard (x + y == 2020)
    pure (x*y)

findTriple :: [Int] -> Maybe Int
findTriple xs = listToMaybe $ do
    x:ys <- tails xs
    y:zs <- tails ys
    z    <- zs
    guard (x + y + z == 2020)
    pure (x*y*z)
```

But this method is a little bit "extra", since we actually don't need to search
all of `ys` for the proper sum...if we pick `x` as `500`, then we really only
need to check if `1520` is a part of `ys`.

So we really only need to check for set inclusion:

```haskell
import qualified Data.IntSet as IS

findPair :: Int -> IS.IntSet -> Maybe Int
findPair goal xs = listToMaybe $ do
    x <- IS.toList xs
    let y = goal - x
    guard (y `IS.member` xs)
    pure (x * y)
```

And our first part will be `findPair 2020`!

You could even implement `findTriple` in terms of `findPair`, using `IS.split`
to partition a set into all items smaller than and larger than a number.
Splitting is a very efficient operation on a binary search tree like `IntSet`:

```haskell
findTriple :: Int -> IS.IntSet -> Maybe Int
findTriple goal xs = listToMaybe $ do
    x <- IS.toList xs
    let (_, ys) = IS.split x xs
        goal' = goal - x
    case findPair goal' ys of
      Nothing -> empty
      Just yz -> pure (x*yz)
```

But hey...this recursive descent is kind of neat.  We could write a general
function to find any goal in any number of items!

```haskell
-- | Given a number n of items and a goal sum and a set of numbers to
-- pick from, finds the n numbers in the set that add to the goal sum.
knapsack
    :: Int              -- ^ number of items n to pick
    -> Int              -- ^ goal sum
    -> IS.IntSet        -- ^ set of options
    -> Maybe [Int]      -- ^ resulting n items that sum to the goal
knapsack 0 _    _  = Nothing
knapsack 1 goal xs
    | goal `IS.member` xs = Just [goal]
    | otherwise          = Nothing
knapsack n goal xs = listToMaybe $ do
    x <- IS.toList xs
    let goal'   = goal - x
        (_, ys) = IS.split x xs
    case knapsack (n - 1) goal' ys of
      Nothing -> empty
      Just rs -> pure (x:rs)
```

And so we have:

```haskell
part1 :: [Int] -> Maybe Int
part1 = knapsack 2 2020 . IS.fromList

part2 :: [Int] -> Maybe Int
part2 = knapsack 3 2020 . IS.fromList
```

And we could go on, and on, and on!

Definitely very unnecessary, but it does shave my time on Part 2 down from
around 2ms to around 20μs :)
