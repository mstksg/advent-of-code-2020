Today is another day where the "automatically build a memoized recursive map"
in Haskell really shines :)  It's essentially the same problem as Day 7.

For the first part, once you sort the list, you can compute the differences and
then build a frequency map

```haskell
-- | Build a frequency map
freqs :: Ord a => [a] -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

diffs :: [Int] -> [Int]
diffs xs@(_:ys) = zipWith (-) ys xs
```

```haskell
ghci> diffs [1,3,4,7]
[2,1,3]
```

And so part 1 can be done with:

```haskell
part1 :: [Int] -> Int
part1 xs = (stepFreqs M.! 1) * (stepFreqs M.! 3)
  where
    xs' = 0 : xs ++ [maximum xs + 3]
    stepFreqs = freqs (diffs (sort xs))
```


For part 2, if we get an `IntSet` of all of your numbers (and adding the zero,
and the goal, the maximum + 3), then we can use it to build our `IntMap` of all
the number of paths from a given number.

```haskell
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

-- | A map of numbers to the count of how many paths from that number to
-- the goal
pathsToGoal :: IntSet -> IntMap Int
pathsToGoal xs = res
  where
    res = flip IM.fromSet xs $ \i ->
      if i == goal
        then 1
        else sum [ IM.findWithDefault 0 (i + j) res
                 | j <- [1,2,3]
                 ]
    goal = IS.findMax is
```

Our answer is `res`, the map of numbers to the count of how many paths exist
from that number to the goal.  To generate the count for a given number `i`, we
add the number of paths from `i+1`, `i+2`, and `i+3`.  We get that count by
looking it up in `res`!

```haskell
part2 :: [Int] -> Int
part2 xs = pathsToGoal xs IM.! 0
  where
    xs' = IS.fromList (0 : xs ++ [maximum xs + 3])
```
