Neat, Game of Life! :D  Actually, the 3D/4D twist does make a big impact for
the best method we'd pick: we run into the [curse of
dimensionality](https://en.wikipedia.org/wiki/Curse_of_dimensionality).  It
means that when we get to 3D and 4D, our world will become vanishingly sparse.
In my own input, only about 4% of the 3D space ended up being active, and 2% of
my 4D space ends up being active.  This means that holding a dense vector of
all possible active points (which will be `(6+8+6)^n`) is up to 98% wasteful.
And because of the way this process works, we have to completely copy our
entire space at every iteration.

In these times, I'm happy that Haskell has a nice immutable sparse
data structure like `Set`.  Sparse being beneficial in that we can easily look up and process
only the 2% of active squares, and immutable being beneficial in that each step
already requires a full copy in any case, so immutability doesn't give us any
drawback.

First a function to get all neighbors of a point, using the `V3` type from the
*[linear](https://hackage.haskell.org/package/linear)* library, which I've used
many times already for its convenient `Num` and `Applicative` instances:

```haskell
import           Data.Set (Set)
import qualified Data.Set as S

-- from linear
data V3 a = V3 a a a
-- its Applicative instance
pure x = V3 x x x

neighbsSet :: V3 Int -> Set (V3 Int)
neighbsSet p = S.fromList
    [ p + d
    | d <- sequence (pure [-1,0,1])
    , d /= pure 0
    ]
```

Just as a reminder, `pure [0,1]` for `V3 Int` gives us `V3 [0,1] [0,1] [0,1]`,
and if we `sequence` that we get a cartesian N-product of all combinations `[V3
0 0, V3 0 0 1, V3 0 1 0, V3 0 1 1, V3 1 0 0, .. etc.]`.  We add each of those
to `p`, except for the one that is `V3 0 0 0`.

Now we can write our stepper, which takes a `Set (V3 Int)` and returns the next
`Set (V3 Int)` after applying the rules.  We can do that first by making a `Map
(V3 Int) Int`, where `Int` is the number of neighbors at a given point.  This
can be done by "exploding" every `V3 Int` in our set to a `Map (V3 Int) Int`,
a map of all its neighbors keyed to values 1, and then using `M.unionsWith (+)`
to union together all of those exploded neighbors, adding any overlapping keys.

```haskell
import           Data.Map (Map)
import qualified Data.Map as M

neighborMap :: Set (V3 Int) -> Map (V3 Int) Int
neighborMap ps = M.unionsWith (+)
    [ M.fromSet (const 1) (neighbsSet p)
    | p <- S.toList ps
    ]
```

Now to implement the rules:

```haskell
stepper
    :: Set (V3 Int)
    -> Set (V3 Int)
stepper ps = stayAlive <> comeAlive
  where
    neighborCounts = neighborMap ps
    stayAlive = M.keysSet . M.filter (\n -> n == 2 || n == 3) $
                  neighborCounts `M.restrictKeys` ps
    comeAlive = M.keysSet . M.filter (== 3) $
                  neighborCounts `M.withoutKeys`  ps
```

`stayAlive` is all of the `neighborCounts` keys that correspond to already-alive
points (``neighborCounts `M.restrictKeys` ps``), but filtered to the counts
that are 2 or 3.  `comeAlive` is all of the `neighborCounts` keys that
correspond to dead points (``neighborCounts `M.withoutKeys` ps``), but filtered
to only counts that are exactly 3.  And our result is the set union of both of
those.


So our part 1 becomes:

```haskell
part1 :: Set (V3 Int) -> Int
part1 = S.size . (!! 6) . iterate stepper
```

And for part 2...notice that all of our code actually never does anything
*specific* to `V3`!  In fact, if we leave the type signatures of `neighbsSet`
and `neighborMap` and `stepper` off, GHC will actually suggest more general
type signatures for us.

```haskell
neighbsSet
    :: (Applicative f, Num a, Ord (f a), Traversable f)
    => f a -> Set (f a)

neighborMap
    :: (Applicative f, Num a, Ord (f a), Traversable f)
    => Set (f a)
    -> Map (f a) Int

stepper
    :: (Applicative f, Num a, Ord (f a), Traversable f)
    => Set (f a)
    -> Set (f a)
```

Neat!  This means that our code *already works* for any other fixed-sized
`Vector` type with a `Num` instance.  Like, say...`V4`, also from *linear*?

```haskell
-- also from the Linear library, with all the same instances
data V4 a = V4 a a a a

part1 :: Set (V3 Int) -> Int
part1 = S.size . (!! 6) . iterate stepper

part2 :: Set (V4 Int) -> Int
part2 = S.size . (!! 6) . iterate stepper
```

And that's it --- code that should work for both parts :)
