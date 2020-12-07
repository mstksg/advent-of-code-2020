Another AoC staple, the recursive knot tying on a graph!

Here we can represent a graph as a map of vertices to other vertices, with an
edge value:

```haskell
type Graph v e = Map v (Map v e)
```

Exercise is left to the reader to parse our dataset into a `Graph String Int`,
a graph of bags to bags with `Int` edges.

Because our map has no cycles, we can take advantage of recursive knot tying to
"fold up" all children and sub-children.

For example, part 1 can be written as:

```haskell
allDescendants :: Ord v => Graph v e -> Map v (Set v)
allDescendants gr = descendantMap
  where
    descendantMap = gr <&>
      M.foldMapWithKey (\v _ -> S.insert v (M.findWithDefault S.empty v descendantMap))

-- note: (<&>) is flip fmap
```

Here we "assume" we already have a fully-featured `Map v (Set v)` map of
vertices to all their descendants, and then build `descendantMap` in terms of
it.  For every vertex `v` in the `Map v e` directly underneath a given vertex,
`v` is a descendant, and also all of `v`'s descendants (which we find by
looking things up in `descendantMap`, the map of all descendants).

Oh, um...oops, this found all the descendants, but we want all of the
ancestors.  So we have to flip the graph if we want to use this.

```haskell
flipGraph :: Ord v => Graph v e -> Graph v e
flipGraph mp = M.fromListWith M.union
    [ (m, M.singleton n e)
    | (n, ms) <- M.toList mp
    , (m, e ) <- M.toList ms
    ]

allAncestors :: Ord v => Graph v e -> Map v (Set v)
allAncestors = allDescendants . flipGraph
```

And so that leaves Part 1 as:

```haskell
part1 :: Graph String (String Int) -> Maybe (Set String)
part1 = M.lookup "shiny gold" . allAncestors
```

Part 2 we can do a similar way, by "assuming" we have a map of all vertices to
their "usage count", and looking things up to build it:

```haskell
usageCounts :: Ord v => Graph v Int -> Map v Int
usageCounts gr = usageMap
  where
    usageMap = gr <&> \neighbors -> sum
      [ n * (M.findWithDefault 0 v usageMap + 1)
      | (v, n) <- M.toList neighbors
      ]
```

So to find the total usage of each bag, we look under each `(v, Int)` pair in the
`Map v Int` underneath a given vertex, look up the usage of that `v` (by
looking it up in `usageMap`), add 1 (because the bag itself is used), and
multiply by `n`, the number of times the full contents of the bag is used.

And so Part 2 is:

```haskell
part2 :: Graph String (String Int) -> Maybe Int
part2 = M.lookup "shiny gold" . usageCounts
```

If we stare at the two implementations, we note that both are pretty much the
same overall structure: we are accumulating some sort of fold over all
descendants of a given node.  If we "outsource" this accumulation as a monoidal
one (for part 1, it's `Set` union, and for part 2, it's `Sum Int` addition), we
can needlessly hyper-generalize this to fold over any `Monoid` instance.

```haskell
-- | Recursively fold up a monoid value for each vertex and all of its
-- children's monoid values.  You can transform the value in-transit before it
-- is accumulated if you want.
foldMapGraph
    :: (Ord v, Monoid m)
    => (v -> m)         -- ^ embed the vertex
    -> (e -> m -> m)    -- ^ transform with edge before it is accumulated
    -> Graph v e
    -> Map v m
foldMapGraph f g gr = res
  where
    res = gr <&>
      M.foldMapWithKey (\s v -> f s <> foldMap (g v) (M.lookup s res))
 
allDescendants :: Ord v => Graph v e -> Map v (Set v)
allDescendants = foldMapGraph
    S.singleton     -- the node is embedded as itself
    (\_ -> id)      -- ignore the edge

usageCounts :: Ord v => Graph v Int -> Map v (Sum Int)
usageCounts = foldMapGraph
    (const 0)                   -- ignore the nodes
    (\n x -> Sum n * (x + 1))   -- the edge multiplies the accumulator plus one
```

That's the curse of Haskell, I guess?  If you write these things you can't help
but notice the common patterns, and you somehow wind up trying to figure out
the higher-order function that can abstract over them, even though you know you
don't need to :)
