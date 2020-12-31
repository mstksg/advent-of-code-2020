Another nice self-contained constraint satisfaction problem, along the lines of
[Day
16](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day16.md)
:)  Actually, after solving this one, I went back and rewrote my day 16
solution in terms of a common solver function that works for both!

```haskell
-- | Given a map of @k@ to possible @a@s for that @k@, find possible
-- configurations where each @k@ is given its own unique @a@.
pickUnique :: (Ord k, Ord a) => [(k, Set a)] -> [Map k a]
pickUnique mp = flip evalStateT S.empty $ do
    fmap M.fromList . for opts . traverse $ \poss -> do
      seen <- get
      pick <- lift $ S.toList (poss `S.difference` seen)
      pick <$ modify (S.insert pick)
  where
    opts = sortOn (S.size . snd) mp
```

It uses `StateT` over list, like I described in [a constraint solving blog
post](https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html).
Basically it explores all of the possibilities of drawing from a state of
"items left-over to assign".  The state is a `Set a` of items not yet picked,
and at every step we non-deterministically `pick` an `a` out of the given `(k,
Set a)` of options that hasn't already been chosen.  We use that pick and
add that picked item to the picked item set along that branch.

We also sort by the size of the possibility set for each `k`, because starting
with smaller possibilities keeps our tree tight at the top, instead of wide ---
we can eliminate options much more quickly.

Now all we need to do is to get our information into a `[(k, Set a)]`.  In our
case, this is `[(String, Set String)]` -- with each allergen, associate a set
of possible foods they might be associated with.

We can do this by just taking an intersection of all the possibilities on each
line:

```haskell
assembleOptions
    :: (Ord k, Ord a)
    => [(Set a, Set k)] -- set of foods, set of allergens
    -> Map k (Set a)    -- each allergen with the foods they were seen with in all occurrences
assembleOptions info = M.unionsWith S.intersection $
    [ M.fromSet (const igr) alg   -- a map of allergens to all foods they were seen with in this item
    | (igr, alg) <- info
    ]
```

We generate a list of allergens to all foods they were seen with on each item,
and then `intersect` all of those foods within an allergen, so that our final
`Map k (Set a)` matches each `k` allergen with a set ofall foods that were
present in *all* of the occurrences of each allergen.


Now part 2 is basically just reading off the results of `pickUnique`

```haskell
part2 :: [(Set String, Set String)] -> Maybe [String]
part2 = fmap M.elems . listToMaybe . pickUnique . assembleOptions
```

We definitely have a nice advantage here in that the `Map String String` (the
result map of allergens to foods) already is sorted in order of allergens
(alphabetically), so no need to do anything other than just `M.elems` :)

Part 1 is definitely slightly more complicated: not only do we need to find the
allergenic foods, we have to count the occurrences of non-allergenic foods in
all the items:

```haskell
part2 :: [(Set String, Set String)] -> Maybe Int
part2 info = do
    allergenicFoods <- fmap (S.fromList . M.elems)
                     . listToMaybe
                     . pickUnique
                     . assembleOptions
                     $ info
    pure . sum $
      [ length $ filter (`S.notMember` allergenicFoods) foods
      | (foods, _) <- info
      ]
  where
    allFoodOccurrences :: [String]
    allFoodOccurrences = concatMap (S.toList . fst) info
```
