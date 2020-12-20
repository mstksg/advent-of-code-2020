I had originally solved this puzzle using recursive knot tying and a funky
custom Monad --- the writeup for that is [available online
here](https://github.com/mstksg/advent-of-code-2020/blob/5065aad720f6996386e9c94fbd7904a6fa9f2d9d/reflections-out/day19.md).
But after some thought and reflection, I saw that things might be a little
cleaner as a hylomorphism from
*[recursion-schemes](https://hackage.haskell.org/package/recursion-schemes)*,
so I did a rewrite based on it!  It also ended up being about 25% faster to
run, which was a nice bonus.  Note that I do have a [blog post on hylomorphisms
and recurion schemes]()
(https://blog.jle.im/entry/tries-with-recursion-schemes.html), if you'd like to
investigate more about the topic :)

The central type ("base functor") is `Rule`:

```haskell
data Rule a = Simple Char
            | Compound [[a]]
  deriving (Show, Eq, Ord, Generic, Functor)
```

A `Rule a` is either a "base" `Char` match, or it is a list of options of sequences
(a list of "or"'s of "and then"'s) of `a`.  The choice of `a` gives us
our interesting behavior.

For example, our initial ruleset from the input file is a list of `Rule Int`s:
either they are a simple `Char`, or they contain a list of options of sequences
of rule id's (`Int`).  We can load it all as an `IntMap (Rule Int)`, where each
`Rule Int` is stored under its rule ID.

Just to help us get an intuition for this type, let's look at what happens if
we want to "expand" out a rule all the way to only leaves at the end of a bunch
of nested choices and sequences.  This isn't required for the solve, but could
be pretty fun.

For that, we can use the `Fix` data type:

```haskell
newtype Fix f = Fix (f (Fix f))

type ExpandedRule = Fix Rule
```

A `Fix Rule` is infinite nested `Rule`s: it's essentially `Rule (Rule (Rule
(Rule ...)))` forever, meaning underneath each `Compound` are new rules, and at
the end of it all we only have `Leaf Char`s, and no more `Int`s.  For example,
we could represent rule 0 of

```
0: 1 2 | 3
1: 3
2: 3 3
3: "a"
```

as

```haskell
Fix $ Compound [
    [Fix $ Compoud [[Fix $ Leaf 'a']], Fix $ Compound [[Fix $ Leaf 'a', Fix $ Leaf 'a']]]
  , [Fix $ Leaf 'a']
  ]
```

But, given an `IntMap (Rule Int)` (the "unexpanded" raw rules as they are in
the input file), how do we get our `Fix Rule`?

We can use the handy `ana` function, which, given an expansion function `a ->
Rule a`, returns a `a -> Fix Rule`: It runs the `a -> Rule a` expansion
function on the "seed" `a`, and then runs it again on all the `a`s in the
result, and again, and again, etc., until there are no more `a`s to expand.

Well, in our case, our "expansion" function is `Int -> Rule Int`: "To expand an
`Int`, look it up in the `IntMap Int (RuleInt)`".  And that gives us a function
to fully expand any rule number:

```haskell
expandRule :: IntMap (Rule Int) -> Int -> ExpandedRule
expandRule rs = ana (rs IM.!)
```

Neat, huh?  That will fully expand the rule at any index by repeatedly
re-expanding it with `(rs IM.!)` until we are out of things to expand.

Okay, that's enough playing around for now...time to find our real solution :)

Note that we can "interpret" a rule to match it on a string by turning it into
a `String -> [String]`: it'll take a string and return a list of the leftovers
of every possible match.  For example, running the rules `(he)|h|q` on `"hello"`
*should* give us `["llo","ello"]`.  Then we can just see if we have any matches
that return empty leftovers.

For aid in thinking, let's imagine turning a `Fix Rule` into a `String ->
[String]`.  We can do that with the help of `cata :: (Rule a -> a) -> Fix Rule
-> a`.  The essence is that you are given a `Rule a` to use to make an `a`, in
which each `a` is "the thing itself you are trying to produce, already given to
you".  I talk about this a bit [in my recursion schemes blog
post](https://blog.jle.im/entry/tries-with-recursion-schemes.html), and the
explanation I give is "The `a` values in the `Rule` become the very things we
swore to create."

Because we want to write a `Fix Rule -> (String -> [String])`, our catamorphism
function ("algebra") is `Rule (String -> [String]) -> (String -> [String])`:

```haskell
matchAlg :: Rule (String -> [String]) -> String -> [String]
matchAlg = \case
    Simple c -> \case
      []   -> []
      d:ds -> if c == d then [ds] else []
    Compound xs -> \str ->
      concatMap (sequenceAll str) xs
  where
    -- run the String -> [String]s on an input String one after the other
    sequenceAll :: String -> [String -> [String]] -> [String]
    sequenceAll s0 fs = foldr (>=>) pure fs s0

match :: Fix Rule -> String -> [String]
match = cata matchAlg
```


We want to fail on our input string (return no matches) if we see a `Simple c`
with either an empty input string or one that doesn't match the `c`.  Then for
the `Compound` case with our `xs :: [[String -> [String]]]`, we take a choice
(`concatMap`) of all of the possible full sequences of the inner `[String ->
[String]]` sequences.

```haskell
ghci> match (Fix $ Compound [[Fix $ Leaf 'h', Fix $ Leaf 'e'], [Fix $ Leaf 'h'], [Fix $ Leaf 'q']]) "hello"
["llo", "ello"]
```

Alright, so now how do we solve the final puzzle?

It looks like we need to "generate" a `Fix Rule`, and immediately tear it down
into a `String -> [String]` to use it to match a string.  "Generate recursively
and immediately tear down recursively"...that's a hylomorphism!

```haskell
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b

-- which we use as...
hylo :: (Rule b -> b) -> (a -> Rule a) -> a -> b

-- which we use as...
hylo  :: (Rule (String -> [String]) -> (String -> [String]))
      -> (Int -> Rule Int)
      -> Int
      -> (String -> [String])
```

If we give `hylo` a way to "break down nested `Rule`s" and a way to "build up
nested `Rule`s", then it can actually iteratively expand up `Rule`s while
immediately tearing them down.  The nice thing about this is that it's
very lazy: it'll only *call* the generator function if you ever *need* the
thing during your teardown function.  Since our teardown function (the `String
-> [String]`) will terminate whenever we encounter an empty string or no
matches, `hylo` will only run the build-up function until the point that we hit
one of those conditions.  You can also think of it as running it on a `Rule
Int` where each `Int` is dynamically looked up as you need it from the rules map.

The neat thing about this is that we don't ever need `Fix` at all: it's all
built up and torn down "in-place", and we never built up any intermediate
value.  That's why I mentioned that the `Fix` part earlier was more of a
side-tangent!  But it definitely helps us understand the big picture, I feel.

Our final code (the whole of it, minus the parser) ends up being:

```haskell
data Rule a = Simple Char
            | Compound [[a]]
  deriving (Show, Eq, Ord, Generic, Functor)

matchAlg :: Rule (String -> [String]) -> String -> [String]
matchAlg = \case
    Simple c -> \case
      []   -> []
      d:ds -> if c == d then [ds] else []
    Compound xs -> \str ->
      concatMap (sequenceAll str) xs
  where
    sequenceAll s0 fs = foldr (>=>) pure fs s0

matcher :: IntMap (Rule Int) -> String -> [String]
matcher rules = hylo matchAlg (rules IM.!) 0

solver :: IntMap (Rule Int) -> [String] -> Int
solver rules = length . filter (any null . matcher rules)

part1 :: IntMap Rule -> [String] -> Int
part1 = solver

part2 :: IntMap Rule -> [String] -> Int
part2 rs = solver (extraRules <> rs)

extraRules :: IntMap (Rule Int)
extraRules = IM.fromList [
    (8 , Compound [[42],[42,8]])
  , (11, Compound [[42,31],[42,11,31]])
  ]
```
