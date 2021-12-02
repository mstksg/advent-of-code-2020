Day 19
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day19.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *19* / *[20][day20]* / *[21][day21]* / *[22][day22]* / *[23][day23]* / *[24][day24]* / *[25][day25]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md
[day06]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day06.md
[day07]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md
[day08]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day08.md
[day09]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day09.md
[day10]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day10.md
[day11]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day11.md
[day12]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day12.md
[day13]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day13.md
[day14]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day14.md
[day15]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day15.md
[day16]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day16.md
[day17]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md
[day18]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day18.md
[day20]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day20.md
[day21]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day21.md
[day22]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day22.md
[day23]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day23.md
[day24]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day24.md
[day25]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day25.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d19p]* / *[Code][d19g]* / *[Rendered][d19h]*

[d19p]: https://adventofcode.com/2020/day/19
[d19g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day19.hs
[d19h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day19.html

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
    [Fix $ Compoud [[Fix (Leaf 'a')]], Fix $ Compound [[Fix (Leaf 'a'), Fix (Leaf 'a')]]]
  , [Fix (Leaf 'a')]
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
expandRule :: IntMap (Rule Int) -> Int -> Fix Rule
expandRule rs = ana (rs IM.!)
```

Neat, huh?  That will fully expand the rule at any index by repeatedly
re-expanding it with `(rs IM.!)` until we are out of things to expand.

Another fun thing we can write that we could actually use for part 1 is to turn
an `Fix Rule` into a list of all possible strings to match.  We want to
write a `Fix Rule -> [String]` function by tearing down our recursive data
type, and this could be nicely expressed with a catamorphism (`cata :: (Rule a
-> a) -> Fix Rule -> a`), where we specify how to tear down a "single layer" of
our `Rule` type, and `cata` will generalize that to tear down the entire
structure.  I talk about this a bit [in my recursion schemes blog
post](https://blog.jle.im/entry/tries-with-recursion-schemes.html), and the
explanation I give is "The `a` values in the `Rule` become the very things we
swore to create." --- in this case, the `[String]`

So let's write our `Rule [String] -> [String]`:

```haskell
generateAlg :: Rule [String] -> [String]
generateAlg = \case
    Simple c   -> [[c]]                                   -- the single single-char string is created
    Compoud xs -> concatMap (fmap concat . sequence) xs   -- concat/sequence all options
```

And now `cata generateAlg` will generate all possible matches from a ruleset

```haskell
ghci> cata generateAlg
    (Fix $ Compound [[Fix (Leaf 'h'), Fix (Leaf 'e')], [Fix (Leaf 'h')], [Fix (Leaf 'q')]])
["he","h","q"]
```

Okay, that's enough playing around for now...time to find our real solution :)

Note that we can "interpret" a rule to match it on a string by turning it into
a `String -> [String]`: it'll take a string and return a list of the leftovers
of every possible match.  For example, running the rules `(he)|h|q` on `"hello"`
*should* give us `["llo","ello"]`.  Then we can just see if we have any matches
that return empty leftovers.

For aid in thinking, let's imagine turning a `Fix Rule` into a `String ->
[String]`.  We can do that with the help of `cata :: (Rule a -> a) -> Fix Rule
-> a`.  Because we want to write a `Fix Rule -> (String -> [String])`, our
catamorphism function ("algebra") is `Rule (String -> [String]) -> (String ->
[String])`:

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
ghci> match (Fix $ Compound [[Fix (Leaf 'h'), Fix (Leaf 'e')], [Fix (Leaf 'h')], [Fix (Leaf 'q')]])
                "hello"
["llo", "ello"]
```

Alright, so now how do we solve the final puzzle?

It looks like we need to "generate" a `Fix Rule`, and *immediately* tear it down
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

As a nice little bonus, we can also use `generateAlg` with a hylomorphism to
also turn an `IntMap (Rule Int)` into a list of all possible strings, which
works for part 1 but would return an infinite list for part 2.

```haskell
generateAll :: IntMap (Rule Int) -> Int -> [String]
generateAll rules = hylo generateAlg (rules IM.!) 0
```


*[Back to all reflections for 2020][reflections]*

## Day 19 Benchmarks

```
>> Day 19a
benchmarking...
time                 4.273 ms   (4.202 ms .. 4.507 ms)
                     0.990 R²   (0.965 R² .. 1.000 R²)
mean                 4.244 ms   (4.200 ms .. 4.390 ms)
std dev              220.8 μs   (54.67 μs .. 480.5 μs)
variance introduced by outliers: 30% (moderately inflated)

* parsing and formatting times excluded

>> Day 19b
benchmarking...
time                 27.13 ms   (26.34 ms .. 28.22 ms)
                     0.994 R²   (0.987 R² .. 1.000 R²)
mean                 26.20 ms   (25.94 ms .. 26.80 ms)
std dev              908.6 μs   (525.5 μs .. 1.450 ms)
variance introduced by outliers: 10% (moderately inflated)

* parsing and formatting times excluded
```

