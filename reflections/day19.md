So this is *yet another* puzzle where recursive knot tying was useful!  This
happened this year already in [day
7](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md)
and [day
10](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day10.md)...it's
a nice progression of techniques!

Most of my solution revolves around this tree monad, `AndOr`:

```haskell
data AndOr a = Leaf a
             | And [AndOr a]
             | Or  [AndOr a]
  deriving (Show, Eq, Ord, Generic, Functor)

instance Monad AndOr where
    return  = Leaf
    ao >>= f = case ao of
      Leaf x -> f x
      And xs -> And $ map (>>= f) xs
      Or  xs -> Or  $ map (>>= f) xs
```

An `AndOr` is a nested/recursive list of and's and or's; for example, I parse a
nested rule as `AndOr Int`, so

```
1 2 | 3 4 6
```

gets parsed into

```haskell
Or [
    And [Leaf 1, Leaf 2]
  , And [Leaf 3, Leaf 4, Leaf 6]
  ]
```

And an *expanded* rule like `ab|ba` would be parsed as an `AndOr Char`:

```haskell
Or [
    And [Leaf 'a', Leaf 'b']
  , And [Leaf 'b', Leaf 'a']
  ]
```

First we can parse the rules into an `IntMap Rule`, indexed by the rule number:

```haskell
data Rule = Simple Char
          | Compound (AndOr Int)
  deriving (Show, Eq, Ord, Generic)
```

for simple rules that are a single `Char`, and a compound rule that is a
combination of `Int`s.

The knot-tying comes in when we turn the `IntMap Rule` into an `IntMap (AndOr
Char)`: the fully-expanded `AndOr Char` rule:

```haskell
expandRules :: IntMap Rule -> IntMap (AndOr Char)
expandRules rules = res
  where
    res = rules <&> \case
      Simple c    -> Leaf c
      Compound cs -> cs >>= (res IM.!)
-- again, <&> is flip fmap
```

So, the final `IntMap (AndOr Char)` comes from the rule at the original
`IntMap Rule`: if it's a `Simple` rule, the result is just that `Leaf c` simple
single-char match.  If it's a `Compond cs`, then we replace every `Int` in the
`AndOr Int` with the `AndOr Char` stored in `res` at that `Int` index.

Let's just take some time to remember what the `Monad` instance for `AndOr`
does: `(>>=) :: AndOr a -> (a -> AndOr b) -> AndOr b` will take an `AndOr a`
and replace every `Leaf (x :: a)` with the *application* of the `a -> AndOr b`
to `x :: a`.  This allows us to fully transform an `AndOr a` into an `AndOr b`
simply by telling us what to expand each `a` into.

Anyway, now we write a function to actually *run* the `AndOr Char` on a
`String` to check if it matches.  This can be written as a `AndOr Char ->
(String -> [String])`, which takes a `String` and returns the leftovers that
could be returned from each possible parse:

```haskell
match :: AndOr Char -> String -> [String]
match = \case
    Leaf c -> \case
      []     -> []
      q : qs -> qs <$ guard (q == c)
    And xs -> foldr (>=>) pure (match <$> xs)
    Or  xs -> \str -> concatMap (`match` str) xs
```

Our `And` branch will sequence each `String -> [String]` on each `AndOr Char`
in `xs`, and our `Or` branch will concat all the possible parses from each
`AndOr Char` in `xs`.

```haskell
ghci> match (And [Leaf 'h', Leaf 'e']) "hello"
["llo"]
ghci> match (Or [And [Leaf 'h', Leaf 'e'], And [Leaf 'h'], Leaf 'q']) "hello"
["llo", "ello"]
```

It's written in a way such that hitting a `Leaf` at the end of a string or at a
non-matching `Char` will kill that branch.

We know our match "succeeds" on a complete string if there is at least one
empty string in the resulting list (`any null`).

And that should be it!

```haskell
solver :: IntMap Rule -> [String] -> Maybe Int
solver rs ss = do
    rule <- IM.lookup 0 (expandRules rs)
    pure . length $ filter (any null . match rule) ss

part1 :: IntMap Rule -> [String] -> Bool
part1 = solver

part2 :: IntMap Rule -> [String] -> Bool
part2 rs = solver (extraRules <> rs)

extraRules :: IntMap Rule
extraRules = IM.fromList [
    (8 , Compound $ Or [ Leaf 42 , And [Leaf 42, Leaf 8] ])
  , (11, Compound $ Or [ And [Leaf 42, Leaf 31] , And [Leaf 42, Leaf 11, Leaf 31] ])
  ]
```

Part 2 works without any extra work because `AndOr` is lazy, so it will
recursively expand its rules forever as long as you demand it :D  In practice
we actually will always *stop* demanding items (and so things will terminate)
because the strings we are testing are finite.
