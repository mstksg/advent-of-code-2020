Day 22
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day22.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[19][day19]* / *22*

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
[day19]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day19.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d22p]* / *[Code][d22g]* / *[Rendered][d22h]*

[d22p]: https://adventofcode.com/2020/day/22
[d22g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day22.hs
[d22h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day22.html

This one can be a fun exercise in explicit/direct tail recursion :)  It's a
straightforward implementation of an "imperative" algorithm, but the purity in
Haskell offers an advantage in that we don't have to worry about cloning decks
or caches --- it's given to us automatically!  We get the advantages
of imperative programming without most of the drawbacks of implicit mutation.

This problem is also a nice showcase of Haskell's standard "queue" data type,
`Seq` from
*[Data.Sequence](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html)*,
with O(1) pushing and popping from both ends.

I decided to write a function that I could use to parameterize on for both
parts.

```haskell
import           Data.Sequence              (Seq(..))
import           Data.Sequence.NonEmpty     (NESeq(..))
import qualified Data.Sequence              as Seq

type Deck   = Seq Int
type NEDeck = NESeq Int

data Player = P1 | P2

playGameWith
    :: (NEDeck -> NEDeck -> Maybe Player)       -- ^ handler
    -> Deck                                     -- ^ p1 starting deck
    -> Deck                                     -- ^ p2 starting deck
    -> (Player, Deck)                           -- ^ winner and deck
```

The handler function will let us specify how to handle the situation when both
decks are non-empty (represented by
*[Data.Sequence.NonEmpty](https://hackage.haskell.org/package/nonempty-containers/docs/Data-Sequence-NonEmpty.html)*).
If returns `Nothing`, we defer to the
higher-card-wins [War](https://en.wikipedia.org/wiki/War_(card_game)) rules,
and if it returns `Just`, we take that `Player` as the winner of that round.

For part 1, we always defer to the higher-card-wins rule, so we can ignore our
decks and return `Nothing`.

```haskell
game1 :: Deck -> Deck -> (Player, Deck)
game1 = playGameWith $ \_ _ -> Nothing
```

For part 2, we want to play a game with the tops of the decks given to us, but
only if we have enough cards.

```haskell
game2 :: Deck -> Deck -> (Player, Deck)
game2 = playGameWith $ \(x :<|| xs) (y :<|| ys) -> do
    xs' <- takeExactly x xs
    ys' <- takeExactly y ys
    pure $ fst (game2 xs' ys')

takeExactly :: Int -> Seq a -> Maybe (Seq a)
takeExactly n xs = Seq.take n xs <$ guard (Seq.length xs >= n)
```

If we don't have enough items to take exactly `x` items from `xs`, then we fail
and defer to higher-card-wins rules (and same for `y` and `ys`).  Otherwise, we
play a `game2` with the exactly-sized deck tops to determine the winner.

Now the only thing left is to actually write `playGameWith` :D  This one is not
too bad if we use a helper function to make sure things stay tail-recursive so
we don't accidentally leak space.  We also would like to make sure we keep the
*same* top-level `f` in the closure for the whole time, so that the recursive
call in `go` to `go` will go *exactly* back to its own function pointer.

```haskell
import           Data.Set (Set)
import qualified Data.Set as S

playGameWith
    :: (NEDeck -> NEDeck -> Maybe Player)       -- ^ handler
    -> Deck                                     -- ^ p1 starting deck
    -> Deck                                     -- ^ p2 starting deck
    -> (Player, Deck)                           -- ^ winner and deck
playGameWith f = go S.empty
  where
    go :: Set -> Deck -> Deck -> (Player, Deck)
    go !seen !xs0 !ys0
        | (xs0, ys0) `S.member` seen = (P1, xs0)
        | otherwise                  = case (xs0, ys0) of
            (x :<| xs, y :<| ys) ->
              let winner = case f (x :<|| xs) (y :<|| ys) of
                    Nothing -> if x > y then P1 else P2
                    Just p  -> p
              in  case winner of
                    P1 -> go seen' (xs :|> x :|> y) ys
                    P2 -> go seen' xs (ys :|> y :|> x)
            (Empty, _    ) -> (P2, ys0)
            (_    , Empty) -> (P1, xs0)
      where
        seen' = S.insert (xs0, ys0) seen
```

Most of this implementation follows the logic straightforwardly, remembering to
use `f` to give the callback a chance to "intercept" the "highest card win"
rule if it wants to.  We get a lot of mileage here out of the `:<|`, `:|>` and
`Empty` constructors for `Seq`, which allows us to match on the head and tail
or an empty `Seq` as a pattern.  Note that this isn't *perfectly*
tail-recursive -- we do get another layer deeper into the stack on every call
of `f`, when we play a "recursive game".  It's tail-recursive within the same
game, however.

It should be possible to make this completely tail recursive by keeping an
explicit stack of decks/caches, but overall I'm happy with this O(d) space ont
he depth of the game recursion, because I don't think a fully tail recursive
version would be any better space-wise!

Note that this talk about tail recursion isn't because we are afraid of
overflowing the call stack like in other languages (and trying to take
advantage of tail-call optimization) --- the value in tail recursion is that we
can stay constant-space on the heap (since haskell function calls go on the
heap, not a call stack).

One gotcha when computing the score of a deck...remember that `sum . Seq.zipWith (*)
(Seq.fromList [1..])`, while tempting, is not going to work very well because
`Seq` is strict on its spline, and so has to build its whole internal
fingertree before returning anything.   Just remember to only use `[1..]` on
spline-lazy things like lists :)

```haskell
score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . toList
```


*[Back to all reflections for 2020][reflections]*

## Day 22 Benchmarks

```
>> Day 22a
benchmarking...
time                 261.6 μs   (241.7 μs .. 281.1 μs)
                     0.975 R²   (0.963 R² .. 0.990 R²)
mean                 256.6 μs   (249.5 μs .. 262.9 μs)
std dev              25.71 μs   (20.07 μs .. 33.93 μs)
variance introduced by outliers: 78% (severely inflated)

* parsing and formatting times excluded

>> Day 22b
benchmarking...
time                 2.445 ms   (2.351 ms .. 2.525 ms)
                     0.985 R²   (0.974 R² .. 0.992 R²)
mean                 2.368 ms   (2.292 ms .. 2.435 ms)
std dev              223.2 μs   (195.5 μs .. 250.5 μs)
variance introduced by outliers: 65% (severely inflated)

* parsing and formatting times excluded
```

