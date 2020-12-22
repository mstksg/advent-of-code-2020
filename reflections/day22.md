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
or an empty `Seq` as a pattern.

One gotcha when computing the score of a deck...remember that `sum . Seq.zipWith (*)
(Seq.fromList [1..])`, while tempting, is not going to work very well because
`Seq` is strict on its spline, and so has to build its whole internal
fingertree before returning anything.   Just remember to only use `[1..]` on
spline-lazy things like lists :)

```haskell
score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . toList
```
