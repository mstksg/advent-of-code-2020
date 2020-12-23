This one can be a fun exercise in explicit/direct tail recursion :)  It's a
straightforward implementation of an "imperative" algorithm, but we actually
gain a lot from implementing our imperative algorithm in a purely functional
setting, and can write something that runs faster than we might write in a
language with implicit mutation.  Immutability can be an optimization, since
our data structures are designed around sharing and avoiding deep clones, so
storing references and caches to old values are extremely cheap.  I explain
more about this at the end, but it's nice that we can get the advantages of
imperative programming without most of the drawbacks of implicit mutation
slowing down our code.

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
play a `game2` with the exactly-sized deck tops to determine the winner.  The
way the recursion is structured here is pretty night because there is a loop
between the two function pointers (`game2`, and the lambda passed to it), so we
can go back and forth between them without allocating new functions.

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
    go :: Set (Deck, Deck) -> Deck -> Deck -> (Player, Deck)
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
or an empty `Seq` as a pattern. Note that this isn't *perfectly*
tail-recursive -- we do get another layer of data allocated whenever we
recurse into a game.  But at least it's tail-recursive within the same game.


Note that this talk about tail recursion isn't because we are afraid of
overflowing the call stack like in other languages (and trying to take
advantage of tail-call optimization) --- the value in tail recursion is that we
can stay constant-space on the heap (since haskell function calls go on the
heap, not a call stack).

This works, but we can make it a little faster in a way that only purely
functional languages can benefit from.  Checking for seen decks in a `Set
(Deck, Deck)` can be pretty expensive in such a tight loop, and it's definitely
the bottleneck of our loop.  One quick optimization we can do is use an
`IntSet` instead of a `Set`, and store a "hash" (really, partition index) of
our data:

```haskell
hashHand ;: Deck -> Deck -> Int
hashHand xs ys = hash (take 2 (toList xs), take 2 (toList ys), length xs)
```

So instead of checking if a hand pair has been seen before, we can only check
``hashHand xs0 ys0 `IS.member` seen``, and `IS.insert (hashHand xs0 ys0) seen`
at every step.  This becomes very efficient (takes my time from 1.8s down to
8ms), effectively eliminating the main bottleneck.

However, this method is mathematically unsound because it's possible for two
different decks to "hash" to the same `Int`.  It didn't happen in my own input,
but it happened when solving the game for one of my friend's inputs.

Instead what we can do is implement "hash set", with easy negative checks, and
expensive positive checks --- but those should only happen basically once per
*game*, and not once per round.  We can store a `IntMap (Set (Deck, Deck))`:

```haskell
go :: IntMap (Set (Deck, Deck)) -> Deck -> Deck -> (Player, Deck)
go !seen !xs0 !ys0
    | collision = (P1, xs0)
    | otherwise = case (xs0, ys0) of
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
    collision = case IM.lookup (hashHand xs0 ys0) seen of
      Nothing -> False
      Just s  -> (xs0, ys0) `S.member` s
    seen' = IM.insertWith (<>) (hashHand xs0 ys0) (S.singleton (xs0, ys0)) seen
```

Note storing the `(Deck, Deck)` in our `IntMap` is very expensive if we are
using in-place mutation for our decks: we'd have to do a full copy of our
decks *every round* to store them into our set, because mutating them will
change them.  In the purely functional case, we don't have to do anything
special because no values are ever mutated --- the reference to our old data is
already there!

In addition, inserting/popping values off of a `Seq` does *not* require a full
copy: because `Seq` is internally a [finger
tree](https://en.wikipedia.org/wiki/Finger_tree) (a purely functional
persistent data structure optimized for these operations), adding a new value
does not require a full copy, but instead allocates very little because most of
your "new" tree's internal nodes are pointing at references to the original
tree.  So no copying is ever made, and storing these `Seq`s in our `IntMap` is
essentially just storing a pointer.

This is one of the nice ways which immutability can give us performance
increases!  These are always fun to highlight because there's some common
fantasy that immutability = slower, when in reality it's often an
*optimization*.
