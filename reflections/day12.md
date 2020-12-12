Hello!  Today's puzzle for me ended up a neat exercise in fitting together
simple parts into something fun.

To preface this, I do usually represent all my coordinates using `V2 Int` from
the *[linear](https://hackage.haskell.org/package/linear)* library, which
supports addition and scaling:

```haskell
data V2 a = V2 !a !a

type Point = V2 Int

-- | You can add points using the Num instance
(+) :: Point -> Point -> Point

-- | You can do scaling
(*^) :: Int -> Point -> Point
```

And I have a utility type that represents a compass direction:

```haskell
data Dir = North | East | South | West

dirPoint :: Dir -> Point
dirPoint = \case
    North -> V2   0   1
    East  -> V2   1   0
    South -> V2   0 (-1)
    West  -> V2 (-1)  0

rotPoint :: Num a => Dir -> V2 a -> V2 a
rotPoint = \case
    North -> id
    East  -> \(V2 x y) -> V2   y  (-x)
    West  -> \(V2 x y) -> V2 (-y)   x
    South -> negate
```

And I do like to define a `Group` interface for my `Dir` type, just for fun.

```haskell
-- | If you consider a Dir as a turn, then `mulDir a b` is like turning a, then
-- turning b.
mulDir :: Dir -> Dir -> Dir
mulDir North = id
mulDir East  = \case North -> East
                     East  -> South
                     South -> West
                     West  -> North
mulDir South = \case North -> South
                     East  -> West
                     South -> North
                     West  -> East
mulDir West  = \case North -> West
                     East  -> North
                     South -> East
                     West  -> South

-- | '<>' is 'mulDir'.
instance Semigroup Dir where
    (<>) = mulDir

-- | If you consider Dir as a turn, then turning by North is the same as not
-- turning at all.
instance Monoid Dir where
    mempty = North

-- | Reverse a turn.  Not needed for this puzzle, but still useful in general.
instance Group Dir where
    invert = \case North -> South
                   East  -> West
                   South -> North
                   West  -> East
```

I did not write any of this for the puzzle --- this is just a nice way I like
to think about directions and points in my head :)

One major advantage of defining a `Semigroup` instance for `Dir` is that you can
take advantage of the `pow` function from
[Data.Group](https://hackage.haskell.org/package/groups-0.5.2/docs/Data-Group.html):

```haskell
pow :: Group m => m -> Int -> m
```

which is like `stimes`, but supporting negative numbers.  `pow x 3` is `x <> x
<> x`, and `pow x (-3)` is `invert x <> invert x <> invert x`, or `invert (x <>
x <> x)` (same thing, 'cause Group theory).  We don't actually need the support
for negative numbers in this puzzle, so we could just use `stimes`, but it's
nice that we can just use `pow` and not think about our input range.  And,
though it doesn't matter for this challenge, it also uses [repeated
squaring](https://en.wikipedia.org/wiki/Exponentiation_by_squaring) so it can
do these operations in log-n time (`pow x 1000000000` only takes 30
operations), which is pretty neat for a lot of different applications (like [in
my writeup for 2019 Day
22](https://blog.jle.im/entry/shuffling-things-up.html)).

Anyway I think that's enough preamble...now let's use it! :D  Each instruction
seems to be one of three forms: "go forward", "turn", or "move an absolute
vector".  So I represented these three as a data type, parameterized by the
amount to go forward, the direction to turn, and the vector to move by,
respectively.

And each first character gives us a different way to process the `Int`
argument, so I stored those instructions in a `Map`.  Then we can parse it by
just using `readMaybe :: Read a => String -> Maybe a` on a pattern match.

```haskell
data Instr = Forward Int
           | Turn Dir
           | Move Point
  deriving Show

-- | A map of a Char to the way to interpret the Int argument
mkInstr :: Map Char (Int -> Instr)
mkInstr = M.fromList
    [ ('F', Forward)
    , ('L', Turn . pow West . (`div` 90))
    , ('R', Turn . pow East . (`div` 90))
    , ('N', Move . (*^ dirPoint North))
    , ('S', Move . (*^ dirPoint South))
    , ('E', Move . (*^ dirPoint East ))
    , ('W', Move . (*^ dirPoint West ))
    ]

parseInstr :: String -> Maybe Instr
parseInstr []    = Nothing
parseInstr (c:n) = M.lookup c mkInstr <*> readMaybe n
```

```haskell
ghci> parseInstr "F30"
Forward 30
ghci> parseInstr "L270"
Turn East
ghci> parseInstr "N15"
Move (V2 0 15)
```

And now part 1, part 2 are basically just different ways of folding through a
list of instructions:

```haskell
toInstrs :: String -> [Instr]
toInstrs = traverse parseInstr . lines

-- | Use (ship heading, position) as the state
part1 :: [Instr] -> (Dir, Point)
part1 = foldl' go (East, V2 0 0)
  where
    go :: (Dir, Point) -> Instr -> (Dir, Point)
    go (!dir, !p) = \case
      Forward n -> (dir     , p + n *^ dirPoint dir)
      Turn d    -> (dir <> d, p                    )
      Move r    -> (dir     , p + r                )

-- | Use (ship position, waypoint vector from ship) as the state
part2 :: [Instr] -> (Point, Point)
part2 = foldl' go (V2 0 0, V2 10 1)
  where
    go :: (Point, Point) -> Instr -> (Point, Point)
    go (!shp, !wp) = \case
      Forward n -> (shp + n *^ wp, wp           )
      Turn d    -> (shp          , rotPoint d wp)
      Move r    -> (shp          , wp + r       )
```

And that's it!  For `part1`, we want the mannhattan distance of the ship's
final position (the second item in the tuple), and for part2, we want the
manhattan distance of the ship's final position (the first item in the tuple).

```haskell
mannDist :: Point -> Int
mannDist (V2 x y) = abs x + abs y
````
