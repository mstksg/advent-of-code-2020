Another day that is fairly straightforward in Haskell, I feel!  But in other
languages that support functional approaches, it should be straightforward as
well.

The answer involves lists of groups of responses:

```haskell
import           Data.List.NonEmpty
import           Data.Set
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as S

type Response = Set Char
type Group    = NonEmpty Response

parseAnswers :: Set Char -> [Group]
parseAnswers = mapMaybe ((fmap . fmap) S.fromList . NE.nonEmpty . lines)
             . splitOn "\n\n"
```

And now we just need to decide how to aggregate each group.  For part 1, this
requires a set union between every `Response` in a `Group`:

```haskell
part1 :: [Group] -> Int
part1 = sum . map (S.size . foldr1 S.union)
```

(`foldr1` here is safe because we have a non-empty container)

And for part 2, this requires a set intersection between every `Response` in a
`Group`:


```haskell
part2 :: [Group] -> Int
part2 = sum . map (S.size . foldr1 S.intersection)
```

That's it!
