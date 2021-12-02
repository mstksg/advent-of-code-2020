Day 18
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day18.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *18* / *[19][day19]* / *[20][day20]* / *[21][day21]* / *[22][day22]* / *[23][day23]* / *[24][day24]* / *[25][day25]*

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
[day19]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day19.md
[day20]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day20.md
[day21]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day21.md
[day22]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day22.md
[day23]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day23.md
[day24]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day24.md
[day25]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day25.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d18p]* / *[Code][d18g]* / *[Rendered][d18h]*

[d18p]: https://adventofcode.com/2020/day/18
[d18g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day18.hs
[d18h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day18.html

Let's parse with parser combinators!

The main way I have learned how to deal with these binary-operation parsers is
to separate out the stages into a "bottom" level containing only the leaves
(here, the int literals) and parentheses, and then build up layers of
precedence one-by-one from highest to lowest.  For the first part we only have
two layers, then, since we only have one level of precedence.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PP

type Parser = P.Parsec Void String

parseBottom1 :: Parser Int
parseBottom1 = P.choice
    [ PP.decimal
    , P.between "(" ")" parseTop1  -- use -XOverloadedStrings to get parsers that match strings
    ]

parseTop1 :: Parser Int
parseTop1 = do
    leftOfOp <- parseBottom1   -- parse the left hand side of a possible binary operator
    doNext acc
  where
    doNext acc = P.choice          -- once we parse a left hand side, pick from:
      [ do " * "                        -- either it's a *
           rightOfOp <- parseBottom1    --   ... so we parse the right hand side and multiply
           doNext (acc * rightOfOp)
      , do " + "                        -- or it's a +
           rightOfOp <- parseBottom1    --   ... so we parse the right hand side and add
           doNext (acc + rightOfOp)
      , pure acc                        -- otherwise that was it, no operator
      ]
```

Remember that `leftOfOp` could either come from a leaf literal number or from a
parenthesized equation.  In the end, we get an `Int`, representing whatever
number was on the left hand side of our operator.  Then we move into `doNext`,
which continually accumulates new operations after that first `leftOfOp` parse.

If we see a `*`, we parse the right hand side, fold that into our accumulator
and repeat until we hit a dead end and yield our accumulated value; same for
`+`.

So there's this sort of "cycle" that `parseTop` defers to `parseBottom` for its
underlying things "in between" the operators, but `parseBottom` loops back up
to `parseTop` to handle what is in the parentheses.

```haskell
part1 :: String -> Maybe Int
part1 = P.parseMaybe $
          sum <$> P.many parseTop1
```

The twist for part 2 is that now we have to have another layer of precedence,
so we split things out:

```haskell
parseBottom2 :: Parser Int
parseBottom2 = P.choice
    [ PP.decimal
    , P.between "(" ")" parseTop2
    ]

parseMiddle2 :: Parser Int
parseMiddle2 = do
    leftOfOp <- parseBottom2
    doNext leftOfOp
  where
    doNext acc = P.choice
      [ do " + "
           rightOfOp <- parseBottom2
           doNext (acc + rightOfOp)
      , pure acc
      ]

parseTop2 :: Parser Int
parseTop2 = do
    leftOfOp <- parseMiddle2
    doNext leftOfOp
  where
    doNext acc = P.choice
      [ do " * "
           rightOfOp <- parseMiddle2
           doNext (acc * rightOfOp)
      , pure acc
      ]
```

So the parser dependency again is kind of interesting: `parseTop2` is built up
of chained `parseMiddle2`s, which is built up of chained `parseBottom2`, which
could loop back up with `parseTop2` if detect parentheses.

```haskell
part2 :: String -> Maybe Int
part2 = P.parseMaybe $
          sum <$> (parseTop2 `P.sepBy` P.newline)
```

Note that this chaining and looping behavior can be abstracted out --- that's
essentially what I wrote in my [cleaned up solution][d18g].  But also the
*[Control.Monad.Combinators.Expr](https://hackage.haskell.org/package/parser-combinators-1.2.1/docs/Control-Monad-Combinators-Expr.html)*
module also abstracts over this pattern, letting you specify the "layers" you
want, and it'll generate the right parser for you with the correct weaving of
dependencies like I described here.  But still, I think it's fun to see how
these things end up looking like under the hood :)


*[Back to all reflections for 2020][reflections]*

## Day 18 Benchmarks

```
>> Day 18a
benchmarking...
time                 2.824 ms   (2.691 ms .. 3.014 ms)
                     0.975 R²   (0.952 R² .. 0.998 R²)
mean                 2.748 ms   (2.703 ms .. 2.844 ms)
std dev              208.7 μs   (100.8 μs .. 383.4 μs)
variance introduced by outliers: 53% (severely inflated)

* parsing and formatting times excluded

>> Day 18b
benchmarking...
time                 2.270 ms   (2.143 ms .. 2.447 ms)
                     0.974 R²   (0.958 R² .. 0.996 R²)
mean                 2.231 ms   (2.180 ms .. 2.378 ms)
std dev              236.7 μs   (129.2 μs .. 403.0 μs)
variance introduced by outliers: 70% (severely inflated)

* parsing and formatting times excluded
```

