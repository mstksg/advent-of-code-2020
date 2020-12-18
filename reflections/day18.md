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

-- | Just a simple combinator to make a parser eat all of the spaces after it
tok :: Parser a -> Parser a
tok p = p <* P.spaces

parseBottom1 :: Parser Int
parseBottom1 = P.choice
    [ tok $ PP.decimal
    , tok $ P.between "(" ")" parseTop1  -- use -XOverloadedStrings to get parsers that match strings
    ]

parseTop1 :: Parser Int
parseTop1 = do
    leftOfOp <- parseBottom1   -- parse the left hand side of a possible binary operator
    doNext acc
  where
    doNext acc = P.choice          -- once we parse a left hand side, pick from:
      [ do tok "*"                      -- either it's a *
           rightOfOp <- parseBottom1    --   ... so we parse the right hand side and multiply
           doNext (acc * rightOfOp)
      , do tok "+"                      -- or it's a +
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
    [ tok $ PP.decimal
    , tok $ P.between "(" ")" parseTop2
    ]

parseMiddle2 :: Parser Int
parseMiddle2 = do
    leftOfOp <- parseBottom2
    doNext leftOfOp
  where
    doNext acc = P.choice
      [ do tok "+"
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
      [ do tok "*"
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
          sum <$> P.many parseTop2
```

Note that this chaining and looping behavior can be abstracted out --- that's
essentially what I wrote in my [cleaned up solution][d18g].  But also the
*[Control.Monad.Combinators.Expr](https://hackage.haskell.org/package/parser-combinators-1.2.1/docs/Control-Monad-Combinators-Expr.html)*
module also abstracts over this pattern, letting you specify the "layers" you
want, and it'll generate the right parser for you with the correct weaving of
dependencies like I described here.  But still, I think it's fun to see how
these things end up looking like under the hood :)
