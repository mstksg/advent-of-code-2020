Merry Christmas everyone, it's December 25th :D

The Christmas Problem is usually supposed to be a quick and concise one, since
Eric wants people to spend the holiday with their family.  This one is a bit
obscured in the jargon, but once you sort through it, the solution ends up
being pretty tidy :)

In the end you are exponentiating the number 7 by a given number of times (the
loop count) to get the number you see.  So you're solving `7^x = <your
number>`...so that's basically a logarithm!

The *[arithmoi](https://hackage.haskell.org/package/arithmoi)* library (which I
previously used in problems like Day 13) offers a nice discrete logarithm
function, so that's really all we need to use:


```haskell
type Magic = 20201227

magicGroup :: CyclicGroup Integer Magic
Just magicGroup = cyclicGroup

primBase :: PrimitiveRoot Magic
Just primBase = isPrimitiveRoot magicGroup 7

findSecret :: Mod Magic -> Maybe Natural
findSecret = fmap (discreteLogarithm magicGroup primBase)
           . isMultElement
```

And so our final solution is just (after converting the input numbers to the
`Mod Magic` data type)...

```haskell
day25 :: Mod Magic -> Mod Magic -> Maybe Integer
day52 x y = do
    secret <- findSecret x
    pure . getVal $ y ^% secret         -- exponentiate by the loop count
```

Merry Christmas to everyone, and happy New Years too.  Thank you for reading
these reflections, and I hope they have been helpful in some way :)  Special
thanks to Eric Wastl too for such a great event as always.  Until next year!
