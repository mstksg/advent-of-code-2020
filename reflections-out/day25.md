Day 25
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day25.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[19][day19]* / *[20][day20]* / *[21][day21]* / *[22][day22]* / *[23][day23]* / *[24][day24]* / *25*

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
[day20]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day20.md
[day21]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day21.md
[day22]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day22.md
[day23]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day23.md
[day24]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day24.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d25p]* / *[Code][d25g]* / *[Rendered][d25h]*

[d25p]: https://adventofcode.com/2020/day/25
[d25g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day25.hs
[d25h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day25.html

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


*[Back to all reflections for 2020][reflections]*

## Day 25 Benchmarks

```
>> Day 25a
benchmarking...
time                 1.321 ms   (1.274 ms .. 1.358 ms)
                     0.992 R²   (0.987 R² .. 0.996 R²)
mean                 1.289 ms   (1.263 ms .. 1.319 ms)
std dev              77.97 μs   (68.99 μs .. 92.69 μs)
variance introduced by outliers: 47% (moderately inflated)
```

