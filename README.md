The Haskell Advent of Code Development Environment
==================================================

This package contains the framework and executable for my [Advent of Code][aoc]
interactive development environment and runner/tester/benchmarker.  Networking
powered by *[advent-of-code-api][]* library.

You write your solutions in the `AOC.Challenge.DayXX` modules, make sure to
uncomment the exports, and everything should be good to go for interactive
running, testing, viewing of prompts --- as well as integration into the
executable.

[aoc]: https://adventofcode.com
[advent-of-code-api]: https://hackage.haskell.org/package/advent-of-code-api

Designed to only accommodate development for a single year.  If you want to
work on multiple years, you should re-fork into a new directory and
re-configure (and change the executable name).

### `:~>` type

The solutions expected in terms of a `:~>` record type:

```haskell
data a :~> b = MkSol
    { sParse :: String -> Maybe a    -- ^ parse input into an `a`
    , sSolve :: a      -> Maybe b    -- ^ solve an `a` input to a `b` solution
    , sShow  :: b      -> String     -- ^ print out the `b` solution for submission
    }
```

An `a :~> b` is a solution to a challenge expecting input of type `a` and
producing answers of type `b`.  It also packs in functions to parse a `String`
into an `a`, and functions to show a `b` as a `String` to submit as an answer.

This is meant to help mentally separate out parsing, solving, and showing,
allowing for some cleaner code and an easier time planning my solution.

Such a challenge can be "run" on string inputs by feeding the string into
`sParse`, then `sSolve`, then `sShow`:

```haskell
-- | Run a ':~>' on some input, retuning 'Maybe'
runSolution :: Challenge -> String -> Maybe String
runSolution MkSol{..} s = do
    x <- sParse s
    y <- sSolve x
    pure $ sShow y
```

In the actual library, I have `runSolution` return an `Either` so I can debug
which stage the error happened in.

`sShow` also supports `dyno_ :: Typeable a => String -> a -> a`, which is how
*special test parameters* are implemented.  For example, 2018 Day 6 involves
finding points that had a total distance of less than 10000, but for the test
input, we found the points that had a total distance of less than 32.  So,
`dyno_` allows you to write `dyno_ "limit" 10000`.  This will be `10000` when
running on test input, but will be replaced by the "limit" key that test data
is allowed to manually supply. (See [this file][7btest] for reference.)

[7btest]: https://github.com/mstksg/advent-of-code-dev/blob/master/test-data/2018/07b.txt

It is common to want to use certain common "utility" functions between
different tests.  For this, you can add them to the `AOC.Common` module, and
these will be loaded as a part of `AOC.Prelude`.

Configuration
------------

When you run the `aoc-dev` executable for the first time, it will generate a
default configuration file at `./aoc-conf.yaml`.  At the moment, the
configuration contains two fields:

1.  `session`: the session key.  Allows you to download input data, part 2
    prompts, and also submit challenges.

    Can be found by logging in on a web client and checking the cookies.  You
    can usually check these with in-browser developer tools.

2.  `year`: The year you are working on.  Note that this project is designed to
    only accommodate one "year" at a time.  If you want to work on a different
    year, you should re-fork and start in a new project directory (and change
    he executable name).  Note that you can re-use session keys between years,
    provided that they have not expired.

Interactive
-----------

The *[AOC.Run.Interactive][interactive]* module has code (powered by
*[advent-of-code-api][]*) for testing your solutions and submitting within
GHCI, so you don't have to re-compile. If you edit your solution programs, they
are automatically updated when you hit `:r` in ghci.

[interactive]: https://mstksg.github.io/advent-of-code-2018/AOC-Run-Interactive.html

```haskell
ghci> execSolution_   'day02a  -- get answer for challenge based on solution
ghci> testSolution_   'day02a  -- run solution against test suite
ghci> viewPrompt_     'day02a  -- view the prompt for a part
ghci> waitForPrompt_  'day02a  -- count down to the prompt for a part
ghci> submitSolution_ 'day02a  -- submit a solution
```

These are loaded with session key stored in the configuration file.

These identifiers (like `day02a`) need to be exported and in scope for this to
work.  If they aren't, you can manually specify the day and part, by using
`mkCS 2 Part1`, etc.

Executable
----------

Comes with test examples given in problems.  The executable is named `aoc-dev`
by default, but it is recommended that you change the name (in `package.yaml`)
based on whatever year you are attempting.

```
$ aoc-dev --help
aoc-dev - Advent of Code 2018 challenge runner

Usage: aoc-dev [-c|--config PATH] COMMAND
   Run, test, bench, challenges from Advent of Code, and view prompts.
   Available days: 1, 2, 3 (...)

Available options:
  -c,--config PATH         Path to configuration file (default: aoc-conf.yaml)
  -h,--help                Show this help text

Available commands:
  run                      Run, test, and benchmark challenges
  view                     View a prompt for a given challenge
  submit                   Test and submit answers for challenges
  test                     Alias for run --test
  bench                    Alias for run --bench
  countdown                Alias for view --countdown

$ aoc-dev run 3 b
>> Day 03b
>> [✓] 243
```

You can supply input via stdin with `--stdin`:

```
$ aoc-dev run 1 --stdin
>> Day 01a
+1
+2
+1
-3
<Ctrl+D>
[?] 1
>> Day 01b
[?] 1
```

Benchmarking is implemented using *criterion*

```
$ aoc-dev bench 2
>> Day 02a
benchmarking...
time                 1.317 ms   (1.271 ms .. 1.392 ms)
                     0.982 R²   (0.966 R² .. 0.999 R²)
mean                 1.324 ms   (1.298 ms .. 1.373 ms)
std dev              115.5 μs   (77.34 μs .. 189.0 μs)
variance introduced by outliers: 65% (severely inflated)

>> Day 02b
benchmarking...
time                 69.61 ms   (68.29 ms .. 72.09 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 69.08 ms   (68.47 ms .. 69.99 ms)
std dev              1.327 ms   (840.8 μs .. 1.835 ms)
```

Test suites run the example problems given in the puzzle description, and
outputs are colorized in ANSI terminals.

```
$ aoc-dev test 1
>> Day 01a
[✓] (3)
[✓] (3)
[✓] (0)
[✓] (-6)
[✓] Passed 4 out of 4 test(s)
[✓] 416
>> Day 01b
[✓] (2)
[✓] (0)
[✓] (10)
[✓] (5)
[✓] (14)
[✓] Passed 5 out of 5 test(s)
[✓] 56752
```

This should only work if you're running `aoc-dev` in the project directory.

**To run on actual inputs**, the executable expects inputs to be found in the
folder `data/XX.txt` in the directory you are running in.  That is, the input
for Day 7 will be expected at `data/07.txt`.

Session keys are required to download input data, "Part 2" prompts for each
challenge, and also to submit.

You can "lock in" your current answers (telling the executable that those are
the correct answers) by passing in `--lock`.  This will lock in any final
puzzle solutions encountered as the verified official answers.  Later, if you
edit or modify your solutions, they will be checked on the locked-in answers.

These are stored in `data/ans/XXpart.txt`.  That is, the target output for Day 7
(Part 2, `b`) will be expected at `data/ans/07b.txt`.  You can also manually
edit these files.

You can view prompts: (use `--countdown` to count down until a prompt is
released, and display immediately)

```
$ aoc-dev view 3 b
>> Day 03b
--- Part Two ---
----------------

Amidst the chaos, you notice that exactly one claim doesn't overlap by
even a single square inch of fabric with any other claim. If you can
somehow draw attention to it, maybe the Elves will be able to make
Santa's suit after all!

For example, in the claims above, only claim `3` is intact after all
claims are made.

*What is the ID of the only claim that doesn't overlap?*
```

You can also submit answers:

```
$ aoc-dev submit 1 a
```

Submissions will automatically run the test suite.  If any tests fail, you will
be asked to confirm submission or else abort.  The submit command will output
the result of your submission: The message from the AoC website, and whether or
not your answer was correct (or invalid or ignored).  Answers that are
confirmed correct will be locked in and saved for future testing against, in
case you change your solution.

All networking features are powered by *[advent-of-code-api][]*.

Note also that `stack test`, `stack bench`, `cabal test`, `cabal bench`, etc.
are all convenient aliases of `aoc-dev test all` and `aoc-dev bench all`.  This
can be useful continuous integration purposes.
