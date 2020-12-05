Day 4
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day04.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *4* / *[5][day05]*

[reflections]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md
[day05]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2020

*[Prompt][d04p]* / *[Code][d04g]* / *[Rendered][d04h]*

[d04p]: https://adventofcode.com/2020/day/4
[d04g]: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day04.hs
[d04h]: https://mstksg.github.io/advent-of-code-2020/src/AOC.Challenge.Day04.html

I almost hit the leaderboard today, but hit the 1 minute timeout because I
didn't read carefully enough to treat `cid` as optional ;\_;

Ah well, that's life!

Anyway, there are a lot of great Haskell solutions out there involving parser
combinators and validation of different fields, stuff like that.  My original
solution parsed a map of fields to values, and then validated those values
according to their keys.

But taking a step back from it all, I thought it would be a nice opportunity to
try out the principal of [Parse, Don't Validate][pdv] and see if I can take it
its extremes!  And implementing this in a nice way lead me also to refinement
types with the *[refined][]* library, and also and the [higher-kinded
data][hkd] pattern, supported by  the *[barbies][]* library.

[pdv]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
[hkd]: https://reasonablypolymorphic.com/blog/higher-kinded-data/
[barbies]: https://hackage.haskell.org/package/barbies
[refined]: https://hackage.haskell.org/package/refined

So, what is "Parse, Don't Validate"?  It means: instead of parsing your data
into some structure and then checking if the structure is valid (like my
original parse-a-map-then-check-it), try instead to represent your data in a
structure where it is *imposssible* to represent or create an invalid instance
in the first place.  And so what was originally "validation" is now simply
parsing your data into that correct-by-construction structure.

This seemed like a good candidate for the *[refined][]* library, which gives us
data types that are "literally" impossible to construct unless they are in the
right shape.

```haskell
-- | (a <-> b) will represent the type of an integer between a and b
type a <-> b  = Refined (FromTo a b) Int
-- | (n ** a) will represent the type of a list of a's with exactly n elements
type n ** a   = Refined (SizeEqualTo n) [a]

-- | These come included in the library
refineThrow :: Int -> Maybe (a <-> b)
refineThrow :: [a] -> Maybe (n ** a)
```

Which gives us a good picture for the type of our "correct-by-construction"
passport:

```haskell
data Height =
    HCm (150 <-> 193)
  | HIn ( 59 <->  76)

data Eye = AMB | BLU | BRN | GRY | GRN | HZL | OTH

data Passport = Passport
    { pByr :: 1920 <-> 2002
    , pIyr :: 2010 <-> 2020
    , pEyr :: 2020 <-> 2030
    , pHgt :: Height
    , pHcl :: 6 ** (0 <-> 15)
    , pEcl :: Eye
    , pPid :: 9 ** (0 <-> 9)
    }
```

Et voila!  We now have a passport where it is impossible to construct unless
you have all the correct components!

That's great and all, but...how do we actually parse our data type into this?

One way that could work is to parse each key-value pair into a `Passport` with
all fields blank except for the field corresponding to that key-value pair, and
then combining those optional-field passports into a "certain" passport.

So we can imagine:

```haskell
data PassportMaybe = PassportMaybe
    { pByrMaybe :: Maybe (1920 <-> 2002)
    , pIyrMaybe :: Maybe (2010 <-> 2020)
    , pEyrMaybe :: Maybe (2020 <-> 2030)
    , pHgtMaybe :: Maybe Height
    , pHclMaybe :: Maybe (6 ** (0 <-> 15))
    , pEclMaybe :: Maybe Eye
    , pPidMaybe :: Maybe (9 ** (0 <-> 9))
    }
```

with an appropriate `Monoid` instance that merges known fields together, and a
function like

```haskell
fromPassportMaybe :: PassportMaybe -> Maybe Passport
```

that will only work if all the fields are `Just`.

And hey, we would also maybe like to keep a collection of all the parsers so we
can dispatch them whenever we want...

```haskell
data PassportParser = PassportParser
    { pByrParser :: String -> Maybe (1920 <-> 2002)
    , pIyrParser :: String -> Maybe (2010 <-> 2020)
    , pEyrParser :: String -> Maybe (2020 <-> 2030)
    , pHgtParser :: String -> Maybe Height
    , pHclParser :: String -> Maybe (6 ** (0 <-> 15))
    , pEclParser :: String -> Maybe Eye
    , pPidParser :: String -> Maybe (9 ** (0 <-> 9))
    }
```

And wait a minute ... doesn't part 1 require us to create a passport *without*
validating the strings?  So we also need to create

```haskell
data PassportRaw = PassportRaw
    { pByrRaw :: String
    , pIyrRaw :: String
    , pEyrRaw :: String
    , pHgtRaw :: String
    , pHclRaw :: String
    , pEclRaw :: String
    , pPidRaw :: String
    }
```

And also

```haskell
data PassportRawMaybe = PassportRawMaybe
    { pByrRaw :: Maybe String
    , pIyrRaw :: Maybe String
    , pEyrRaw :: Maybe String
    , pHgtRaw :: Maybe String
    , pHclRaw :: Maybe String
    , pEclRaw :: Maybe String
    , pPidRaw :: Maybe String
    }
```

as well, for the accumulation part?  Wow, this sounds like a horrible idea!

Or...does it?  What if we try the old [higher-kinded data][hkd] trick?

```haskell
data Passport f = Passport
    { pByr :: f (1920 <-> 2002)
    , pIyr :: f (2010 <-> 2020)
    , pEyr :: f (2020 <-> 2030)
    , pHgt :: f Height
    , pHcl :: f (6 ** (0 <-> 15))
    , pEcl :: f Eye
    , pPid :: f (9 ** (0 <-> 9))
    }
  deriving (Generic)
```

Neat, huh?  We now have a flexible data type that can account for all usage
patterns!  For example:

```haskell
-- | the original
type FullPassport = Passport Identity

-- | the optional-field
type PassportMaybe = Passport Maybe

-- | the parser collection
newtype Parser a = Parser { runParser :: String -> Maybe a }
type PassportParser = Passport Parser

-- | the raw strings
newtype Const w a = Const { getConst :: w }
type PassportRaw = Passport (Const String)

 -- | the optional raw strings
type PassportRaw = Passport (Const (Maybe String))
```

We get all of our original desired types, all from a single type definition, by
swapping out the functor `f` we use!  And then we can just use the
*[barbies][]* library to convert between the different formats.  Neat!

Well, what are we waiting for?

First, let's derive all of the instances necessary for our parsing to work,
given by the *barbies* and *one-liner-instances* packages.

```haskell
instance FunctorB Passport
instance ApplicativeB Passport
instance TraversableB Passport
instance ConstraintsB Passport
deriving via GMonoid (Passport f) instance AllBF Semigroup f Passport => Semigroup (Passport f)
deriving via GMonoid (Passport f) instance AllBF Monoid f Passport => Monoid (Passport f)
deriving instance AllBF Show f Passport => Show (Passport f)
```

Now we can write our parsers:

```haskell
newtype Parser a = Parser { runParser :: String -> Maybe a }

passportParser :: Passport Parser
passportParser = Passport
    { pByr = Parser $ refineThrow <=< readMaybe
    , pIyr = Parser $ refineThrow <=< readMaybe
    , pEyr = Parser $ refineThrow <=< readMaybe
    , pHgt = Parser $ \str ->
                let (x, u) = span isDigit str
                in  case u of
                      "cm" -> fmap HCm . refineThrow =<< readMaybe x
                      "in" -> fmap HIn . refineThrow =<< readMaybe x
                      _    -> Nothing
    , pHcl = Parser $ \case
                '#':n -> refineThrow =<< traverse readHex n
                _     -> Nothing
    , pEcl = Parser $ readMaybe . map toUpper
    , pPid = Parser $ refineThrow <=< traverse (refineThrow <=< readMaybe . (:[]))
    }
  where
    readHex c
      | isHexDigit c = refineThrow (digitToInt c)
      | otherwise    = Nothing
```

The usage of `refineThrow` means that we use the machinery already defined in
the *[refined][]* library to automatically check that our data is within the
given ranges...no need for manual range checking!

Now we can load a single `key:val` token into a passport that is *empty* (all
fields are `Const Nothing`) *except for* the value at the seen key

```haskell
-- | Load a single "key:val" token into a passport
loadPassportField :: String -> Passport (Const (Maybe String))
loadPassportField str = case splitOn ":" str of
    [k,v] -> case k of
      "byr" -> mempty { pByr = Const (Just v) }
      "iyr" -> mempty { pIyr = Const (Just v) }
      "eyr" -> mempty { pEyr = Const (Just v) }
      "hgt" -> mempty { pHgt = Const (Just v) }
      "hcl" -> mempty { pHcl = Const (Just v) }
      "ecl" -> mempty { pEcl = Const (Just v) }
      "pid" -> mempty { pPid = Const (Just v) }
      _     -> mempty
    _     -> mempty
```

```haskell
ghci> loadPassportField "eyr:1234"
Passport
  { pByr = Const Nothing
  , pIyr = Const Nothing
  , pEyr = Const (Just "1234")
  , pHgt = Const Nothing
  , pHcl = Const Nothing
  , pEcl = Const Nothing
  , pPid = Const Nothing
  }
```

Now we can parse a field in its entirety by using `bzipWith` (from *barbies*),
to "zip together" a `Passport Parser` and `Passport (Const (Maybe String))`
with a given function that tells how to merge the values in any two fields.

```haskell
parsePassportField :: String -> Passport Maybe
parsePassportField = bzipWith go passportParser . loadPassportField
  where
    go p (Const x) = runParser p =<< x
```

In the above, `go` is run between each matching field in the `Passport Parser`
and the `Passport (Const (Maybe String))`, and the overall effect is that each
string is run with the appropriate parser for its field.

```haskell
ghci> parsePassportField "eyr:2025"
Passport
  { pByr = Nothing
  , pIyr = Nothing
  , pEyr = Just (refined 2025)
  , pHgt = Nothing
  , pHcl = Nothing
  , pEcl = Nothing
  , pPid = Nothing
  }
ghci> parsePassportField "eyr:2050"
Passport
  { pByr = Nothing
  , pIyr = Nothing
  , pEyr = Nothing
  , pHgt = Nothing
  , pHcl = Nothing
  , pEcl = Nothing
  , pPid = Nothing
  }
```

And the way the `Monoid` instance works, we can just combine two `Passport
Maybe`s with `<>`:

```haskell
ghci> parsePassportField "eyr:2025" <> parsePassportField "ecl:brn"
Passport
  { pByr = Nothing
  , pIyr = Nothing
  , pEyr = Just (refined 2025)
  , pHgt = Nothing
  , pHcl = Nothing
  , pEcl = Just BRN
  , pPid = Nothing
  }
```

Which gives us a nice function to parse a whole passport, with the help of
`btraverse` to flip a `Passport Maybe` into a `Maybe (Passport Identity)`

```haskell
parsePassport :: String -> Maybe (Passport Identity)
parsePassport = btraverse (fmap Identity)
              . foldMap parsePassportField
              . words
```

The result of `foldMap parsePassportField . words` is a `Passport Maybe`, and
`btraverse` "pulls out" all of the `Just` fields and returns a `Passport
Identity` if all of the fields are `Just`, failing with `Nothing` if any of the
fields are `Nothing`.

And...that's it for part 2!

```haskell
-- | Get a list of all valid passports.
part2 :: String -> [Passport Identity]
part2 = mapMaybe parsePassport . splitOn "\n\n"
```

This works because we know that if we have a `Passport Identity`, we *know* it
has to be a valid passport.  It's physically impossible to create one that
isn't valid!

**All hail "Parse, Don't Validate"!**

And part 1 is a fun diversion: instead of a `Passport Identity`, we want to
parse into a `Passport (Const String)` instead.  The mechanics are pretty much
the same:

```haskell
loadPassport :: String -> Maybe (Passport (Const String))
loadPassport = btraverse (\(Const x) -> Const <$> x)
             . foldMap loadPassportField
             . words
```

The result of `foldMap loadPassportField` is a `Passport (Const (Maybe
String))`, and so `btraverse` will pull out all the `Just`s again, returning a
`Passport (Const String)` and failing if any of those values were `Nothing`s.
Note the sliiight abuse of the `Monoid` instance for `Maybe`, which combines
strings by concatenation.  But we're more concerned about whether or not it is
present than the actual contents of the string.

Anyway, here's wonderwall.

```haskell
-- | Get a list of all complete passports field string values.
part1 :: String -> [Passport (Const String)]
part1 = mapMaybe loadPassport . splitOn "\n\n"
```


*[Back to all reflections for 2020][reflections]*

## Day 4 Benchmarks

```
>> Day 04a
benchmarking...
time                 1.718 ms   (1.670 ms .. 1.769 ms)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 1.774 ms   (1.744 ms .. 1.812 ms)
std dev              124.2 μs   (47.91 μs .. 189.7 μs)
variance introduced by outliers: 53% (severely inflated)

* parsing and formatting times excluded

>> Day 04b
benchmarking...
time                 4.531 ms   (4.218 ms .. 4.829 ms)
                     0.973 R²   (0.959 R² .. 0.988 R²)
mean                 4.773 ms   (4.638 ms .. 4.931 ms)
std dev              425.8 μs   (346.9 μs .. 577.5 μs)
variance introduced by outliers: 56% (severely inflated)

* parsing and formatting times excluded
```

