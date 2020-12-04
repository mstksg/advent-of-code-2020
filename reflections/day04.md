I almost hit the leaderboard today, but hit the 1 minute timeout because I
didn't read carefully enough to treat `cid` as optional ;\_;

Ah, that's life!

Anyway, there are a lot of great Haskell solutions out there involving parser
combinators and validation of different fields, stuff like that.  My original
solution parsed a map of fields to values, and then validated those values
according to their keys.

But taking a step back from it all, I thought it would be a nice opportunity to
try out the principal of [Parse, Don't Validate][pdv] and see if I can take it
its extremes!  And implementing this in a nice way lead me also to refinement
types with the *[refined][]* library, and also and the [higher-kinded data][]
pattern, supported by  the *[barbies][]* library.

[pdv]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
[higher-kinded data][]: https://reasonablypolymorphic.com/blog/higher-kinded-data/
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
    , pHcl :: 6 ** (0 <~> 15)
    , pEcl :: Eye
    , pPid :: 9 ** (0 <~> 9)
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
    , pHclMaybe :: Maybe (6 ** (0 <~> 15))
    , pEclMaybe :: Maybe Eye
    , pPidMaybe :: Maybe (9 ** (0 <~> 9))
    }
```

With an appropriate `Monoid` instance that merges known fields together, and a
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
    , pHclParser :: String -> Maybe (6 ** (0 <~> 15))
    , pEclParser :: String -> Maybe Eye
    , pPidParser :: String -> Maybe (9 ** (0 <~> 9))
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

Hoo boy.

