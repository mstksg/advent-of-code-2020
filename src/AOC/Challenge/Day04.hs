{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Common            (hexDigit, decimalDigit)
import           AOC.Solver            ((:~>)(..))
import           Control.Applicative   (Const(..))
import           Control.Lens          (preview)
import           Control.Monad         ((<=<))
import           Data.Char             (isDigit, toUpper)
import           Data.Finite           (Finite)
import           Data.Functor.Identity (Identity(..))
import           Data.List.Split       (splitOn)
import           Data.Maybe            (mapMaybe)
import           Data.Monoid           (First(..))
import           Data.Monoid.OneLiner  (GMonoid(..))
import           GHC.Generics          (Generic)
import           Refined               (Refined, FromTo, SizeEqualTo, refineThrow)
import           Text.Read             (readMaybe)
import qualified Barbies               as B

type a <-> b  = Refined (FromTo a b) Int
type n ** a   = Refined (SizeEqualTo n) [a]
type FirstRaw = Const (First String)
type Raw      = Const String

data Height =
    HCm (150 <-> 193)
  | HIn ( 59 <->  76)
  deriving (Show, Read, Eq, Ord)

data Eye = AMB | BLU | BRN | GRY | GRN | HZL | OTH
  deriving (Show, Read, Eq, Ord, Enum)

data Passport f = Passport
    { pByr :: f (1920 <-> 2002)
    , pIyr :: f (2010 <-> 2020)
    , pEyr :: f (2020 <-> 2030)
    , pHgt :: f Height
    , pHcl :: f (6 ** Finite 16)
    , pEcl :: f Eye
    , pPid :: f (9 ** Finite 10)
    }
  deriving (Generic)

instance B.FunctorB Passport
instance B.ApplicativeB Passport
instance B.TraversableB Passport
instance B.ConstraintsB Passport
deriving instance B.AllBF Show f Passport => Show (Passport f)
deriving via GMonoid (Passport f) instance B.AllBF Semigroup f Passport => Semigroup (Passport f)
deriving via GMonoid (Passport f) instance B.AllBF Monoid f Passport => Monoid (Passport f)

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
                '#':n -> refineThrow =<< traverse (preview hexDigit) n
                _     -> Nothing
    , pEcl = Parser $ readMaybe . map toUpper
    , pPid = Parser $ refineThrow <=< traverse (preview decimalDigit)
    }

loadPassportField :: String -> Passport FirstRaw
loadPassportField str = case splitOn ":" str of
    [k,v] -> case k of
      "byr" -> mempty { pByr = Const (pure v) }
      "iyr" -> mempty { pIyr = Const (pure v) }
      "eyr" -> mempty { pEyr = Const (pure v) }
      "hgt" -> mempty { pHgt = Const (pure v) }
      "hcl" -> mempty { pHcl = Const (pure v) }
      "ecl" -> mempty { pEcl = Const (pure v) }
      "pid" -> mempty { pPid = Const (pure v) }
      _     -> mempty
    _     -> mempty

loadPassport :: String -> Maybe (Passport Raw)
loadPassport = B.btraverse (\(Const (First x)) -> Const <$> x)
             . foldMap loadPassportField
             . words

parsePassport :: String -> Maybe (Passport Identity)
parsePassport = B.btraverse (fmap Identity)
              . B.bzipWith go passportParser
            <=< loadPassport
  where
    go p (Const x) = runParser p x

day04a :: [String] :~> Int
day04a = MkSol
    { sParse = Just . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . length . mapMaybe loadPassport
    }

day04b :: [String] :~> Int
day04b = MkSol
    { sParse = Just . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . length . mapMaybe parsePassport
    }
