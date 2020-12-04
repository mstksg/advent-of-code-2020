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

import           AOC.Common           (countTrue, hexDigit, decimalDigit)
import           AOC.Solver           ((:~>)(..))
import           Control.Applicative
import           Control.DeepSeq      (NFData)
import           Control.Lens
import           Control.Monad
import           Data.Char            (isDigit, isHexDigit, toUpper)
import           Data.Finite
import           Data.Foldable        (toList)
import           Data.Ix              (inRange)
import           Data.Kind
import           Data.List.Split      (splitOn, chunksOf)
import           Data.Map             (Map)
import           Data.Maybe           (mapMaybe)
import           Data.Monoid.OneLiner
import           Data.Proxy
import           Data.Set             (Set)
import           Data.Vinyl
import           Data.Word
import           Debug.Trace
import           GHC.Generics         (Generic)
import           GHC.TypeLits
import           Linear               (V3(..))
import           Refined
import           Text.Read            (readMaybe)
import qualified Barbies              as B
import qualified Control.Foldl        as F
import qualified Data.Map             as M
import qualified Data.Monoid          as Monoid
import qualified Data.Set             as S

type a <-> b = Refined (FromTo a b) Int
type n ** a  = Refined (SizeEqualTo n) [a]

data Height =
    HCm (150 <-> 193)
  | HIn ( 59 <->  76)
  deriving (Show, Read, Eq, Ord)

data Eye =
    AMB
  | BLU
  | BRN
  | GRY
  | GRN
  | HZL
  | OTH
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

loadPassportField :: String -> Passport (Const (Monoid.First String))
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

loadPassport :: String -> Maybe (Passport (Const String))
loadPassport = B.btraverse (\(Const (Monoid.First x)) -> Const <$> x)
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
