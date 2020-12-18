{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day18 (
    day18a
  , day18b
  ) where

import           AOC.Solver           ((:~>)(..))
import           Control.Monad        (MonadPlus)
import           Data.Char            (digitToInt)
import           Data.Void            (Void)
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

type Parser = P.Parsec Void String

-- | A right-associative syntax
data Syntax f a = Syntax
    { sBinOps :: [f (a -> a -> a)]  -- ^ Operations at each level; highest precedence is last.
    , sPrim   :: f a                -- ^ How to parse a primitive
    , sPar    :: f a -> f a         -- ^ parentheses
    }

exprSyntax1 :: Syntax Parser Int
exprSyntax1 = Syntax
    { sBinOps = [ P.choice [ (*) <$ " * ", (+) <$ " + " ] ]  -- all same level
    , sPrim   = digitToInt <$> P.digitChar
    , sPar    = P.between "(" ")"
    }

exprSyntax2 :: Syntax Parser Int
exprSyntax2 = Syntax
    { sBinOps = [ (*) <$ " * "    -- + higher than *
                , (+) <$ " + "
                ]
    , sPrim   = digitToInt <$> P.digitChar
    , sPar    = P.between "(" ")"
    }

parseSyntax :: forall f a. MonadPlus f => Syntax f a -> f a
parseSyntax Syntax{..} = parseTopLevel
  where
    parseTopLevel :: f a
    parseTopLevel = parseLevels sBinOps
    parseLevels :: [f (a -> a -> a)] -> f a
    parseLevels = \case
      []   -> sPrim P.<|> sPar parseTopLevel
      o:os ->
        let parseDown = parseLevels os
            parseThisLevelWith x = (P.<|> pure x) $ do
              f <- o
              y <- parseDown
              parseThisLevelWith (f x y)
        in  parseDown >>= parseThisLevelWith

day18 :: (Num a, Show a) => Syntax Parser a -> String :~> a
day18 s = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = P.parseMaybe $ sum <$> (parseSyntax s `P.sepBy` P.newline)
    }
{-# INLINE day18 #-}

day18a :: String :~> Int
day18a = day18 exprSyntax1

day18b :: String :~> Int
day18b = day18 exprSyntax2
