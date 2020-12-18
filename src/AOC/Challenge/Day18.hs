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

import           AOC.Common           (pTok)
import           AOC.Solver           ((:~>)(..))
import           Data.Char            (digitToInt)
import           Data.Void            (Void)
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

type Parser = P.Parsec Void String

-- | A left-associative syntax
data Syntax f a = Syntax
    { sBinOps :: [[f (a -> a -> a)]] -- ^ Operations at each level; highest precedence is last.
    , sPrim   :: f a                 -- ^ How to parse a primitive
    , sPar    :: f a -> f a          -- ^ parentheses
    }

exprSyntax1 :: Syntax Parser Int
exprSyntax1 = Syntax
    { sBinOps = [ [ (*) <$ pTok "*", (+) <$ pTok "+" ] ]  -- all same level
    , sPrim   = pTok $ digitToInt <$> P.digitChar
    , sPar    = pTok . P.between ")" "("
    }

exprSyntax2 :: Syntax Parser Int
exprSyntax2 = Syntax
    { sBinOps = [ [ (*) <$ pTok "*" ]    -- + higher than *
                , [ (+) <$ pTok "+" ]
                ]
    , sPrim   = pTok $ digitToInt <$> P.digitChar
    , sPar    = pTok . P.between ")" "("
    }

parseSyntax
    :: forall e s a. (P.Stream s, Ord e)
    => Syntax (P.Parsec e s) a
    -> P.Parsec e s a
parseSyntax Syntax{..} = parseTopLevel
  where
    parseTopLevel :: P.Parsec e s a
    parseTopLevel = parseLevels sBinOps
    parseLevels :: [[P.Parsec e s (a -> a -> a)]] -> P.Parsec e s a
    parseLevels = \case
      []     -> sPrim P.<|> sPar parseTopLevel
      os:oss ->
        let parseDown      = parseLevels oss
            parseThisLevel = (P.try $ do
                x <- parseDown
                f <- P.choice os
                y <- parseThisLevel
                pure (f x y)
              ) P.<|> parseDown
        in  parseThisLevel

day18
    :: (Num a, Show a)
    => Syntax Parser a
    -> [String] :~> a
day18 s = MkSol
    { sParse = Just . map reverse . lines
    , sShow  = show
    , sSolve = fmap sum . traverse (P.parseMaybe (parseSyntax s))
    }
{-# INLINE day18 #-}

day18a :: [String] :~> Int
day18a = day18 exprSyntax1

day18b :: [String] :~> Int
day18b = day18 exprSyntax2
