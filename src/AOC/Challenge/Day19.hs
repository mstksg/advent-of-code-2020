{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day19 (
    day19a
  , day19b
  ) where

import           AOC.Common                 (countTrue, pTok)
import           AOC.Solver                 ((:~>)(..))
import           Control.Applicative        (empty)
import           Control.DeepSeq            (NFData)
import           Control.Monad              ((>=>), guard)
import           Data.Bifunctor             (first)
import           Data.Functor.Foldable      (hylo)
import           Data.IntMap                (IntMap)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import qualified Data.IntMap                as IM
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PP

data Rule a = Simple Char
            | Compound [[a]]
  deriving (Show, Eq, Ord, Generic, Functor)

instance NFData a => NFData (Rule a)

matchRuleAlg :: Rule (String -> [String]) -> String -> [String]
matchRuleAlg = \case
    Simple c -> \case
      []   -> empty
      d:ds -> ds <$ guard (c == d)
    Compound xs -> \str ->
      concatMap (\ys -> foldr (>=>) pure ys str) xs

matcher :: IntMap (Rule Int) -> String -> [String]
matcher rules = hylo matchRuleAlg (rules IM.!) 0

solver :: IntMap (Rule Int) -> [String] -> Int
solver rules = countTrue (any null . matcher rules)

day19a :: (IntMap (Rule Int), [String]) :~> Int
day19a = MkSol
    { sParse = P.parseMaybe inputParser
    , sShow  = show
    , sSolve = Just . uncurry solver
    }

day19b :: (IntMap (Rule Int), [String]) :~> Int
day19b = MkSol
    { sParse = sParse day19a
    , sShow  = show
    , sSolve = Just . uncurry solver . first (extraRules <>)
    }

extraRules :: IntMap (Rule Int)
extraRules = IM.fromList [
    (8 , Compound [[42],[42,8]])
  , (11, Compound [[42,31],[42,11,31]])
  ]

type Parser = P.Parsec Void String

ruleParser :: Parser (Int, Rule Int)
ruleParser = do
    i <- pTok $ PP.decimal <* ":"
    r <- P.choice
      [ P.try $ Simple <$> simpleParser
      , Compound <$> compoundParser
      ]
    pure (i, r)
  where
    simpleParser   = P.between "\"" "\"" P.letterChar
    compoundParser = P.many (pTok PP.decimal) `P.sepBy` pTok "|"

inputParser :: Parser (IntMap (Rule Int), [String])
inputParser = do
    rs <- IM.fromList <$> P.many (ruleParser <* P.newline)
    P.newline
    ss <- P.many P.letterChar `P.sepBy` P.newline
    pure (rs, ss)
