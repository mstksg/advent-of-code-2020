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
import           Control.DeepSeq            (NFData)
import           Control.Monad              ((>=>), guard, ap)
import           Data.Bifunctor             (first)
import           Data.Functor               ((<&>))
import           Data.Maybe
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.IntMap.Strict         (IntMap)
import           Data.Map.Strict            (Map)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import qualified Data.IntMap.Strict         as IM
import qualified Data.Map.Strict            as M
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PP

data Rule = Simple Char
          | Compound (AndOr Int)
  deriving (Show, Eq, Ord, Generic)

data AndOr a = Leaf a
             | And [AndOr a]
             | Or  [AndOr a]
  deriving (Show, Eq, Ord, Generic, Functor)

instance Applicative AndOr where
    pure  = return
    (<*>) = ap
instance Monad AndOr where
    return  = Leaf
    ao >>= f = case ao of
      Leaf x -> f x
      And xs -> And $ map (>>= f) xs
      Or  xs -> Or  $ map (>>= f) xs

instance NFData Rule
instance NFData a => NFData (AndOr a)

expandRules :: IntMap Rule -> IntMap (AndOr Char)
expandRules rules = res
  where
    res = rules <&> \case
      Simple c    -> Leaf c
      Compound cs -> cs >>= (res IM.!)

-- | Returns all the leftovers after all possible matches
match :: AndOr Char -> String -> [String]
match = \case
    Leaf c -> \case
      []     -> []
      q : qs -> qs <$ guard (q == c)
    And xs -> foldr (>=>) pure (match <$> xs)
    Or  xs -> \str -> concatMap (`match` str) xs

solver :: IntMap Rule -> [String] -> Maybe Int
solver rs ss = do
    rule <- IM.lookup 0 (expandRules rs)
    pure $ countTrue (any null . match rule) ss

day19a :: (IntMap Rule, [String]) :~> Int
day19a = MkSol
    { sParse = P.parseMaybe inputParser
    , sShow  = show
    , sSolve = uncurry solver
    }

day19b :: (IntMap Rule, [String]) :~> Int
day19b = MkSol
    { sParse = sParse day19a
    , sShow  = show
    , sSolve = uncurry solver . first (extraRules <>)
    }

extraRules :: IntMap Rule
extraRules = IM.fromList [
    (8 , Compound . Or $ [
        Leaf 42
      , And [Leaf 42, Leaf 8]
      ]
    )
  , (11, Compound . Or $ [
        And [Leaf 42, Leaf 31]
      , And [Leaf 42, Leaf 11, Leaf 31]
      ]
    )
  ]
{-# NOINLINE extraRules #-}

type Parser = P.Parsec Void String

ruleParser :: Parser (Int, Rule)
ruleParser = do
    i <- pTok $ PP.decimal <* ":"
    r <- P.choice
      [ P.try $ Simple <$> (simpleParser P.<?> "simple")
      , Compound <$> (compoundParser P.<?> "compound")
      ]
    pure (i, r)
  where
    simpleParser   = P.between "\"" "\"" P.letterChar
    compoundParser = Or <$> ((And <$> P.many (Leaf <$> pTok PP.decimal)) `P.sepBy` pTok "|")

inputParser :: Parser (IntMap Rule, [String])
inputParser = do
    rs <- IM.fromList <$> P.many (ruleParser <* P.newline)
    P.newline
    ss <- P.many P.letterChar `P.sepBy` P.newline
    pure (rs, ss)


-- newtype CharTrie = Node { getCharTrie :: Map Char CharTrie }
--   deriving (Show, Eq, Ord)
-- makeBaseFunctor ''CharTrie

-- nullTrie :: CharTrie -> Bool
-- nullTrie = M.null . getCharTrie

-- buildTrie
--     :: [String]
--     -> CharTrie
-- buildTrie = ana (NodeF . preMap)
--   where
--     preMap = M.fromListWith (<>)
--            . mapMaybe (\case [] -> Nothing; c:cs -> Just (c, [cs]))

-- -- | Returns all the leftovers after all possible matches
-- matchTrie :: AndOr Char -> CharTrie -> [CharTrie]
-- matchTrie = \case
--     Leaf c -> \case
--       Node cs -> maybeToList $ M.lookup c cs
--     And xs -> foldr (>=>) pure (matchTrie <$> xs)
--     Or  xs -> \str -> concatMap (`matchTrie` str) xs

-- solverTrie :: IntMap Rule -> [String] -> Maybe Int
-- solverTrie rs ss = do
--     rule <- IM.lookup 0 (expandRules rs)
--     pure $ countTrue nullTrie $
--         matchTrie rule (buildTrie ss)


