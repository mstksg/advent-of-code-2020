-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day07 (
    day07a
  , day07b
  , bagParser
  ) where

import           AOC.Common                 (pWord, parseLines, CharParser)
import           AOC.Solver                 ((:~>)(..))
import           Control.Applicative        (many)
import           Data.Map                   (Map)
import           Data.Semigroup             (Sum(..))
import           Data.Set                   (Set)
import           Text.Megaparsec            (try)
import           Text.Megaparsec.Char       (space)
import           Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Map                   as M
import qualified Data.Set                   as S

type Bag = (String, String)
type Graph v e = Map v (Map v e)

target :: Bag
target = ("shiny", "gold")

bagParser :: CharParser (Bag, Map Bag Int)
bagParser = do
    nm <- bagName <* pWord
    bs <- fmap M.fromList . many . try $ do
      n <- decimal <* space
      b <- bagName
      pure (b, n)
    pure (nm, bs)
  where
    bagName :: CharParser Bag
    bagName = (,) <$> pWord <*> (pWord <* pWord)

flipGraph :: Ord v => Graph v e -> Graph v e
flipGraph mp = M.fromListWith M.union
    [ (m, M.singleton n e)
    | (n, ms) <- M.toList mp
    , (m, e ) <- M.toList ms
    ]

-- | Recursively fold up a monoid value for each vertex and all of its
-- children's monoid values.  You can transform the value in-transit before
-- it is accumulated if you want.
foldMapGraph
    :: (Ord v, Monoid m)
    => (v -> m)         -- ^ embed the vertex
    -> (e -> m -> m)    -- ^ transform with edge before it is accumulated
    -> Graph v e
    -> Map v m
foldMapGraph f g gr = res
  where
    res = M.foldMapWithKey (\s v -> f s <> foldMap (g v) (M.lookup s res))
       <$> gr

allDescendants :: Ord v => Graph v e -> Map v (Set v)
allDescendants = foldMapGraph
    S.singleton     -- the node is embedded as itself
    (\_ -> id)      -- ignore the edge

usageCounts :: Ord v => Graph v Int -> Map v (Sum Int)
usageCounts = foldMapGraph
    (const 0)                   -- ignore the nodes
    (\n x -> Sum n * (x + 1))   -- the edge multiplies the accumulator plus one

day07a :: Graph Bag Int :~> Int
day07a = MkSol
    { sParse = fmap M.fromList . parseLines bagParser
    , sShow  = show
    , sSolve = M.lookup target . fmap S.size . allDescendants . flipGraph
    }

day07b :: Map Bag (Map Bag Int) :~> Int
day07b = MkSol
    { sParse = fmap M.fromList . parseLines bagParser
    , sShow  = show
    , sSolve = M.lookup target . fmap getSum . usageCounts
    }
