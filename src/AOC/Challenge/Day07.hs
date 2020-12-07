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

import           AOC.Common          (TokParser, parseWords)
import           AOC.Solver          ((:~>)(..))
import           Control.Applicative (many)
import           Data.Map            (Map)
import           Data.Semigroup      (Sum(..))
import           Data.Set            (Set)
import           Text.Megaparsec     (anySingle, try)
import           Text.Read           (readMaybe)
import qualified Data.Map            as M
import qualified Data.Set            as S

type Bag = (String, String)
type Graph v e = Map v (Map v e)

bagParser :: TokParser String (Bag, Map Bag Int)
bagParser = do
    nm <- bagName
    _  <- anySingle
    bs <- fmap M.fromList . many . try $ do
      Just n <- readMaybe <$> anySingle
      b      <- bagName
      pure (b, n)
    pure (nm, bs)
  where
    bagName :: TokParser String Bag
    bagName = (,) <$> anySingle <*> (anySingle <* anySingle)

flipGraph :: Ord v => Graph v e -> Graph v e
flipGraph mp = M.fromListWith M.union
    [ (m, M.singleton n e)
    | (n, ms) <- M.toList mp
    , (m, e ) <- M.toList ms
    ]

-- | Recursively fold up a monoid value for each vertex and all of its
-- children.  You can transform the value in-transit before it
-- is accumulated if you want.
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

gatherGold1 :: Ord v => Graph v e -> Map v (Set v)
gatherGold1 = foldMapGraph
    S.singleton     -- the node is embedded as itself
    (\_ -> id)      -- ignore the edge

gatherGold2 :: Ord v => Graph v Int -> Map v (Sum Int)
gatherGold2 = foldMapGraph
    (const mempty)              -- ignore the nodes
    (\n x -> Sum n * (x + 1))   -- the edge multiplies the accumulator

day07a :: Graph Bag Int :~> Int
day07a = MkSol
    { sParse = fmap M.fromList . traverse (parseWords bagParser) . lines
    , sShow  = show
    , sSolve = M.lookup ("shiny","gold")
             . fmap S.size
             . gatherGold1
             . flipGraph
    }

day07b :: Map Bag (Map Bag Int) :~> Int
day07b = MkSol
    { sParse = fmap M.fromList . traverse (parseWords bagParser) . lines
    , sShow  = show
    , sSolve = M.lookup ("shiny","gold") . fmap getSum . gatherGold2
    }
