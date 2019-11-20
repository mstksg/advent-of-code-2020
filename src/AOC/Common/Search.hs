
module AOC.Common.Search (
    aStar
  , binarySearch
  , exponentialSearch
  , binaryMinSearch
  , exponentialMinSearch
  , binaryFindMin
  , exponentialFindMin
  ) where

import           Data.Map      (Map)
import           Data.OrdPSQ   (OrdPSQ)
import qualified Data.Map      as M
import qualified Data.OrdPSQ   as Q

data AStarState n p = AS { _asClosed  :: !(Map n (Maybe n))         -- ^ map of item to "parent"
                         , _asOpen    :: !(OrdPSQ n p (p, Maybe n))    -- ^ map of item to "parent", and cost-so-far
                         }

-- | A* Search
aStar
    :: forall n p. (Ord n, Ord p, Num p)
    => (n -> p)         -- ^ heuristic
    -> (n -> Map n p)   -- ^ neighborhood
    -> n                -- ^ start
    -> n                -- ^ target
    -> Maybe [n]        -- ^ the shortest path, if it exists
aStar h ex x0 dest = reconstruct <$> go (addBack x0 0 Nothing (AS M.empty Q.empty))
  where
    reconstruct :: Map n (Maybe n) -> [n]
    reconstruct mp = reverse $ goreco dest
      where
        goreco n = n : maybe [] goreco (mp M.! n)
    go :: AStarState n p -> Maybe (Map n (Maybe n))
    go as0@AS{..} = Q.minView _asOpen >>= \(n,_,(g,up),queue') ->
      let closed' = M.insert n up _asClosed
      in  if n == dest
            then Just closed'
            else go . M.foldlWithKey' (processNeighbor n g) (as0 { _asOpen = queue', _asClosed = closed'  })
                    $ ex n
    addBack :: n -> p -> Maybe n -> AStarState n p -> AStarState n p
    addBack x g up as0 = as0 { _asOpen = Q.insert x (g + h x) (g, up) . _asOpen $ as0 }
    processNeighbor :: n -> p -> AStarState n p -> n -> p -> AStarState n p
    processNeighbor curr currCost as0@AS{..} neighb moveCost
      | neighb `Q.member` _asOpen || neighb `M.member` _asClosed = as0
      | otherwise = addBack neighb (currCost + moveCost) (Just curr) as0

binarySearch
    :: (Int -> Ordering)        -- LT: Too small, GT: Too big
    -> Int
    -> Int
    -> Maybe Int
binarySearch p = go
  where
    go !x !y
        | x == y    = if p x == EQ then Just x else Nothing
        | otherwise = case p mid of
            LT -> go mid y
            EQ -> Just mid
            GT -> go x mid
      where
        mid = ((y - x) `div` 2) + x

exponentialSearch
    :: (Int -> Ordering)        -- LT: Too small, GT: Too big
    -> Int
    -> Maybe Int
exponentialSearch p = go
  where
    go !x = case p x of
      LT -> go (x * 2)
      EQ -> Just x
      GT -> binarySearch p (x `div` 2) x

-- | Find the lowest value where the predicate is satisfied within the
-- given bounds.
binaryMinSearch
    :: (Int -> Bool)
    -> Int
    -> Int
    -> Maybe Int
binaryMinSearch p = go
  where
    go !x !y
        | x == mid || y == mid = Just (x + 1)
        | p mid                = go x mid
        | otherwise            = go mid y
      where
        mid = ((y - x) `div` 2) + x

-- | Find the lowest value where the predicate is satisfied above a given
-- bound.
exponentialMinSearch
    :: (Int -> Bool)
    -> Int
    -> Maybe Int
exponentialMinSearch p = go
  where
    go !x
      | p x       = binaryMinSearch p (x `div` 2) x
      | otherwise = go (x * 2)

-- | Find the lowest value where the predicate is 'Just' within the
-- given bounds.
binaryFindMin
    :: (Int -> Maybe a)
    -> Int
    -> Int
    -> Maybe a
binaryFindMin p x0 y0 = binaryFindMin_ p (p y0) x0 y0

binaryFindMin_
    :: (Int -> Maybe a)
    -> Maybe a          -- p y0
    -> Int
    -> Int
    -> Maybe a
binaryFindMin_ p = go
  where
    go found !x !y
      | x == mid || y == mid = found
      | otherwise            = case p mid of
          Nothing    -> go found mid y
          f@(Just _) -> go f     x   mid
      where
        mid = ((y - x) `div` 2) + x

-- | Find the lowest value where the predicate is 'Just' above a given
-- bound.
exponentialFindMin
    :: (Int -> Maybe a)
    -> Int
    -> Maybe a
exponentialFindMin p = go
  where
    go !x = case p x of
      Nothing -> go (x * 2)
      f@(Just _) -> binaryFindMin_ p f (x `div` 2) x

