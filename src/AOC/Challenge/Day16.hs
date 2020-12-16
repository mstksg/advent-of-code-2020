{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day16 (
    day16a
  , day16b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import           Data.Semigroup.Foldable
import qualified Data.Sequence                  as Seq
import qualified Data.Set.NonEmpty as NES
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

day16a :: _ :~> _
day16a = MkSol
    { sParse = \xs -> case lines <$> splitOn "\n\n" xs of
            [valids, _:yt, _:others] -> Just (concatMap (getV) valids, map (mapMaybe (readMaybe @Int) . splitOn ",") others)
    , sShow  = show
    , sSolve = \(vs, ts) -> Just $
        let poop v = not $ any (`inRange` v) vs
        -- let allInRange v = any (`inRange` v) vs
        in  sum . filter poop . concat $ ts
    }
  where
    getV str = case dropWhile (/= ':') str of
      _:xs -> case splitOn " or " xs of
        [ a,b ] -> [foo a, foo b]
    foo x = case splitOn "-" x of
                [a,b] -> (read @Int a, read @Int b)


type Passport = IntMap Int
data Info = Info { iFields :: Map String [(Int, Int)], iYours :: Passport, iTheirs :: [Passport] }
           deriving (Show, Eq, Ord, Generic)
instance NFData Info

day16b :: Info :~> _
day16b = MkSol
    { sParse = \xs -> case lines <$> splitOn "\n\n" xs of
            [valids, _:[yt], _:others] -> Just $ Info
                { iFields = M.fromList . map getV $ valids
                -- , iYours = mempty
                -- , iTheirs = mempty
                , iYours = IM.fromList . zip [0..] . mapMaybe readMaybe . splitOn "," $ yt
                , iTheirs = map (IM.fromList . zip [0..] . mapMaybe readMaybe . splitOn ",") others
                }
                -- (map (getV) valids, map (mapMaybe (readMaybe @Int) . splitOn ",") others)
    , sShow  = show . product
    , sSolve = \Info{..} ->
        let anyValid n = any (`inRange` n) (fold iFields)
            theirs = filter (all anyValid) iTheirs
        in  do upTheirs <- NE.nonEmpty $
                 (mapMaybe . traverse) (\n -> NES.nonEmptySet . M.keysSet $ M.filter (any (`inRange` n)) iFields) iTheirs
               -- upTheirs :: NonEmpty (IntMap (NESet String))
               validI <- traverse NES.nonEmptySet $ IM.fromList
                 [ (i, foldr1 S.intersection $ fmap (NES.toSet . (IM.! i)) upTheirs)
                 | i <- [0 .. 19]
                 -- | i <- [0.. (dyno_ @Int "fields" 20 - 1)]
                 ]
               traceM $ show validI
               validMap <- fmap IM.toList . listToMaybe . flip evalStateT (M.keysSet iFields) $ do
                 flip IM.traverseWithKey validI $ \i valids -> do
                   soFar <- get
                   pick <- lift . toList $ NES.toSet valids `S.intersection` soFar
                   modify $ S.delete pick
                   when (i > 18) $ traceM (show (show i, pick))
                   pure pick
                   -- cand <- StateT select
                   -- guard $ cand `NES.member` valids
                   -- pure cand
               traceM $ show validMap
               let departureIx = IS.fromList . mapMaybe (\(i, str) -> i <$ guard ("departure" `isPrefixOf` str)) $ validMap
               pure . toList $ IM.restrictKeys iYours departureIx


        -- in  do validMap <- listToMaybe . flip evalStateT (M.toList iFields) $ do
        --          for [0..19] $ \i -> do
        --            (cand, rs) <- StateT select
        --            when (i > 0) $ traceM (show (show i, cand))
        --            guard $ all ((cand `NES.member`) . (IM.! i)) upTheirs
        --                -- theirs
        --            pure (i, cand)
        --        let departureIx = IS.fromList . mapMaybe (\(i, str) -> i <$ guard ("departure" `isPrefixOf` str)) $ validMap
        --        pure . toList $ IM.restrictKeys iYours departureIx

-- departure location: 50-692 or 705-969
-- departure station: 35-540 or 556-965
-- departure platform: 27-776 or 790-974
-- departure track: 48-903 or 911-955
-- departure date: 33-814 or 836-953
-- departure time: 34-403 or 421-966
-- arrival location: 37-489 or 510-963
-- arrival station: 46-128 or 150-960
-- arrival platform: 45-75 or 97-959
-- arrival track: 28-535 or 541-952
-- class: 38-340 or 349-966
-- duration: 42-308 or 316-969
-- price: 49-324 or 340-970
-- route: 31-627 or 648-952
-- row: 38-878 or 893-955
-- seat: 39-54 or 71-967
-- train: 36-597 or 615-960
-- type: 41-438 or 453-959
-- wagon: 42-370 or 389-971
-- zone: 36-114 or 127-965

-- Once you work out which field is which, look for the six fields on *your
-- ticket* that start with the word `departure`. *What do you get if you
-- multiply those six values together?*

        -- in  Just validMap

    -- , sSolve = \(vs, ts) -> Just $
    --     let inAnyRange v = any (`inRange` v) (concatMap (\(x,y) -> [x,y]) vs)
    --         valids = filter (all inAnyRange) ts
    --     in  True
        -- let poop v = not $ any (`inRange` v) vs
        -- -- let allInRange v = any (`inRange` v) vs
        -- in  sum . filter poop . concat $ ts
    }
  where
    getV str = case splitOn ":" str of
      [k,rs] -> case words rs of
        [r1,_,r2] -> (k, [foo r1, foo r2])
    foo x = case splitOn "-" x of
                [a,b] -> (read @Int a, read @Int b)
      
-- zone: 36-114 or 127-965
-- clearOut :: (Char -> Bool) -> String -> String
-- clearOut p = map $ \c -> if p c then ' '
--                                 else c
    -- getV str = case dropWhile (/= ':') str of
    --   _:xs -> case splitOn " or " xs of
    --     [ a,b ] -> (foo a, foo b)
    -- foo x = case splitOn "-" x of
    --             [a,b] -> (read @Int a, read @Int b)

