{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day21 (
    day21a
  , day21b
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
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import           Control.Monad.State

parseLine :: String -> (Set String, Set String)
parseLine str = case splitOn "(contains" str of
    [igs, rs] -> (S.fromList (words igs), S.fromList (words $ filter go rs))
  where
    go c = isAlpha c || isSpace c

legal :: Map String String -> (Set String, Set String) -> Bool
legal igrs (fd, alg) = inHere `S.isSubsetOf` fd
  where
    inHere = S.fromList . toList $ igrs `M.restrictKeys` alg

day21a :: _ :~> _
day21a = MkSol
    { sParse = Just . map parseLine . lines
    , sShow  = show
    , sSolve = \igrsalg -> do
        let (fold->igr, fold->alg) = unzip igrsalg
            pairings = searchMe igrsalg
        pickMe <- listToMaybe $ flip evalStateT S.empty $ do
                traverse (\poss -> do
                      seen <- get
                      pick <- lift $ S.toList (poss `S.difference` seen)
                      modify $ S.insert pick
                      pure pick
                    )
                  pairings
        let goodFoods = igr `S.difference` (S.fromList $ toList pickMe)
        pure $ sum $ 
            [ S.size $ is `S.intersection` goodFoods
            | (is, _) <- igrsalg
            ]
    }

searchMe
    :: [(Set String, Set String)]
    -> Map String (Set String)
searchMe xs = foldl' go (M.fromSet (const igr) alg) xs
  where
    (fold->igr, fold->alg) = unzip xs
    go    :: Map String (Set String)
           -> (Set String, Set String)
           -> (Map String (Set String))
    go (opts) (ig, al) =
        M.unionWith S.intersection opts (M.fromSet (const ig) al)

day21b :: _ :~> _
day21b = MkSol
    { sParse = sParse day21a
    , sShow  = intercalate "," . toList
    , sSolve = \igrsalg -> do
        let (fold->igr, fold->alg) = unzip igrsalg
            pairings = searchMe igrsalg
        pickMe <- listToMaybe $ flip evalStateT S.empty $ do
                traverse (\poss -> do
                      seen <- get
                      pick <- lift $ S.toList (poss `S.difference` seen)
                      modify $ S.insert pick
                      pure pick
                    )
                  pairings
        pure pickMe
    }
