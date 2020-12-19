{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day19 where

-- module AOC.Challenge.Day19 (
--     day19a
--   , day19b
--   ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import           Data.Bitraversable
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

data Rule = Simple Char
          | Compound [[Int]]
  deriving (Show, Eq, Ord, Generic)

data AndOr = Leaf Char
           | And [AndOr]
           | Or  [AndOr]
  deriving (Show, Eq, Ord, Generic)

instance NFData Rule
instance NFData AndOr

parseRule :: String -> (Int, Rule)
parseRule (splitOn ":"->[i,cs]) = (read (i), supper)
  where
    supper = case traverse (traverse readMaybe . words) $ splitOn "|" cs of
      Just xs -> Compound xs
      Nothing -> case filter isLetter cs of
                     [c] -> Simple c

expandRules
    :: IntMap Rule
    -> IntMap AndOr
expandRules rules = res
  where
    res = rules <&> \case
      Simple c    -> Leaf c
      Compound cs -> Or $ map (And . map (res IM.!)) cs

    -- oneRule :: [Int] -> [[Char]]
    -- oneRule xs = for xs $ \x -> _ $ res IM.! x
        -- let downs = rules M.! c

match
    :: AndOr
    -> String
    -> [String]
match = \case
    Leaf c -> \case
      q : qs -> qs <$ guard (q == c)
      [] -> []
    And xs -> foldr (>=>) pure (match <$> xs)
    Or  xs -> \str -> concatMap (`match` str) xs

matchYes
    :: AndOr
    -> String
    -> Bool
matchYes ao s = not (null mao) && any null mao
  where
    mao = match ao s

day19a :: (IntMap Rule, [String]) :~> _
day19a = MkSol
    { sParse = fmap (bimap (IM.fromList . map parseRule . lines) lines) . preview _ListTup . splitOn "\n\n"
    , sShow  = show
    , sSolve = \(rs, ss) -> Just
            let er = expandRules rs IM.! 0
            in  countTrue (matchYes er) ss
    }

day19b :: _ :~> _
day19b = MkSol
    { sParse = sParse day19a
    , sShow  = show
    , sSolve = \(rs, ss) -> Just
        let rs' = IM.insert 8 (Compound [[42],[42,8]]) $ IM.insert 11 (Compound [[42,31],[42,11,31]]) $ rs
            er  = expandRules rs' IM.! 0
        in  countTrue (matchYes er) ss
    }
