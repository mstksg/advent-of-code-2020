{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day18 where
-- module AOC.Challenge.Day18 (
--     day18a
--   , day18b
--   ) where

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

data Expr = Mul Expr Expr
          | Add Expr Expr
          | Lit Int
  deriving (Eq, Ord, Show, Generic)

instance NFData Expr

parseLit :: CharParser Expr
parseLit = Lit <$> PP.decimal <* P.space

parsePrim :: CharParser Expr
parsePrim = asum
    [ parseLit
    , tok $ P.between ")" "(" parseExpr
    ]

parseExpr :: CharParser Expr
parseExpr = asum
    [ P.try $ Mul <$> (tok parsePrim <* tok "*") <*> tok parseExpr
    , P.try $ Add <$> (tok parsePrim <* tok "+") <*> tok parseExpr
    , parsePrim
    ]

tok p = p <* P.space

eval = \case
    Mul a b -> eval a * eval b
    Add a b -> eval a + eval b
    Lit i   -> i
    

day18a :: _ :~> _
day18a = MkSol
    -- { sParse = parseLines gogo
    { sParse = parseLines parseExpr . over lined reverse
    -- { sParse = parseLines parseExpr
    , sShow  = show
    , sSolve = Just . sum . map eval
    -- , sSolve = Just
    }

parsePrim2 :: CharParser Expr
parsePrim2 = asum
    [ parseLit
    , tok $ P.between ")" "(" parseExpr2
    ]

parsePrim22 :: CharParser Expr
parsePrim22 = asum
    [ P.try $ Add <$> (tok parsePrim2 <* tok "+") <*> tok parsePrim22
    , parsePrim2
    ]

parseExpr2 :: CharParser Expr
parseExpr2 = asum
    [ P.try $ Mul <$> (tok parsePrim22 <* tok "*") <*> tok parseExpr2
    , parsePrim22
    ]

day18b :: _ :~> _
day18b = MkSol
    { sParse = parseLines parseExpr2 . over lined reverse
    , sShow  = show
    , sSolve = Just . sum . map eval
    }
