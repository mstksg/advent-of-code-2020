{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day16 (
    day16a
  , day16b
  ) where

import           AOC.Common                 (CharParser, withAllSized, pickUnique)
import           AOC.Solver                 ((:~>)(..), dyno_)
import           Control.DeepSeq            (NFData)
import           Data.Char                  (isAlpha, isSpace)
import           Data.Distributive          (distribute)
import           Data.IntervalMap.Strict    (IntervalMap)
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Maybe                 (listToMaybe, mapMaybe)
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import qualified Data.ExtendedReal          as ER
import qualified Data.Interval              as I
import qualified Data.IntervalMap.Strict    as IM
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Vector.Sized          as V
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PP

type Passport = [Int]
data Info = Info
      { iFields :: IntervalMap Int (Set Text)
      , iYours  :: Passport
      , iTheirs :: [Passport]
      }
    deriving (Show, Eq, Generic)
instance NFData Info

day16a :: Info :~> Int
day16a = MkSol
    { sParse = P.parseMaybe parseInfo
    , sShow  = show
    , sSolve = \Info{..} -> Just . sum $
        [ n
        | ns <- iTheirs
        , n  <- ns
        , n `IM.notMember` iFields
        ]
    }

day16b :: Info :~> [Int]
day16b = MkSol
    { sParse = sParse day16a
    , sShow  = show . product
    , sSolve = \Info{..} -> do
        th : ths <- pure $ mapMaybe (traverse (`IM.lookup` iFields)) iTheirs
        withAllSized (th :| ths) $ \vths -> do
          yours      <- V.fromList iYours
          let candidates = V.toList . V.indexed
                         . fmap (foldl1 S.intersection)
                         $ distribute vths
          validMap   <- listToMaybe $ pickUnique candidates
          pure
            [ yours `V.index` i
            | (i, k) <- M.toList validMap
            , dyno_ "prefix" "departure" `T.isPrefixOf` k
            ]
    }

parseInfo :: CharParser Info
parseInfo = do
    iFields <- IM.fromListWith (<>) . concat <$> P.many (tok (P.try fieldParser))
    tok "your ticket:"
    iYours  <- tok $ passportParser
    tok "nearby tickets:"
    iTheirs <- passportParser `P.sepBy` P.newline
    pure Info{..}
  where
    tok p = p <* P.some P.newline
    fieldParser = do
      k  <- (P.satisfy (\c -> isAlpha c || isSpace c) `P.manyTill` ":") <* " "
      vs <- rangeParser `P.sepBy` P.string " or "
      pure $ (,S.singleton (T.pack k)) <$> vs
    rangeParser = (I.<=..<=)
              <$> (ER.Finite <$> PP.decimal <* "-")
              <*> (ER.Finite <$> PP.decimal)
    passportParser = PP.decimal `P.sepBy` ","
