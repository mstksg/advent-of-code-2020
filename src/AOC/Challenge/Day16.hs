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

import           AOC.Common                 (CharParser, sortSizedBy, withAllSized)
import           AOC.Solver                 ((:~>)(..), dyno_)
import           Control.DeepSeq            (NFData)
import           Control.Monad.State        (lift, modify, get, evalStateT)
import           Data.Char                  (isAlpha, isSpace)
import           Data.Distributive          (distribute)
import           Data.Foldable              (toList)
import           Data.IntervalMap.Strict    (IntervalMap)
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Maybe                 (listToMaybe, mapMaybe)
import           Data.Ord                   (comparing)
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import           Data.Traversable           (for)
import           GHC.Generics               (Generic)
import qualified Data.ExtendedReal          as ER
import qualified Data.Interval              as I
import qualified Data.IntervalMap.Strict    as IM
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
          let candidates = sortSizedBy (comparing (S.size . snd)) . V.indexed
                         . fmap (foldl1 S.intersection)
                         $ distribute vths
          -- we technically don't need a backtracking search, but it feels
          -- cleaner to write one
          validMap <- listToMaybe . flip evalStateT S.empty $ do
            for candidates $ \(i, cands) -> do
              soFar <- get
              pick  <- lift . toList $ cands S.\\ soFar
              (i, pick) <$ modify (S.insert pick)
          pure
            [ yours `V.index` i
            | (i, k) <- toList validMap
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
