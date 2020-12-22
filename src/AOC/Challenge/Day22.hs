{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day22
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day22 (
    day22a
  , day22b
  ) where

import           AOC.Solver                 ((:~>)(..))
import           Control.DeepSeq            (NFData)
import           Control.Monad              (guard)
import           Data.Foldable              (toList)
import           Data.Hashable              (hash)
import           Data.IntSet                (IntSet)
import           Data.Sequence              (Seq(..))
import           Data.Sequence.NonEmpty     (NESeq(..))
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import qualified Data.IntSet                as IS
import qualified Data.Sequence              as Seq
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PP

type Deck   = Seq Int
type NEDeck = NESeq Int

data Player = P1 | P2
  deriving (Show, Eq, Ord, Generic)
instance NFData Player

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . toList

playGameWith
    :: (NEDeck -> NEDeck -> Maybe Player)       -- ^ recurse
    -> Deck
    -> Deck
    -> (Player, Deck)
playGameWith f = go IS.empty
  where
    go :: IntSet -> Deck -> Deck -> (Player, Deck)
    go !seen !xs0 !ys0
        | hashHand xs0 ys0 `IS.member` seen = (P1, xs0)
        | otherwise                         = case (xs0, ys0) of
            (x :<| xs, y :<| ys) ->
              let winner = case f (x :<|| xs) (y :<|| ys) of
                    Nothing -> if x > y then P1 else P2
                    Just p  -> p
              in  case winner of
                    P1 -> go seen' (xs :|> x :|> y) ys
                    P2 -> go seen' xs (ys :|> y :|> x)
            (Empty, _    ) -> (P2, ys0)
            (_    , Empty) -> (P1, xs0)
      where
        seen' = IS.insert (hashHand xs0 ys0) seen
{-# INLINE playGameWith #-}

hashHand :: Deck -> Deck -> Int
hashHand xs ys = hash
    ( headMay xs, lastMay xs
    , headMay ys, lastMay ys
    , Seq.length xs
    )
  where
    headMay (r :<| _) = Just r
    headMay _         = Nothing
    lastMay (_ :|> r) = Just r
    lastMay _         = Nothing
{-# INLINE hashHand #-}

game1 :: Deck -> Deck -> (Player, Deck)
game1 = playGameWith $ \_ _ -> Nothing
{-# INLINE game1 #-}

game2 :: Deck -> Deck -> (Player, Deck)
game2 = playGameWith $ \(x :<|| xs) (y :<|| ys) -> do
    xs' <- takeExactly x xs
    ys' <- takeExactly y ys
    let xMax = maximum xs'
        yMax = maximum ys'
        -- P1 has unbeatable card, and cannot be recursed without it
    pure if xMax > yMax && xMax > (x+y)
      then P1
      else fst $ game2 xs' ys'
{-# INLINE game2 #-}

day22a :: (Deck, Deck) :~> Deck
day22a = MkSol
    { sParse = P.parseMaybe gameParser
    , sShow  = show . score
    , sSolve = Just . snd . uncurry game1
    }

day22b :: (Deck, Deck) :~> Deck
day22b = MkSol
    { sParse = P.parseMaybe gameParser
    , sShow  = show . score
    , sSolve = Just . snd . uncurry game2
    }

takeExactly :: Int -> Seq a -> Maybe (Seq a)
takeExactly n xs = Seq.take n xs <$ guard (Seq.length xs >= n)
{-# INLINE takeExactly #-}

gameParser :: P.Parsec Void String (Deck, Deck)
gameParser = (,) <$> deckParser <*> deckParser
  where
    deckParser = do
      _ :: Int <- "Player " *> PP.decimal <* ":" <* P.newline
      fmap Seq.fromList . P.many $ PP.decimal <* P.many P.newline

