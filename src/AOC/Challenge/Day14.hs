{-# LANGUAGE OverloadedStrings        #-}

-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import           AOC.Common                 (CharParser, parseLines)
import           AOC.Solver                 ((:~>)(..))
import           Control.DeepSeq            (NFData)
import           Control.Lens.Indexed       (ifoldlM, ifoldl')
import           Data.Bits                  (setBit, clearBit)
import           Data.Functor               (void)
import           Data.IntMap                (IntMap)
import           Data.List                  (foldl')
import           GHC.Generics               (Generic)
import qualified Data.IntMap                as IM
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PP

data Instr =
      Mask ![Maybe Bool]
    | Write !Int !Int
  deriving (Show, Eq, Ord, Generic)
instance NFData Instr

applyMask1
    :: Int
    -> [Maybe Bool]
    -> Int
applyMask1 = ifoldl' $ \i x -> \case
    Nothing    -> x
    Just False -> clearBit x i
    Just True  -> setBit   x i

day14a :: [Instr] :~> Int
day14a = MkSol
    { sParse = parseLines parseInstr
    , sShow  = show
    , sSolve = Just . sum . fst . foldl' go mempty
    }
  where
    go :: (IntMap Int, [Maybe Bool]) -> Instr -> (IntMap Int, [Maybe Bool])
    go (!mp, !msk) = \case
      Mask  msk'   -> (mp, msk')
      Write addr n -> (IM.insert addr (applyMask1 n msk) mp, msk)

applyMask2
    :: Int
    -> [Maybe Bool]
    -> [Int]
applyMask2 = ifoldlM $ \i x -> \case  -- we can save like 2ms with manual ifoldl'
    Nothing    -> [clearBit x i, setBit x i]
    Just False -> [x]
    Just True  -> [setBit x i]

day14b :: [Instr] :~> Int
day14b = MkSol
    { sParse = sParse day14a
    , sShow  = show
    , sSolve = Just . sum . fst . foldl' go mempty
    }
  where
    go :: (IntMap Int, [Maybe Bool]) -> Instr -> (IntMap Int, [Maybe Bool])
    go (!mp, !msk) = \case
      Mask  msk'   -> (mp, msk')
      Write addr n -> (IM.fromList ((,n) <$> applyMask2 addr msk) <> mp, msk)

parseInstr :: CharParser Instr
parseInstr = (Mask <$> P.try masker) P.<|> (uncurry Write <$> memer)
  where
    masker = do
      void "mask = "
      reverse <$> P.many bitter
    bitter = P.choice
      [ Just True  <$ P.char '1'
      , Just False <$ P.char '0'
      , Nothing    <$ P.char 'X'
      ]
    memer = (,)
      <$> ("mem[" *> PP.decimal <* "]")
      <*> (" = "  *> PP.decimal)

