-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Common               (perturbationsBy, CharParser, parseLines, pDecimal)
import           AOC.Solver               ((:~>)(..))
import           Control.DeepSeq          (NFData)
import           Control.Lens             (_1, Ixed(..), Index, IxValue, (^?))
import           Data.IntSet              (IntSet)
import           Data.Maybe               (listToMaybe)
import           Data.Vector              (Vector)
import           GHC.Generics             (Generic)
import qualified Data.Functor.Foldable    as R
import qualified Data.Functor.Foldable.TH as R
import qualified Data.IntSet              as IS
import qualified Data.Vector              as V
import qualified Text.Megaparsec          as P
import qualified Text.Megaparsec.Char     as P

data Instr = NOP | ACC | JMP
  deriving (Generic, Eq, Ord, Show)
instance NFData Instr

type Command = (Instr, Int)

instrParser :: CharParser Instr
instrParser = P.choice
    [ NOP <$ P.string "nop"
    , ACC <$ P.string "acc"
    , JMP <$ P.string "jmp"
    ]

commandParser :: CharParser Command
commandParser = (,) <$> (instrParser <* P.space) <*> pDecimal

-- RIP explicit state

-- data CState = CS { csPtr :: !Int, csAcc :: !Int }
--   deriving (Generic, Show)
-- instance NFData CState

-- initialCS :: CState
-- initialCS = CS 0 0

-- runCommand
--     :: (Ixed t, Index t ~ Int, IxValue t ~ (Instr, Int))
--     => t
--     -> CState
--     -> Maybe CState
-- runCommand cmds cs = (cmds ^? ix (csPtr cs)) <&> \case
--     (NOP, _) -> cs & #csPtr +~ 1
--     (ACC, i) -> cs & #csPtr +~ 1
--                    & #csAcc +~ i
--     (JMP, i) -> cs & #csPtr +~ i

data EndType = Halt | Loop
  deriving (Generic, Eq, Ord, Show)
instance NFData EndType

data AccStream = EndAcc EndType | Step AccStream | Acc Int AccStream
R.makeBaseFunctor ''AccStream

-- | Unfold an 'AccStream' over a program bank (@t@), given a seen-items
-- list and the current instruction pointer.
vmStreamCoalg
    :: (Ixed t, Index t ~ Int, IxValue t ~ (Instr, Int))
    => t
    -> (IntSet, Int)
    -> AccStreamF (IntSet, Int)
vmStreamCoalg cmds (!seen, !i)
    | i `IS.member` seen = EndAccF Loop
    | otherwise          = case cmds ^? ix i of
        Nothing  -> EndAccF Halt
        Just cmd -> case cmd of
          (NOP, _) -> StepF  (seen', i+1)
          (ACC, n) -> AccF n (seen', i+1)
          (JMP, n) -> StepF  (seen', i+n)
  where
    seen' = i `IS.insert` seen

-- | Collapse an 'AccStream' to get the sum and the end state.
sumStreamAlg
    :: AccStreamF (EndType, Int)
    -> (EndType, Int)
sumStreamAlg = \case
    EndAccF es      -> (es, 0)
    StepF a         -> a
    AccF n (es, !x) -> (es, x + n)

exhaustVM
    :: (Ixed t, Index t ~ Int, IxValue t ~ (Instr, Int))
    => t
    -> (EndType, Int)
exhaustVM cmds = R.hylo sumStreamAlg (vmStreamCoalg cmds) (IS.empty, 0)

day08a :: Vector Command :~> Int
day08a = MkSol
    { sParse = fmap V.fromList . parseLines commandParser
    , sShow  = show
    , sSolve = Just . snd . exhaustVM
    }

day08b :: Vector Command :~> Int
day08b = MkSol
    { sParse = fmap V.fromList . parseLines commandParser
    , sShow  = show
    , sSolve = \cmds0 -> listToMaybe [
          i
        | cmds <- perturbationsBy (traverse . _1) perturbs cmds0
        , let (es, i) = exhaustVM cmds
        , es == Halt
        ]
    }
  where
    perturbs = \case
      NOP -> [JMP]
      ACC -> []
      JMP -> [NOP]
