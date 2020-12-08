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

import           AOC.Common                 (iterateMaybe, perturbationsBy, firstRepeatedBy, CharParser, parseLines)
import           AOC.Solver                 ((:~>)(..))
import           Control.Applicative        (empty)
import           Control.DeepSeq            (NFData)
import           Control.Lens               ((+~), each, _1, Ixed(..), Index, IxValue, (^?))
import           Data.Conduino
import           Data.Foldable
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.Generics.Labels       ()
import           Data.Maybe                 (listToMaybe)
import           Data.Vector                (Vector)
import           GHC.Generics               (Generic)
import           Safe                       (lastMay)
import qualified Data.Conduino.Combinators  as C
import qualified Data.Functor.Foldable      as R
import qualified Data.Functor.Foldable.TH   as R
import qualified Data.Set                   as S
import qualified Data.Vector                as V
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PL

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
commandParser = (,) <$> (instrParser <* P.space) <*> intParser
  where
    intParser = P.choice
      [ P.char '-' *> (negate <$> PL.decimal)
      , P.char '+' *> PL.decimal
      , PL.decimal
      ]

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

data EndState = Halt | Loop
  deriving (Generic, Eq, Ord, Show)
instance NFData EndState

vm  :: (Ixed t, Index t ~ Int, IxValue t ~ (Instr, Int))
    => t
    -> Pipe i Int u m EndState
vm cmds = go S.empty 0
  where
    go !seen !i
      | i `S.member` seen = pure Loop
      | otherwise         = case cmds ^? ix i of
          Nothing  -> pure Halt
          Just cmd -> case cmd of
            (NOP, _) -> go seen' (i + 1)
            (ACC, n) -> yield n *> go seen' (i + 1)
            (JMP, n) -> go seen' (i + n)
      where
        seen' = i `S.insert` seen

exhaustVM
    :: (Ixed t, Index t ~ Int, IxValue t ~ (Instr, Int))
    => t
    -> (EndState, Int)
exhaustVM cmds = runPipePure $ vm cmds 
             &| C.foldl (+) 0

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
        | cmds <- perturbationsBy (each . _1) perturbs cmds0
        , let (es, i) = exhaustVM cmds
        , es == Halt
        ]
    }
  where
    perturbs = \case
      NOP -> [JMP]
      ACC -> []
      JMP -> [NOP]
