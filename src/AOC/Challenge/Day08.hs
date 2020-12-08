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
import           Control.DeepSeq            (NFData)
import           Control.Lens               ((+~), each, _1)
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.Generics.Labels       ()
import           Data.Maybe                 (listToMaybe, maybeToList)
import           Data.Sequence              (Seq(..))
import           GHC.Generics               (Generic)
import           Safe                       (lastMay)
import qualified Data.Sequence              as Seq
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

data CState = CS { csPtr :: Int, csAcc :: Int }
  deriving (Generic, Show)
instance NFData CState

initialCS :: CState
initialCS = CS 0 0

runCommand :: Seq Command -> CState -> Maybe CState
runCommand cmds cs = Seq.lookup (csPtr cs) cmds <&> \case
    (NOP, _) -> cs & #csPtr +~ 1
    (ACC, i) -> cs & #csPtr +~ 1
                   & #csAcc +~ i
    (JMP, i) -> cs & #csPtr +~ i

day08a :: Seq Command :~> Int
day08a = MkSol
    { sParse = fmap Seq.fromList . parseLines commandParser
    , sShow  = show
    , sSolve = \cmds ->
        csAcc <$> firstRepeatedBy csPtr
            (iterateMaybe (runCommand cmds) initialCS)
    }

day08b :: Seq Command :~> Int
day08b = MkSol
    { sParse = fmap Seq.fromList . parseLines commandParser
    , sShow  = show
    , sSolve = \cmds0 -> listToMaybe
        [ res
        | cmds <- perturbationsBy (each . _1) perturbs cmds0
        , let states = iterateMaybe (runCommand cmds) initialCS
        , res <- maybeToList case firstRepeatedBy csPtr states of
            Nothing -> csAcc <$> lastMay states
            Just _  -> Nothing
        ]
    }
  where
    perturbs = \case
      NOP -> [NOP, JMP]
      ACC -> [ACC]
      JMP -> [NOP, JMP]
