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
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.Generics.Labels       ()
import           Data.Maybe                 (listToMaybe)
import           Data.Vector                (Vector)
import           GHC.Generics               (Generic)
import           Safe                       (lastMay)
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

data CState = CS { csPtr :: !Int, csAcc :: !Int }
  deriving (Generic, Show)
instance NFData CState

initialCS :: CState
initialCS = CS 0 0

runCommand
    :: (Ixed t, Index t ~ Int, IxValue t ~ (Instr, Int))
    => t
    -> CState
    -> Maybe CState
runCommand cmds cs = (cmds ^? ix (csPtr cs)) <&> \case
    (NOP, _) -> cs & #csPtr +~ 1
    (ACC, i) -> cs & #csPtr +~ 1
                   & #csAcc +~ i
    (JMP, i) -> cs & #csPtr +~ i

day08a :: Vector Command :~> Int
day08a = MkSol
    { sParse = fmap V.fromList . parseLines commandParser
    , sShow  = show
    , sSolve = \cmds ->
        csAcc <$> firstRepeatedBy csPtr
            (iterateMaybe (runCommand cmds) initialCS)
    }

day08b :: Vector Command :~> Int
day08b = MkSol
    { sParse = fmap V.fromList . parseLines commandParser
    , sShow  = show
    , sSolve = \cmds0 -> listToMaybe $ do
        cmds <- perturbationsBy (each . _1) perturbs cmds0
        let states = iterateMaybe (runCommand cmds) initialCS
        case firstRepeatedBy csPtr states of
          Nothing -> maybe empty (pure . csAcc) $ lastMay states
          Just _  -> empty
    }
  where
    perturbs = \case
      NOP -> [NOP, JMP]
      ACC -> [ACC]
      JMP -> [NOP, JMP]
