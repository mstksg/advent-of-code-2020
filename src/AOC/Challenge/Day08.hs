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

import           AOC.Common      (iterateMaybe, perturbations)
import           AOC.Solver      ((:~>)(..))
import           Control.DeepSeq (NFData)
import           Data.Functor    ((<&>))
import           Data.IntMap     (IntMap)
import           Data.Maybe      (listToMaybe)
import           GHC.Generics    (Generic)
import           Text.Read       (readMaybe)
import qualified Data.IntMap     as IM
import qualified Data.Set        as S

data Command = NOP Int | ACC Int | JMP Int
  deriving (Generic, Show)

instance NFData Command

parseCommand :: String -> Maybe Command
parseCommand str = case words str of
    "nop":n:_ -> NOP <$> readn n
    "jmp":n:_ -> JMP <$> readn n
    "acc":n:_ -> ACC <$> readn n
    _         -> Nothing
  where
    readn ('-':ns) = negate <$> readMaybe ns
    readn ('+':ns) = readMaybe ns
    readn _        = Nothing

data CState = CS { csPt :: Int, csAcc :: Int }
  deriving (Generic, Show)

instance NFData CState

runCommand :: IntMap Command -> CState -> Maybe CState
runCommand cmds cs = IM.lookup (csPt cs) cmds <&> \case
    NOP _ -> cs { csPt = csPt cs + 1 }
    ACC i -> cs { csPt = csPt cs + 1, csAcc = csAcc cs + i }
    JMP i -> cs { csPt = csPt cs + i }

zipInts :: [a] -> IntMap a
zipInts = IM.fromList . zip [0..]

day08a :: [Command] :~> Int
day08a = MkSol
    { sParse = traverse parseCommand . lines
    , sShow  = show
    , sSolve = \cmds ->
        let states = iterateMaybe (runCommand (zipInts cmds)) (CS 0 0)
        in  csAcc <$> firstRepeatedBy csPt states
    }

firstRepeatedBy :: Ord a => (b -> a) -> [b] -> Maybe b
firstRepeatedBy f = go S.empty
  where
    go seen (x:xs)
      | f x `S.member` seen = Just x
      | otherwise         = go (f x `S.insert` seen) xs
    go _ []     = Nothing

day08b :: [Command] :~> Int
day08b = MkSol
    { sParse = traverse parseCommand . lines
    , sShow  = show
    , sSolve = \cmds0 -> listToMaybe
        [ res
        | cmds <- perturbations (\case NOP i -> [NOP i, JMP i]
                                       ACC i -> [ACC i]
                                       JMP i -> [JMP i, NOP i]
                                ) cmds0
        , let states = iterateMaybe (runCommand (zipInts cmds)) (CS 0 0)
        , res <- case firstRepeatedBy csPt states of
            Nothing -> [csAcc $ last states]
            Just _  -> []
        ]
    }

