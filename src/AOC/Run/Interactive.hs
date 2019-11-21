-- |
-- Module      : AOC.Run.Interactive
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Versions of loaders and runners meant to be used in GHCI.
--

module AOC.Run.Interactive (
  -- * Fetch and Run
  -- ** Return Answers
    execSolution
  , execSolutionWith
  , testSolution
  , viewPrompt
  , waitForPrompt
  , submitSolution
  -- ** No Answers
  , execSolution_
  , execSolutionWith_
  , testSolution_
  , viewPrompt_
  , waitForPrompt_
  , submitSolution_
  -- * Load Inputs
  , loadInput
  , loadParseInput
  , loadTests
  , loadParseTests
  -- * Util
  , mkSpec
  ) where

import           AOC.Challenge
import           AOC.Run
import           AOC.Run.Config
import           AOC.Run.Load
import           AOC.Solver
import           AOC.Util
import           Advent
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Text            (Text)

-- | Run the solution indicated by the challenge spec on the official
-- puzzle input.  Get answer as result.
execSolution :: ChallengeSpec -> IO String
execSolution cs = eitherIO $ do
    cfg <- liftIO $ configFile defConfPath
    out <- mainRun cfg . defaultMRO $ TSDayPart cs
    res <- maybeToEither ["Result not found in result map (Internal Error)"] $
      lookupSolution cs out
    liftEither $ snd res

-- | Run the solution indicated by the challenge spec on a custom input.
-- Get answer as result.
execSolutionWith
    :: ChallengeSpec
    -> String               -- ^ custom puzzle input
    -> IO String
execSolutionWith cs inp = eitherIO $ do
    cfg <- liftIO $ configFile defConfPath
    out <- mainRun cfg $ (defaultMRO (TSDayPart cs))
      { _mroInput = \_ _ -> pure $ Just inp
      }
    res <- maybeToEither ["Result not found in result map (Internal Error)"] $
      lookupSolution cs out
    liftEither $ snd res

-- | Run test suite for a given challenge spec.
--
-- Returns 'Just' if any tests were run, with a 'Bool' specifying whether
-- or not all tests passed.
testSolution :: ChallengeSpec -> IO (Maybe Bool)
testSolution cs = eitherIO $ do
    cfg <- liftIO $ configFile defConfPath
    out <- mainRun cfg $ (defaultMRO (TSDayPart cs))
      { _mroTest  = True
      }
    res <- maybeToEither ["Result not found in result map (Internal Error)"] $
      lookupSolution cs out
    pure $ fst res

-- | View the prompt for a given challenge spec.
viewPrompt :: ChallengeSpec -> IO Text
viewPrompt cs@CS{..} = eitherIO $ do
    cfg <- liftIO $ configFile defConfPath
    out <- mainView cfg . defaultMVO $ TSDayPart cs
    maybeToEither ["Prompt not found in result map (Internal Error)"] $
      lookupSolution cs out

-- | Countdown to get the prompt for a given challenge spec, if not yet
-- available.
waitForPrompt :: ChallengeSpec -> IO Text
waitForPrompt cs@CS{..} = eitherIO $ do
    cfg <- liftIO $ configFile defConfPath
    out <- mainView cfg $ (defaultMVO (TSDayPart cs))
      { _mvoWait = True
      }
    maybeToEither ["Prompt not found in result map (Internal Error)"] $
      lookupSolution cs out

-- | Submit solution for a given challenge spec, and lock if correct.
submitSolution :: ChallengeSpec -> IO (Text, SubmitRes)
submitSolution cs = eitherIO $ do
    cfg <- liftIO $ configFile defConfPath
    mainSubmit cfg . defaultMSO $ cs

-- | Result-suppressing version of 'execSolution'.
execSolution_ :: ChallengeSpec -> IO ()
execSolution_ = void . execSolution

-- | Result-suppressing version of 'execSolutionWith'.
execSolutionWith_
    :: ChallengeSpec
    -> String               -- ^ custom puzzle input
    -> IO ()
execSolutionWith_ cs = void . execSolutionWith cs

-- | Result-suppressing version of 'testSolution'.
testSolution_ :: ChallengeSpec -> IO ()
testSolution_ = void . testSolution

-- | Result-suppressing version of 'viewPrompt'.
viewPrompt_ :: ChallengeSpec -> IO ()
viewPrompt_ = void . viewPrompt

-- | Result-suppressing version of 'waitForPrompt'.
waitForPrompt_ :: ChallengeSpec -> IO ()
waitForPrompt_ = void . waitForPrompt

-- | Result-suppressing version of 'submitSolution'.
submitSolution_ :: ChallengeSpec -> IO ()
submitSolution_ = void . submitSolution

-- | Run the parser of a solution, given its 'ChallengeSpec'.
--
-- @
-- 'loadParseInput' (solSpec 'day01a) day01a
-- @
loadParseInput :: ChallengeSpec -> a :~> b -> IO a
loadParseInput cs s = eitherIO $ do
    i <- liftIO $ loadInput cs
    maybeToEither ["No parse"] $ sParse s i

-- | Run the parser of a solution on test data, given its 'ChallengeSpec'.
--
-- @
-- 'loadParseTests' (solSpec 'day01a) day01a
-- @
loadParseTests :: ChallengeSpec -> a :~> b -> IO [(Maybe a, TestMeta)]
loadParseTests cs s = (map . first) (sParse s) <$> loadTests cs

-- | Load input for a given challenge
loadInput :: ChallengeSpec -> IO String
loadInput cs = eitherIO $ do
    CD{..}  <- liftIO $ do
      Cfg{..} <- configFile defConfPath
      challengeData _cfgSession _cfgYear cs
    liftEither _cdInput

-- | Load test cases for a given challenge
loadTests :: ChallengeSpec -> IO [(String, TestMeta)]
loadTests cs = do
    Cfg{..} <- configFile defConfPath
    _cdTests <$> challengeData _cfgSession _cfgYear cs

-- | Unsafely create a 'ChallengeSpec' from a day number and part.
--
-- Is undefined if given a day number out of range (1-25).
mkSpec :: Integer -> Part -> ChallengeSpec
mkSpec i = CS (mkDay_ i)

eitherIO :: ExceptT [String] IO a -> IO a
eitherIO act = runExceptT act >>= \case
    Right x  -> pure x
    Left  es -> fail $ unlines es

