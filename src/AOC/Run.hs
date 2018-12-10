{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : AOC.Interactive
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Run actions regarding challenges, solutions, tests, submissions, viewing
-- prompts, etc.
--
-- Essentially implements the functionality of the main app.
--

module AOC.Run (
  -- * Options
    TestSpec(..)
  -- * Runners
  -- ** Run solutions, tests, benchmarks
  , MainRunOpts(..), HasMainRunOpts(..), mainRun, defaultMRO
  -- ** View prompts
  , MainViewOpts(..), HasMainViewOpts(..), mainView, defaultMVO
  -- ** Submit answers
  , MainSubmitOpts(..), HasMainSubmitOpts(..), mainSubmit, defaultMSO
  -- * Util
  , withColor
  ) where

import           AOC.Challenge
import           Lens.Micro.TH
import           AOC.Run.Config
import           AOC.Run.Load
import           AOC.Solver
import           AOC.Util
import           Advent
import           Control.Applicative
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Criterion
import           Data.Bifunctor
import           Data.Char
import           Data.Finite
import           Data.Map                 (Map)
import           Data.Maybe
import           Data.Text                (Text)
import           Data.Time
import           Text.Printf
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified System.Console.ANSI      as ANSI
import qualified System.Console.Haskeline as H

-- | Specification of parts to test and run
data TestSpec = TSAll
              | TSDayAll  { _tsDay  :: Finite 25     }
              | TSDayPart { _tsSpec :: ChallengeSpec }
  deriving Show

-- | Options for 'mainRun'.
data MainRunOpts = MRO { _mroSpec  :: !TestSpec
                       , _mroTest  :: !Bool     -- ^ Run tests?  (Default: False)
                       , _mroBench :: !Bool     -- ^ Benchmark?  (Default: False)
                       , _mroLock  :: !Bool     -- ^ Lock in answer as correct?  (Default: False)
                       , _mroInput :: !(Finite 25 -> Part -> IO (Maybe String))   -- ^ Manually supply input (Default: always return Nothing)
                       }

makeClassy ''MainRunOpts

-- | Options for 'mainView'.
data MainViewOpts = MVO { _mvoSpec :: !TestSpec
                        , _mvoWait :: !Bool
                        }
  deriving Show

makeClassy ''MainViewOpts

-- | Options for 'mainSubmit'
data MainSubmitOpts = MSO { _msoSpec  :: !ChallengeSpec
                          , _msoTest  :: !Bool    -- ^ Run tests before submitting?  (Default: True)
                          , _msoForce :: !Bool    -- ^ Force submission even if bad?  (Default: False)
                          , _msoLock  :: !Bool    -- ^ Lock answer if submission succeeded?  (Default: True)
                          }
  deriving Show

makeClassy ''MainSubmitOpts

-- | Default options for 'mainRun'.
defaultMRO :: TestSpec -> MainRunOpts
defaultMRO ts = MRO { _mroSpec  = ts
                    , _mroTest  = False
                    , _mroBench = False
                    , _mroLock  = False
                    , _mroInput = \_ _ -> pure Nothing
                    }

-- | Default options for 'mainView'.
defaultMVO :: TestSpec -> MainViewOpts
defaultMVO ts = MVO { _mvoSpec = ts
                    , _mvoWait = False
                    }

-- | Default options for 'mainSubmit'.
defaultMSO :: ChallengeSpec -> MainSubmitOpts
defaultMSO cs = MSO { _msoSpec  = cs
                    , _msoTest  = True
                    , _msoForce = False
                    , _msoLock  = True
                    }

filterChallengeMap :: TestSpec -> Either String ChallengeMap
filterChallengeMap = \case
    TSAll      -> pure challengeMap
    TSDayAll d -> maybeToEither (printf "Day not yet avaiable: %d" (dayToInt d)) $
                     M.singleton d <$> M.lookup d challengeMap
    TSDayPart (CS d p) -> do
      ps <- maybeToEither (printf "Day not yet available: %d" (dayToInt d)) $
              M.lookup d challengeMap
      c  <- maybeToEither (printf "Part not found: %c" (partChar p)) $
              M.lookup p ps
      pure $ M.singleton d (M.singleton p c)

-- | Run, test, bench.
mainRun
    :: (MonadIO m, MonadError [String] m)
    => Config
    -> MainRunOpts
    -> m (Map (Finite 25) (Map Part (Maybe Bool, Either [String] String)))  -- whether or not passed tests, and result
mainRun Cfg{..} MRO{..} =  do
    toRun <- liftEither . first (:[]) . filterChallengeMap $ _mroSpec
    liftIO . runAll _cfgSession _cfgYear _mroLock _mroInput toRun $ \c inp0 cd@CD{..} -> do
      testRes <- fmap join . forM (guard _mroTest) $ \_ ->
        runTestSuite c cd

      let inp1 = maybe _cdInput  Right           inp0
          ans1 = maybe _cdAnswer (const Nothing) inp0
      case inp1 of
        Right inp
          | _mroBench -> (testRes, Left ["Ran benchmark, so no result"]) <$ benchmark (nf (runSomeSolution c) inp)
          | otherwise -> (second . first) ((:[]) . show) <$> testCase False c inp (TM ans1 M.empty)
        Left e
          | _mroTest  -> pure (testRes, Left ["Ran tests, so no result"])
          | otherwise -> (testRes, Left e) <$ putStrLn "[INPUT ERROR]" <* mapM_ putStrLn e

-- | View prompt
mainView
    :: (MonadIO m, MonadError [String] m)
    => Config
    -> MainViewOpts
    -> m (Map (Finite 25) (Map Part Text))
mainView Cfg{..} MVO{..} = do
    let toRun = maybe S.empty (M.keysSet . pullMap)
              . eitherToMaybe
              . filterChallengeMap
              $ _mvoSpec
        allRun = foldMap S.singleton singleTest <> toRun
    fmap pushMap . sequenceA . flip M.fromSet allRun $ \(d,p) -> do
      pmpt   <- waitFunc d $ do
        CD{..} <- liftIO $ challengeData _cfgSession _cfgYear (CS d p)
        liftEither . first ("[PROMPT ERROR]":) $ _cdPrompt
      liftIO $ do
        withColor ANSI.Dull ANSI.Blue $
          printf ">> Day %02d%c\n" (dayToInt d) (partChar p)
        T.putStrLn pmpt
        putStrLn ""
      pure pmpt
  where
    waitFunc d
      | _mvoWait  = countdownConsole d . (liftIO (threadDelay 500000) *>)
      | otherwise = id
    singleTest = case _mvoSpec of
      TSAll        -> Nothing
      TSDayAll d   -> Just (d, Part1)
      TSDayPart cs -> Just (_csDay cs, _csPart cs)

-- | Submit and analyze result
mainSubmit
    :: (MonadIO m, MonadError [String] m)
    => Config
    -> MainSubmitOpts
    -> m (Text, SubmitRes)
mainSubmit Cfg{..} MSO{..} = do
    cd@CD{..} <- liftIO $ challengeData _cfgSession _cfgYear _msoSpec
    dMap      <- maybeToEither [printf "Day not yet available: %d" d'] $
                   M.lookup _csDay challengeMap
    c         <- maybeToEither [printf "Part not found: %c" (partChar _csPart)] $
                   M.lookup _csPart dMap
    inp       <- liftEither . first ("[PROMPT ERROR]":) $ _cdInput
    opts      <- defaultAoCOpts 2018 <$>
                    maybeToEither ["ERROR: Session Key Required to Submit"]
                      _cfgSession

    when _msoTest $ do
      testRes <- liftIO $ runTestSuite c cd
      unless (and testRes) $
        if _msoForce
          then liftIO $ putStrLn "Proceeding with submission despite test failures (--force)"
          else do
            conf <- liftIO . H.runInputT H.defaultSettings $
              H.getInputChar "Some tests failed. Are you sure you wish to proceed? y/(n) "
            case toLower <$> conf of
              Just 'y' -> pure ()
              _        -> throwError ["Submission aborted."]

    resEither <- liftIO . evaluate . force . runSomeSolution c $ inp
    res       <- liftEither . first (("[SOLUTION ERROR]":) . (:[]) . show) $ resEither
    liftIO $ printf "Submitting solution: %s\n" res

    output@(resp, status) <- liftEither . first showAoCError
                         =<< liftIO (runAoC opts (AoCSubmit _csDay _csPart res))
    let resp' = formatResp
              . either (map T.pack) T.lines
              . htmlToMarkdown False
              $ resp
        (color, lock, out) = displayStatus status
    liftIO $ do
      withColor ANSI.Vivid color $
        putStrLn out
      putStrLn resp'
      when lock $
        if _msoLock
          then putStrLn "Locking correct answer." >> writeFile _cpAnswer res
          else putStrLn "Not locking correct answer (--no-lock)"
      zt <- getZonedTime
      appendFile _cpLog $ printf logFmt (show zt) res (showSubmitRes status) resp resp'
    pure output
  where
    CS{..} = _msoSpec
    CP{..} = challengePaths _msoSpec
    d' = dayToInt _csDay
    formatResp = T.unpack . T.intercalate "\n" . map ("> " <>)
    logFmt = unlines [ "[%s]"
                     , "Submission: %s"
                     , "Status: %s"
                     , "Raw: %s"
                     , "%s"
                     ]

displayStatus :: SubmitRes -> (ANSI.Color, Bool, String)
displayStatus = \case
    SubCorrect r     -> ( ANSI.Green  , True , correctMsg r     )
    SubIncorrect t h -> ( ANSI.Red    , False, incorrectMsg t h )
    SubWait t        -> let (m, s) = t `divMod` 60
                            resp   = printf "Answer re-submitted too soon.  Please wait %dmin %dsec" m s
                        in  ( ANSI.Yellow, False, resp )
    SubInvalid{}     -> ( ANSI.Blue   , False
                        , "Submission was rejected.  Maybe not unlocked yet, or already answered?"
                        )
    SubUnknown{}     -> ( ANSI.Magenta, False
                        , "Response from server was not recognized."
                        )
  where
    correctMsg Nothing  = "Answer was correct!"
    correctMsg (Just r) =
        printf "Answer was correct, and you made the global leaderboard at rank %d !!"
          r
    incorrectMsg t h =
        printf "Answer was incorrect!%s  Please wait %d before submitting again"
          hintStr
          (t `div` 60)
      where
        hintStr :: String
        hintStr = case h of
          Nothing -> ""
          Just s  -> printf "  Hint: Answer was %s." s

runAll
    :: Maybe String                             -- ^ session key
    -> Integer                                  -- ^ year
    -> Bool                                     -- ^ run and lock answer
    -> (Finite 25 -> Part -> IO (Maybe String))   -- ^ replacements
    -> ChallengeMap
    -> (SomeSolution -> Maybe String -> ChallengeData -> IO a)  -- ^ callback. given solution, "replacement" input, and data
    -> IO (Map (Finite 25) (Map Part a))
runAll sess yr lock rep cm f = flip M.traverseWithKey cm $ \d ->
                               M.traverseWithKey $ \p c -> do
    let CP{..} = challengePaths (CS d p)
    inp0 <- rep d p
    withColor ANSI.Dull ANSI.Blue $
      printf ">> Day %02d%c\n" (dayToInt d) (partChar p)
    when lock $ do
      CD{..} <- challengeData sess yr (CS d p)
      forM_ (inp0 <|> eitherToMaybe _cdInput) $ \inp ->
        mapM_ (writeFile _cpAnswer) =<< evaluate (force (runSomeSolution c inp))
    f c inp0 =<< challengeData sess yr (CS d p)

runTestSuite :: SomeSolution -> ChallengeData -> IO (Maybe Bool)
runTestSuite c CD{..} = do
    testRes <- mapMaybe fst <$> mapM (uncurry (testCase True c)) _cdTests
    unless (null testRes) $ do
      let (mark, color)
              | and testRes = ('✓', ANSI.Green)
              | otherwise   = ('✗', ANSI.Red  )
      withColor ANSI.Vivid color $
        printf "[%c] Passed %d out of %d test(s)\n"
            mark
            (length (filter id testRes))
            (length testRes)
    pure $ and testRes <$ guard (not (null testRes))


-- | Run a single test case
testCase
    :: Bool             -- ^ is just an example
    -> SomeSolution
    -> String
    -> TestMeta
    -> IO (Maybe Bool, Either SolutionError String)
testCase emph c inp TM{..} = do
    withColor ANSI.Dull color $
      printf "[%c]" mark
    if emph
      then printf " (%s)\n" resStr
      else printf " %s\n"   resStr
    forM_ showAns $ \a ->
      withColor ANSI.Vivid ANSI.Red $
        printf "(Expected: %s)\n" a
    return (status, res)
  where
    res = runSomeSolutionWith _tmData c inp
    resStr = case res of
      Right r -> r
      Left SEParse -> "ERROR: No parse"
      Left SESolve -> "ERROR: No solution"
    (mark, showAns, status) = case _tmAnswer of
      Just (strip->ex)    -> case res of
        Right (strip->r)
          | r == ex   -> ('✓', Nothing, Just True )
          | otherwise -> ('✗', Just ex, Just False)
        Left _        -> ('✗', Just ex, Just False)
      Nothing         -> ('?', Nothing, Nothing   )
    color = case status of
      Just True  -> ANSI.Green
      Just False -> ANSI.Red
      Nothing    -> ANSI.Blue

-- | Do the action with a given ANSI foreground color and intensity.
withColor
    :: ANSI.ColorIntensity
    -> ANSI.Color
    -> IO ()
    -> IO ()
withColor ci c act = do
    ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ci c ]
    act
    ANSI.setSGR [ ANSI.Reset ]

pullMap
    :: Map a (Map b c)
    -> Map (a, b) c
pullMap = M.fromDistinctAscList
        . concatMap (uncurry go . second M.toAscList)
        . M.toAscList
  where
    go x = (map . first) (x,)

pushMap
    :: Eq a
    => Map (a, b) c
    -> Map a (Map b c)
pushMap = fmap M.fromDistinctAscList
        . M.fromAscListWith (flip (++))
        . map (uncurry go)
        . M.toAscList
  where
    go (x, y) z = (x, [(y, z)])
