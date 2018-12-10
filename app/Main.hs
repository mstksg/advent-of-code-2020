{-# LANGUAGE OverloadedStrings              #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

import           AOC
import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Lens.Micro
import           Options.Applicative
import           System.IO.Error
import           Text.Printf
import           Text.Read
import qualified Data.Map             as M
import qualified System.Console.ANSI  as ANSI

data Mode = MRun    MainRunOpts
          | MView   MainViewOpts
          | MSubmit MainSubmitOpts

data Opts = O { _oMode   :: !Mode
              , _oConfig :: !(Maybe FilePath)
              }

main :: IO ()
main = do
    inputCache <- newIORef Nothing
    O{..} <- execParser $ info (parseOpts inputCache <**> helper)
                ( fullDesc
               <> header "aoc-dev - Advent of Code interactive development environment"
               <> progDesc ("Run, test, bench, challenges from Advent of Code, and view prompts. Available days: " ++ availableDays)
                )
    cfg@Cfg{..} <- configFile $ fromMaybe defConfPath _oConfig
    out <- runExceptT $ case _oMode of
      MRun    mro -> void $ mainRun  cfg mro
      MView   mvo -> void $ mainView cfg mvo
      MSubmit mso -> void $ mainSubmit cfg mso
    forOf_ _Left out $ \e -> do
      withColor ANSI.Vivid ANSI.Red $
        putStrLn "[ERROR]"
      mapM_ putStrLn e
  where
    availableDays = intercalate ", "
                  . map (show . dayToInt)
                  . M.keys
                  $ challengeMap


-- ---------
-- | Parsers
-- ---------

readFinite :: ReadM (Finite 25)
readFinite = eitherReader $ \s -> do
    n <- maybe (Left "Invalid day") Right $ readMaybe s
    maybe (Left "Day out of range") Right $ packFinite (n - 1)

readPart :: ReadM Part
readPart = eitherReader $ \case
    ""  -> Left "No part"
    "a" -> Right Part1
    "b" -> Right Part2
    _   -> Left "Invalid part (not 'a' or 'b')"

parseChallengeSpec :: Parser ChallengeSpec
parseChallengeSpec = do
    d <- argument pDay ( metavar "DAY"
                      <> help "Day of challenge (1 - 25)"
                       )
    p <- argument pPart ( metavar "PART"
                      <> help "Challenge part (a, b, c, etc.)"
                        )
    pure $ CS d p
  where
    pDay  = readFinite
    pPart = readPart

parseTestSpec :: Parser TestSpec
parseTestSpec = do
    d <- argument pDay ( metavar "DAY"
                      <> help "Day of challenge (1 - 25), or \"all\""
                       )
    p <- optional $ argument pPart ( metavar "PART"
                                  <> help "Challenge part (a, b, c, etc.)"
                                   )
    pure $ case d of
      Just d' -> case p of
        Just p' -> TSDayPart (CS d' p')
        Nothing -> TSDayAll  d'
      Nothing -> TSAll
  where
    pDay = asum [ Nothing <$ maybeReader (guard . (== "all") . map toLower)
                , Just <$> readFinite
                ]
    pPart = readPart

parseOpts
    :: IORef (Maybe (Maybe (Finite 25, String)))
    -> Parser Opts
parseOpts inputCache = do
    _oConfig <- optional . strOption . mconcat $
      [ long "config"
      , metavar "PATH"
      , help $ printf "Path to configuration file (default: %s)" defConfPath
      ]
    _oMode <- subparser . mconcat $
      [ command "run"       $
          info (MRun    <$> parseRun       <**> helper) (progDesc "Run, test, and benchmark challenges"   )
      , command "view"      $
          info (MView   <$> parseView      <**> helper) (progDesc "View a prompt for a given challenge"   )
      , command "submit"    $
          info (MSubmit <$> parseSubmit    <**> helper) (progDesc "Test and submit answers for challenges")
      , command "test"      $
          info (MRun    <$> parseTest      <**> helper) (progDesc "Alias for run --test"                  )
      , command "bench"     $
          info (MRun    <$> parseBench     <**> helper) (progDesc "Alias for run --bench"                 )
      , command "countdown" $
          info (MView   <$> parseCountdown <**> helper) (progDesc "Alias for view --countdown"            )
      ]
    pure O{..}
  where
    parseRun    :: Parser MainRunOpts
    parseRun = do
        _mroSpec <- parseTestSpec
        _mroTest <- switch . mconcat $
          [ long "test"
          , short 't'
          , help "Run sample tests"
          ]
        _mroBench <- switch . mconcat $
          [ long "bench"
          , short 'b'
          , help "Run benchmarks"
          ]
        _mroLock <- switch . mconcat $
          [ long "lock"
          , short 'l'
          , help "Lock in results as \"correct\" answers"
          ]
        takeStdin <- switch . mconcat $
          [ long "stdin"
          , help "Take first input from stdin instead of input directory"
          ]
        pure $ let _mroInput
                      | takeStdin = \d _ -> pullStdin inputCache d
                      | otherwise = \_ _ -> pure Nothing
               in  MRO{..}
    parseView   :: Parser MainViewOpts
    parseView = do
        _mvoSpec <- parseTestSpec
        _mvoWait <- switch . mconcat $
          [ long "countdown"
          , short 'c'
          , help "Countdown until release if not yet available"
          ]
        pure MVO{..}
    parseSubmit :: Parser MainSubmitOpts
    parseSubmit = do
        _msoSpec <- parseChallengeSpec
        _msoTest <- fmap not . switch . mconcat $
          [ long "skip-tests"
          , short 's'
          , help "Skip running tests before submission"
          ]
        _msoForce <- switch . mconcat $
          [ long "force"
          , short 'f'
          , help "Always submit, even if tests fail"
          ]
        _msoLock <- fmap not . switch . mconcat $
          [ long "no-lock"
          , short 'n'
          , help "Do not lock in answer, even if correct submission was received"
          ]
        pure MSO{..}
    parseTest  :: Parser MainRunOpts
    parseTest  = parseRun & mapped . mroTest  .~ True
    parseBench :: Parser MainRunOpts
    parseBench = parseRun & mapped . mroBench .~ True
    parseCountdown :: Parser MainViewOpts
    parseCountdown = parseView & mapped . mvoWait .~ True

pullStdin
    :: IORef (Maybe (Maybe (Finite 25, String)))  -- ^ Nothing: first time; Just Nothing: failed forever.
    -> Finite 25
    -> IO (Maybe String)
pullStdin inputCache d = readIORef inputCache >>= \case
    Nothing -> do
      out <- fmap eitherToMaybe . tryJust (guard . isEOFError) $
                  evaluate . force =<< getContents
      writeIORef inputCache . Just $ (d,) <$> out
      pure out
    Just old -> pure . findMaybe (\(d',o) -> o <$ guard (d == d')) $ old
