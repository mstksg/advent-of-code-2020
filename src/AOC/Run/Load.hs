{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- |
-- Module      : AOC.Run.Load
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Loading challenge data and prompts.
--

module AOC.Run.Load (
    ChallengePaths(..), challengePaths
  , ChallengeData(..), challengeData
  , Day(..)
  , countdownConsole
  , countdownWithPrint
  , timeToRelease
  , showNominalDiffTime
  , charPart
  , showAoCError
  , htmlToMarkdown
  , mkDay, mkDay_, dayInt
  , TestMeta(..)
  -- * Parsers
  , parseMeta
  , parseTests
  ) where

import           AOC.Challenge
import           AOC.Util
import           Advent
import           Control.Applicative
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Char
import           Data.Dynamic
import           Data.Foldable
import           Data.Map                  (Map)
import           Data.Maybe
import           Data.Text                 (Text)
import           Data.Time hiding          (Day)
import           Data.Void
import           System.Console.ANSI       as ANSI
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Error
import           Text.Printf
import           Text.Read                 (readMaybe)
import qualified Control.Monad.Combinators as MP
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Text.Megaparsec           as MP
import qualified Text.Megaparsec.Char      as MP
import qualified Text.Pandoc               as P

-- | A record of paths corresponding to a specific challenge.
data ChallengePaths = CP { _cpPrompt    :: !FilePath
                         , _cpInput     :: !FilePath
                         , _cpAnswer    :: !FilePath
                         , _cpTests     :: !FilePath
                         , _cpLog       :: !FilePath
                         }
  deriving Show

-- | A record of data (test inputs, answers) corresponding to a specific
-- challenge.
data ChallengeData = CD { _cdPrompt :: !(Either [String] Text  )
                        , _cdInput  :: !(Either [String] String)
                        , _cdAnswer :: !(Maybe String)
                        , _cdTests  :: ![(String, TestMeta)]
                        }

-- | Generate a 'ChallengePaths' from a specification of a challenge.
challengePaths :: Integer -> ChallengeSpec -> ChallengePaths
challengePaths y (CS d p) = CP
    { _cpPrompt    = "prompt"          </> printf "%02d%c" d' p' <.> "md"
    , _cpInput     = "data"            </> printf "%02d" d'      <.> "txt"
    , _cpAnswer    = "data/ans"        </> printf "%02d%c" d' p' <.> "txt"
    , _cpTests     = "test-data"       </> printf "%04d/%02d%c" y d' p' <.> "txt"
    , _cpLog       = "logs/submission" </> printf "%02d%c" d' p' <.> "txt"
    }
  where
    d' = dayInt d
    p' = partChar p

makeChallengeDirs :: ChallengePaths -> IO ()
makeChallengeDirs CP{..} =
    mapM_ (createDirectoryIfMissing True . takeDirectory)
          [_cpPrompt, _cpInput, _cpAnswer, _cpTests, _cpLog]

-- | Load data associated with a challenge from a given specification.
-- Will fetch answers online and cache if required (and if giten a session
-- token).
challengeData
    :: Maybe String   -- ^ session key
    -> Integer        -- ^ year
    -> ChallengeSpec
    -> IO ChallengeData
challengeData sess yr spec = do
    makeChallengeDirs ps
    inp   <- runExceptT . asum $
      [ maybeToEither [printf "Input file not found at %s" _cpInput]
          =<< liftIO (readFileMaybe _cpInput)
      , fetchInput
      ]
    prompt <- runExceptT . asum $
      [ maybeToEither [printf "Prompt file not found at %s" _cpPrompt]
          =<< liftIO (fmap T.pack <$> readFileMaybe _cpPrompt)
      , fetchPrompt
      ]
    ans    <- readFileMaybe _cpAnswer
    ts     <- readFileMaybe _cpTests >>= \case
                Nothing  -> pure []
                Just str -> case MP.parse parseTests _cpTests str of
                  Left e  -> [] <$ putStrLn (MP.errorBundlePretty e)
                  Right r -> pure r
    return CD
      { _cdPrompt = prompt
      , _cdInput  = inp
      , _cdAnswer = ans
      , _cdTests  = ts
      }
  where
    ps@CP{..} = challengePaths yr spec
    readFileMaybe :: FilePath -> IO (Maybe String)
    readFileMaybe =
        (traverse (evaluate . force) . eitherToMaybe =<<)
       . tryJust (guard . isDoesNotExistError)
       . readFile
    fetchInput :: ExceptT [String] IO String
    fetchInput = do
        s <- maybeToEither ["Session key needed to fetch input"]
              sess
        let opts = defaultAoCOpts yr s
        inp <- liftEither . bimap showAoCError T.unpack
           =<< liftIO (runAoC opts a)
        liftIO $ writeFile _cpInput inp
        pure inp
      where
        a = AoCInput $ _csDay spec
    fetchPrompt :: ExceptT [String] IO Text
    fetchPrompt = do
        prompts <- liftEither . first showAoCError
               =<< liftIO (runAoC opts a)
        promptH  <- maybeToEither [e]
                 . M.lookup (_csPart spec)
                 $ prompts
        prompt   <- liftEither $ htmlToMarkdown True promptH
        liftIO $ T.writeFile _cpPrompt prompt
        pure prompt
      where
        opts = defaultAoCOpts yr $ fold sess
        a = AoCPrompt $ _csDay spec
        e = case sess of
          Just _  -> "Part not yet released"
          Nothing -> "Part not yet released, or may require session key"
      -- where
      --   go (inp:meta:xs) =

        -- go [] = []
    -- parseTests xs = case break (">>>" `isPrefixOf`) xs of
    --   (inp,[])
    --     | null (strip (unlines inp))  -> []
    --     | otherwise -> [(unlines inp, Nothing)]
    --   (inp,(strip.drop 4->ans):rest)
    --     | null (strip (unlines inp))  -> parseTests rest
    --     | otherwise ->
    --         let ans' = ans <$ guard (not (null ans))
    --         in  (unlines inp, ans') : parseTests rest


showAoCError :: AoCError -> [String]
showAoCError = \case
    AoCClientError e -> [ "Error contacting Advent of Code server to fetch input"
                        , "Possible invalid session key"
                        , printf "Server response: %s" (show e)
                        ]
    AoCReleaseError t -> [ "Challenge not yet released!"
                         , printf "Please wait %s" (showNominalDiffTime t)
                         ]
    AoCThrottleError  -> [ "Too many requests at a time.  Please slow down." ]

-- | Pretty-print a 'NominalDiffTime'
showNominalDiffTime :: NominalDiffTime -> String
showNominalDiffTime (round @Double @Int . realToFrac -> rawSecs) =
    printf "%02dd %02d:%02d:%02d" days hours mins secs
  where
    (rawMins , secs ) = rawSecs  `divMod` 60
    (rawHours, mins ) = rawMins  `divMod` 60
    (days    , hours) = rawHours `divMod` 24

-- | Run a countdown on the console.
countdownConsole
    :: MonadIO m
    => Integer          -- ^ year of challenge
    -> Day              -- ^ day to count down to
    -> m a              -- ^ callback on release
    -> m a
countdownConsole yr d = countdownWithPrint
    (liftIO $ timeToRelease yr d)
    250000
    (printf "Day %d release" (dayInt d))

countdownWithPrint
    :: MonadIO m
    => m NominalDiffTime
    -> Int
    -> String
    -> m a
    -> m a
countdownWithPrint getNDT delay cbstr = countdownWith getNDT delay $ \ttr -> liftIO $ do
    ANSI.clearFromCursorToScreenEnd
    printf "> %s in: %s" cbstr (showNominalDiffTime ttr)
    ANSI.setCursorColumn 0
    hFlush stdout

-- | Run a countdown with a given callback on each tick.
countdownWith
    :: MonadIO m
    => m NominalDiffTime            -- ^ get time
    -> Int                          -- ^ interval (milliseconds)
    -> (NominalDiffTime -> m ())    -- ^ callback on each tick
    -> m a                          -- ^ callback on finish
    -> m a
countdownWith getNDT delay callback release = go
  where
    go = do
      ttr <- getNDT
      if ttr <= 0
        then release
        else do
          callback ttr
          liftIO $ threadDelay delay
          go


htmlToMarkdown :: Bool -> Text -> Either [String] T.Text
htmlToMarkdown pretty html = first ((:[]) . show) . P.runPure $ do
    p <- P.readHtml (P.def { P.readerExtensions = exts })
            html
    writer (P.def { P.writerExtensions = exts }) p
  where
    writer
      | pretty    = P.writeMarkdown
      | otherwise = P.writePlain
    exts = P.disableExtension P.Ext_header_attributes
         . P.disableExtension P.Ext_smart
         $ P.pandocExtensions






type Parser = MP.Parsec Void String






data TestMeta = TM { _tmAnswer :: Maybe String
                   , _tmData   :: Map String Dynamic
                   }
  deriving Show

data MetaLine = MLData   String Dynamic
              | MLAnswer String
  deriving Show


parseTests :: Parser [(String, TestMeta)]
parseTests = MP.many parseTest <* MP.eof
  where
    parseTest = do
      inp <- MP.manyTill MP.anySingle $ MP.lookAhead (MP.string ">>>")
      met <- optional (MP.try parseMeta) MP.<?> "Metadata Block"
      pure (inp, fromMaybe (TM Nothing M.empty) met)

parseMeta :: Parser TestMeta
parseMeta = do
    dats <- MP.many (MP.try parseData) MP.<?> "Data Block"
    ans  <- optional (MP.try parseAnswer) MP.<?> "Expected Answer"
    pure $ TM ans (M.fromList dats)
  where
    parseAnswer = MP.string ">>>"
               *> MP.space1
               *> MP.many (MP.noneOf ['\n'])
               <* "\n"
    parseData = do
      MP.string ">>>"
      sym <- MP.manyTill (MP.try MP.letterChar)   (MP.try (MP.char ':'))
      val <- MP.manyTill (MP.try MP.alphaNumChar) (MP.try (MP.char ':'))
      typ <- MP.many     (MP.try MP.letterChar)
      MP.space
      case toLower <$> typ of
        "int"    -> maybe (fail "Could not parse metadata value") (pure . (sym,) . toDyn)
                  . readMaybe @Int
                  $ val
        "string" -> pure (sym, toDyn val)
        "text"   -> pure (sym, toDyn (T.pack val))
        _        -> fail $ "Unrecognized type " ++ typ

