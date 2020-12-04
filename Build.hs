#!/usr/bin/env stack
-- stack --install-ghc runghc --package shake --package template --package directory --package containers --package text --package filepath --package strip-ansi-escape --package html-entities --package time

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wall          #-}

-- | Assemble README and Reflections

import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.String.AnsiEscapeCodes.Strip.Text
import           Data.Text                              (Text)
import           Data.Text.Template
import           Data.Time
import           Development.Shake
import           Development.Shake.FilePath
import           Text.Printf
import           Text.Read
import qualified Data.Map                               as M
import qualified Data.Set                               as S
import qualified Data.Text                              as T
import qualified Data.Text.Lazy                         as TL
import qualified HTMLEntities.Text                      as H

-- CONSTANTS
year :: Integer
year = 2020
github :: String
github = "mstksg"
otherYears :: S.Set Integer
otherYears = S.fromList [2016 .. 2020]

ctx0 :: M.Map Text Text
ctx0 = M.fromList [
    ("year"  , T.pack (show year)                                   )
  , ("github", T.pack github                                        )
  , ("name"  , "Justin Le"                                          )
  , ("email" , "justin@jle.im"                                      )
  , ("rss"   , "http://feeds.feedburner.com/jle-advent-of-code-2020")
  , ("other_years", yearLinks                                       )
  ]

opts :: ShakeOptions
opts = shakeOptions { shakeFiles     = "_build"
                    , shakeVersion   = "1.0"
                    , shakeVerbosity = Chatty
                    , shakeThreads   = 1    -- for benchmarks to work properly
                    }

parseDayFp :: FilePath -> Maybe Int
parseDayFp = readMaybe . filter validChar . takeBaseName
  where
    validChar p = isDigit p || p == '_'

reflPath :: Int -> FilePath
reflPath d = "reflections" </> printf "day%02d.md" d
reflOutPath :: Int -> FilePath
reflOutPath d = "_reflections" </> printf "day%02d.md" d
reflOutCodedPath :: Int -> FilePath
reflOutCodedPath d = "_reflections" </> printf "day%02d-coded.md" d
reflXmlPath :: Int -> FilePath
reflXmlPath d = "_reflections" </> printf "day%02d.xml" d
benchPath :: Int -> FilePath
benchPath d = "bench-out" </> printf "day%02d.txt" d
standaloneReflectionPath :: Int -> FilePath
standaloneReflectionPath d = "reflections-out" </> printf "day%02d.md" d

main :: IO ()
main = shakeArgs opts $ do
    action $ do
      rd <- S.toList <$> reflectionDays
      need $ ["README.md", "reflections.md", "feed.xml"]
          ++ map standaloneReflectionPath rd

    "reflections.md" %> \fp -> do
        days   <- getDays
        bodies <- forM (M.toList days) $ \(d, hasRefl) ->
          if hasRefl
            then T.pack <$> readFile' (reflOutPath d)
            else T.pack <$> readFile' (reflOutCodedPath d)
        let toc = flip map (M.toList days) $ \(d, hasRefl) ->
              if hasRefl
                then printf "* [Day %d](#day-%d)" d d
                else printf "* [Day %d](#day-%d) *(no reflection yet)*" d d
            yearUrls = tUnlines' . flip foldMap otherYears $ \oy ->
              T.pack (printf "[%04d]: https://github.com/%s/advent-of-code-%04d/blob/master/reflections.md" oy github oy)
                 <$ guard (oy /= year)

            ctx = ctx0 <> M.fromList
              [ ("toc" , T.pack $ unlines' toc        )
              , ("body", T.intercalate "\n\n\n" bodies)
              , ("other_links", yearUrls              )
              ]
        writeTemplate fp ctx "template/reflections.md.template"

    "reflections-out/*.md" %> \fp -> do
        rDays <- S.toList <$> reflectionDays
        let Just d  = parseDayFp fp
            hasRefl = not $ "coded" `isInfixOf` fp
        refl   <- if hasRefl
          then T.pack <$> readFile' (reflPath  d)
          else pure "*Reflection not yet written -- please check back later!*"
        bench  <- T.pack <$> readFile' (benchPath d)
        let otherDays = T.intercalate " / " . flip map rDays $ \od ->
                let linker :: String
                    linker
                      | od == d   = printf "%d" od
                      | otherwise = printf "[%d][day%02d]" od od
                in  T.pack $ printf "*%s*" linker
            dayLinks = tUnlines' . flip mapMaybe rDays $ \od ->
              T.pack
                (printf "[day%02d]: https://github.com/%s/advent-of-code-%04d/blob/master/%s" od github year (standaloneReflectionPath od))
                  <$ guard (od /= d)
            ctx = ctx0 <> M.fromList
              [ ("daylong"   , T.pack $ printf "%02d" d)
              , ("dayshort"  , T.pack $ printf "%d" d  )
              , ("body"      , refl                    )
              , ("benchmarks", bench                   )
              , ("other_links", dayLinks               )
              , ("other_days" , otherDays              )
              ]
        writeTemplate fp ctx "template/standalone-reflection.md.template"


    "README.md" %> \fp -> do
        days <- getDays
        let mkRow d = case M.lookup d days of
              Just True ->
                  printf "| Day %2d    | [x][d%02dr]   | [x][d%02dg] | [x][d%02dh]  | [x][d%02db]  |"
                    d d d d d
              Just False ->
                  printf "| Day %2d    |             | [x][d%02dg] | [x][d%02dh]  | [x][d%02db]  |"
                    d d d d
              Nothing    ->
                  printf "| Day %2d    |             |           |            |            |"
                    d
            table = unlines' $
               "| Challenge | Reflections | Code      | Rendered   | Benchmarks |"
             : "| --------- | ----------- | --------- | ---------- | ---------- |"
             : map mkRow [1..25]
            links      = unlines' . M.foldMapWithKey mkLinks $ days
            yearUrls   = tUnlines' . flip foldMap otherYears $ \oy ->
                T.pack (printf "[%04d]: https://github.com/%s/advent-of-code-%04d" oy github oy)
                    <$ guard (oy /= year)
            ctx = ctx0 <> M.fromList
                [ ("table"      , T.pack table)
                , ("links"      , T.pack links)
                , ("other_links", yearUrls    )
                ]
        writeTemplate fp ctx "template/README.md.template"

    "feed.xml" %> \fp -> do
        days <- reflectionDays
        bodies <- forM (reverse (toList days)) $ \d ->
          T.pack <$> readFile' (reflXmlPath d)
        time <- utcToZonedTime (read "EST") <$> liftIO getCurrentTime
        let ctx = ctx0 <> M.fromList
              [ ("body", T.intercalate "\n" bodies)
              , ("time", T.pack . formatTime defaultTimeLocale rfc822DateFormat $ time )
              ]
        writeTemplate fp ctx "template/feed.xml.template"

    "_reflections/*.md" %> \fp -> do
        let Just d  = parseDayFp fp
            hasRefl = not $ "coded" `isInfixOf` fp
        refl   <- if hasRefl
          then T.pack <$> readFile' (reflPath  d)
          else pure "*Reflection not yet written -- please check back later!*"
        bench  <- T.pack <$> readFile' (benchPath d)
        let ctx = ctx0 <> M.fromList
              [ ("daylong"   , T.pack $ printf "%02d" d)
              , ("dayshort"  , T.pack $ printf "%d" d  )
              , ("body"      , refl                    )
              , ("benchmarks", bench                   )
              ]
        writeTemplate fp ctx "template/reflection.md.template"

    "_reflections/*.xml" %> \fp -> do
        let Just d = parseDayFp fp
        refl <- readFile' (reflOutPath d)
        Stdout html <- cmd ("pandoc -t html -f markdown" :: String)
                           (Stdin refl)
        let time = ZonedTime
                     (LocalTime (fromGregorian year 12 d)
                                (TimeOfDay 1 0 0)
                     )
                     (read "EST")
            ctx = ctx0 <> M.fromList
              [ ("day" , T.pack $ printf "%d" d )
              , ("body", H.text . T.pack $ html )
              , ("time", T.pack . formatTime defaultTimeLocale rfc822DateFormat $ time )
              ]
        writeTemplate fp ctx "template/feed-item.xml.template"

    "bench-out/*.txt" %> \fp -> do
        let Just d = parseDayFp fp
        Stdout out <- cmd ("stack run --" :: String) (printf "bench %d" d :: String)
        writeFileChanged fp . T.unpack . T.strip . stripAnsiEscapeCodes . T.pack $ out

    "clean" ~> do
      removeFilesAfter "_reflections" ["//*"]
      -- removeFilesAfter "bench-out" ["//*"]
      removeFilesAfter "_build" ["//*"]
      removeFilesAfter "reflections-out" ["//*"]
      removeFilesAfter "/" ["README.md", "reflections.md"]
  where
    reflectionDays = S.fromList . mapMaybe parseDayFp <$> getDirectoryFiles "reflections" ["*.md"]
    codedDays      = do
      ds <- mapMaybe parseDayFp <$> getDirectoryFiles codePath ["*.hs"]
      fmap S.fromList . flip filterM ds $ \d -> do
        let dayFile = codePath </> printf "Day%02d.hs" d
        not . ("AOC.Prelude" `T.isInfixOf`) . T.pack <$> readFile' dayFile
    getDays = do
      rd <- M.fromSet (const True ) <$> reflectionDays
      cd <- M.fromSet (const False) <$> codedDays
      pure $ M.unionWith (||) rd cd
    writeTemplate fp ctx templ = do
      out <- flip substitute (ctx M.!) . T.pack <$> readFile' templ
      writeFileChanged fp (TL.unpack out)

yearLinks :: Text
yearLinks  = T.intercalate " / " . flip map (S.toList otherYears) $ \oy ->
    let linker | oy == year = "%04d"
               | otherwise  = "[%04d][]"
    in  T.pack $ printf "*%s*" (printf linker oy :: String)

mkLinks :: Int -> Bool -> [String]
mkLinks d hasRef = catMaybes [
    Just $ printf "[d%02dg]: https://github.com/mstksg/advent-of-code-%04d/blob/master/src/AOC/Challenge/Day%02d.hs"
      d year d
  , Just $ printf "[d%02dh]: https://mstksg.github.io/advent-of-code-%04d/src/AOC.Challenge.Day%02d.html"
      d year d
  , do guard hasRef
       Just $ printf "[d%02dr]: https://github.com/mstksg/advent-of-code-%04d/blob/master/reflections.md#day-%d"
         d year d
  , Just $ printf "[d%02db]: https://github.com/mstksg/advent-of-code-%04d/blob/master/reflections.md#day-%d-benchmarks"
      d year d
  ]

unlines' :: [String] -> String
unlines' = intercalate "\n"
tUnlines' :: [Text] -> Text
tUnlines' = T.intercalate "\n"

codePath :: FilePath
codePath = "src/AOC/Challenge"
