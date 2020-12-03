#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-16 --package template --package text --package filepath --package directory -- -Wall

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Text.Template
import           System.FilePath
import           System.Directory
import           Text.Printf
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.Text.Lazy.IO  as TL

outRoot :: FilePath
outRoot = "src/AOC/Challenge"

main :: IO ()
main = do
    temp <- template <$> T.readFile "template/DayXX.hs"
    forM_ [1..25] $ \i -> do
      let newFilePath = outRoot </> printf "Day%02d.hs" i
          Just newFile = renderA temp (ctx i)
      skip <- doesFileExist newFilePath
      unless skip $
        TL.writeFile newFilePath newFile

ctx :: Int -> ContextA Maybe
ctx i = \case
  "day"       -> Just . T.pack $ printf "%02d" i
  "day_short" -> Just . T.pack $ printf "%d"   i
  _           -> Nothing
