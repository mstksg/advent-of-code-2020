-- |
-- Module      : AOC.Run.Config
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Utilities for loading configuration file.
--


module AOC.Run.Config (
    Config(..), configFile, defConfPath
  , session
  ) where

import           Control.Exception
import           Control.Monad
import           Data.Default.Class
import           GHC.Generics      (Generic)
import           System.IO.Error
import           Text.Printf
import qualified Data.Aeson        as A
import qualified Data.ByteString   as BS
import qualified Data.Yaml         as Y


-- | Configuration for auto-runner.
data Config = Cfg
    { _cfgSession :: Maybe String     -- ^ Default: 'Nothing'
    , _cfgYear    :: Integer          -- ^ Default: 2015
    }
  deriving (Generic)

-- | No session key, and 2015.
instance Default Config where
    def = Cfg { _cfgSession = Nothing
              , _cfgYear    = 2015
              }

-- | Default math to find a configuration file.
defConfPath :: FilePath
defConfPath = "aoc-conf.yaml"

-- | Load a 'Config' from a given filepath.
configFile :: FilePath -> IO Config
configFile fp = do
    cfgInp <- tryJust (guard . isDoesNotExistError)
            $ BS.readFile fp
    case cfgInp of
      Left () -> do
        Y.encodeFile @Config fp def
        return def
      Right b ->
        case Y.decodeEither' b of
          Left e -> do
            printf "Configuration file at %s could not be parsed:\n" fp
            print e
            return def
          Right cfg -> return cfg

-- | Load a session token from the configuration file at a given filepath.
session :: FilePath -> IO (Maybe String)
session = fmap _cfgSession . configFile

configJSON :: A.Options
configJSON = A.defaultOptions
    { A.fieldLabelModifier = A.camelTo2 '-' . drop 4 }

instance A.ToJSON Config where
    toJSON     = A.genericToJSON configJSON
    toEncoding = A.genericToEncoding configJSON
instance A.FromJSON Config where
    parseJSON  = A.genericParseJSON configJSON
