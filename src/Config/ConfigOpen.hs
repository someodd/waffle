-- | Config stuff for open associations...

module Config.ConfigOpen where

--import           Data.Text.Encoding.Error       (lenientDecode)
--import           Data.Text.Encoding            as E
--import qualified Data.ByteString.Char8 as Char8
import           System.FilePath
import           System.Directory

--import           Data.FileEmbed
import           Data.ConfigFile

import           Config                            ( getConfigDirectory, readConfigParser, doIfPathDoesntExist )
import           Paths_waffle

-- FIXME: check if file already exists
-- TODO: maybe should be setupFactoryOpenConfig
setupDefaultOpenConfig :: IO ()
setupDefaultOpenConfig = do
  userOpenConfigPath <- getUserOpenConfigPath
  defaultOpenConfigPath <- getDefaultOpenConfigPath
  doIfPathDoesntExist userOpenConfigPath (copyFile defaultOpenConfigPath userOpenConfigPath)

getDefaultOpenConfigPath :: IO FilePath
getDefaultOpenConfigPath = getDataFileName "data/open.ini"

-- | Get the `FilePath` to the user's open/associations configuration file.
getUserOpenConfigPath :: IO FilePath
getUserOpenConfigPath = do
  configDir <- getConfigDirectory
  pure $ joinPath [configDir, "open.ini"]

-- | The default open.ini list of associations between item types and
-- commands to open them. This is the default config file included
-- with Waffle, not the user's config.
getDefaultOpenConfig :: IO ConfigParser
getDefaultOpenConfig = getDataFileName "data/open.ini" >>= readConfigParser

-- | The default open.ini list of associations between item types and
-- commands to open them.
getUserOpenConfig :: IO ConfigParser
getUserOpenConfig = getUserOpenConfigPath >>= readConfigParser

--E.decodeUtf8With lenientDecode contents
