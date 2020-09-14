{-# LANGUAGE TemplateHaskell #-}

-- Module for managing a homepage.

module Config.Homepage where

{-
import           System.FilePath
import qualified Data.ByteString.Lazy as BL

import           Data.FileEmbed
import           Data.ConfigFile

import           Config                            ( getConfigDirectory, readConfigParser, doIfPathDoesntExist )

defaultOpenConfig :: BL.ByteString
defaultOpenConfig = BL.fromStrict $(embedFile "data/open.ini")

-- TODO: maybe should be setupFactoryOpenConfig
setupDefaultOpenConfig :: IO ()
setupDefaultOpenConfig = do
  userOpenConfigPath <- getUserOpenConfigPath
  doIfPathDoesntExist userOpenConfigPath (BL.writeFile userOpenConfigPath defaultOpenConfig)

-- | Get the `FilePath` to the user's open/associations configuration file.
getUserOpenConfigPath :: IO FilePath
getUserOpenConfigPath = do
  configDir <- getConfigDirectory
  pure $ joinPath [configDir, "open.ini"]

-- | The default open.ini list of associations between item types and
-- commands to open them.
getUserOpenConfig :: IO ConfigParser
getUserOpenConfig = getUserOpenConfigPath >>= readConfigParser
-}
