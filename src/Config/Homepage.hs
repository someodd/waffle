{-# LANGUAGE TemplateHaskell #-}

-- Module for managing a homepage.

module Config.Homepage where

import           System.FilePath
import qualified Data.ByteString.Lazy as BL

import           Data.FileEmbed
import           Data.ConfigFile

import           Config                            ( getConfigDirectory, readConfigParser, doIfPathDoesntExist )

defaultHomepageConfig :: BL.ByteString
defaultHomepageConfig = BL.fromStrict $(embedFile "data/homepage.ini")

-- TODO: maybe should be setupFactoryOpenConfig
setupDefaultHomepageConfig :: IO ()
setupDefaultHomepageConfig = do
  userHomepageConfigPath <- getUserHomepageConfigPath
  doIfPathDoesntExist userHomepageConfigPath (BL.writeFile userHomepageConfigPath defaultHomepageConfig)

-- | Get the `FilePath` to the user's open/associations configuration file.
getUserHomepageConfigPath :: IO FilePath
getUserHomepageConfigPath = do
  configDir <- getConfigDirectory
  pure $ joinPath [configDir, "homepage.ini"]

-- | The default open.ini list of associations between item types and
-- commands to open them.
getUserHomepageConfig :: IO ConfigParser
getUserHomepageConfig = getUserHomepageConfigPath >>= readConfigParser
