{-# LANGUAGE TemplateHaskell #-}

-- | Configuration for themes, namely handling theme.ini.

module Config.Theme ( setupDefaultTheme, getUserThemePath ) where

import           System.FilePath
import qualified Data.ByteString.Lazy as BL

import           Data.FileEmbed

import           Config                            ( getConfigDirectory, doIfPathDoesntExist )

-- | The standard theme.ini which ships with waffle; before it gets copied
-- to the user's config directory.
defaultThemeContents :: BL.ByteString
defaultThemeContents = BL.fromStrict $(embedFile "data/theme.ini")

-- | Writes a theme.ini to the user's config directory if they don't have one already.
setupDefaultTheme :: IO ()
setupDefaultTheme = do
  userThemePath <- getUserThemePath
  doIfPathDoesntExist userThemePath (BL.writeFile userThemePath defaultThemeContents)

-- | Get the `FilePath` to the user's open/associations configuration file.
getUserThemePath :: IO FilePath
getUserThemePath = do
  configDir <- getConfigDirectory
  pure $ joinPath [configDir, "theme.ini"]
