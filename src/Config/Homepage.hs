{-# LANGUAGE TemplateHaskell #-}

-- Module for managing a homepage.

module Config.Homepage where

import Data.Maybe (fromMaybe)
import           Control.Monad.Except
import           System.FilePath
import qualified Data.ByteString.Lazy as BL

import           Data.FileEmbed
import           Data.ConfigFile

import           Config                            ( customEmptyCP, getConfigDirectory, readConfigParser, doIfPathDoesntExist )

defaultHomepageConfig :: BL.ByteString
defaultHomepageConfig = BL.fromStrict $(embedFile "data/homepage.ini")

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

setHomepage :: String -> Maybe String -> IO ()
setHomepage homepageURI maybeDisplayString = do
  userHomepagePath <- getUserHomepageConfigPath
  -- Trust me, I know how this looks, but that's how the `Data.ConfigFile` author
  -- wants it done. It's an interface I don't appreciate much.
  outCP <- runExceptT $
         do
         cp <- join $ liftIO $ readfile customEmptyCP userHomepagePath
         cp' <- set cp "homepage" "uri" (homepageURI)
         cp'' <- set cp' "homepage" "display" (fromMaybe homepageURI maybeDisplayString)
         pure cp''

  -- Handle errors from the building the output configuration parser
  case outCP of
    Left (exception, _) ->
      -- FIXME: needs to catch the other exceptions...
      case exception of
       -- Rewrite in case of the section already existing!
       SectionAlreadyExists _'         -> pure ()
       ParseError errorMessage         -> error $ "Parse error: " ++ errorMessage
       NoSection errorMessage          -> error $ "No such section: " ++ errorMessage
       NoOption errorMessage           -> error $ "No such option: " ++ errorMessage -- FIXME: this should never happen.
       InterpolationError errorMessage -> error $ "No such option: " ++ errorMessage -- FIXME: this should never happen.
       OtherProblem errorMessage       -> error $ "Error: " ++ errorMessage
    Right cp -> do
      outPath <- getUserHomepageConfigPath
      writeFile outPath (to_string cp)


