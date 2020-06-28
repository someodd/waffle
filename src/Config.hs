module Config where

import           System.Directory
import           System.FilePath

--import           Data.FileEmbed
import           Data.ConfigFile

-- | Basically `emptyCP`, but doesn't toLower the options.
customEmptyCP :: ConfigParser
customEmptyCP = emptyCP { optionxform = id }

-- | Get the directory where Waffle's configs should be located.
getConfigDirectory :: IO FilePath
getConfigDirectory = do
  homeDirectory <- getHomeDirectory
  pure $ joinPath [homeDirectory, ".config", "waffle"]

doIfPathDoesntExist :: FilePath -> IO () -> IO ()
doIfPathDoesntExist filePath someAction = do
  pathExists <- doesPathExist filePath
  if pathExists
    then pure ()
    else someAction

-- | Create Waffle's config directory.
setupConfigDirectory :: IO ()
setupConfigDirectory = do
  configDirectory <- getConfigDirectory
  doIfPathDoesntExist configDirectory (createDirectory configDirectory)

-- | Create the default config for opening menu items (gophermaps) with
-- commands.
--setupOpenConfig :: IO ()
--setupOpenConfig = do

-- | Setup the config directory and all the associated configs.
--setupConfigs :: IO ()
--setupConfigs = do
--  let (kk

readConfigParser :: FilePath -> IO ConfigParser
readConfigParser filePath = do
  val <- readfile customEmptyCP filePath
  case val of
    Left readError -> error (show readError)
    Right cp       -> pure cp

readConfigParserValue :: ConfigParser -> SectionSpec -> OptionSpec -> IO String
readConfigParserValue configParser section option =
  let val = get configParser section option
  in  case val of
    Left readError -> error (show readError)
    Right cp       -> pure cp
