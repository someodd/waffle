-- | Open a file with the proper association set as a command in
-- the config.

module Open (openItem) where

import           System.Process                 (runCommand, ProcessHandle)

import           Config
import           Config.ConfigOpen
import           Gopher                         ( ItemType(..)
                                                )

openItemCommand :: ItemType -> IO String
openItemCommand itemType = do
  configParser <- getUserOpenConfig
  case itemType of
    Canonical canonType -> readConfigParserValue configParser "open-assocs" (show canonType)
    NonCanonical nonCanonType -> readConfigParserValue configParser "open-assocs" (show nonCanonType)

openItem :: ItemType -> FilePath -> IO ProcessHandle
openItem itemType filePath = do
  command <- openItemCommand itemType
  runCommand $ command ++ " \"" ++ filePath ++ "\""
