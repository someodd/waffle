 {-# LANGUAGE TemplateHaskell #-}

-- | Config stuff for open associations...

module Config.Bookmarks where

import Control.Monad.Except
import           System.FilePath
import qualified Data.ByteString.Lazy          as BL

import           Data.FileEmbed
import           Data.ConfigFile

import           Config
  ( getConfigDirectory
  , readConfigParser
  , doIfPathDoesntExist
  , customEmptyCP
  )

defaultBookmarks :: BL.ByteString
defaultBookmarks = BL.fromStrict $(embedFile "data/bookmarks.ini")

-- TODO: maybe should be setupFactoryBookmarks
setupDefaultBookmarks :: IO ()
setupDefaultBookmarks = do
  userBookmarksPath <- getUserBookmarksPath
  doIfPathDoesntExist userBookmarksPath (BL.writeFile userBookmarksPath defaultBookmarks)

-- | Get the `FilePath` to the user's open/associations configuration file.
getUserBookmarksPath :: IO FilePath
getUserBookmarksPath = do
  configDir <- getConfigDirectory
  pure $ joinPath [configDir, "bookmarks.ini"]

-- | The default open.ini list of associations between item types and
-- commands to open them.
getUserBookmarks :: IO ConfigParser
getUserBookmarks = getUserBookmarksPath >>= readConfigParser

-- FIX: Should be (SectionSpec, HostName, PortNumber, Resource, ItemChar...
type BookmarkEntry = (SectionSpec, String, Int, String, Char)

addBookmark :: BookmarkEntry -> IO ()
addBookmark (sectionSpec, host, port, resource, itemType) = do
  userBookmarksPath <- getUserBookmarksPath
  -- Trust me, I know how this looks, but that's how the `Data.ConfigFile` author
  -- wants it done. It's an interface I don't appreciate much.
  outCP <- runExceptT $
         do
         cp      <- join $ liftIO $ readfile customEmptyCP userBookmarksPath
         cp'     <- add_section cp sectionSpec
         cp''    <- set cp' sectionSpec "host" (host)
         cp'''   <- set cp'' sectionSpec "port" (show port)
         cp''''  <- set cp''' sectionSpec "resource" resource
         cp''''' <- set cp'''' sectionSpec "type" (itemType:[])
         pure cp'''''

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
      outPath <- getUserBookmarksPath
      writeFile outPath (to_string cp)

-- | Remove a bookmark from the user's bookmark configuration.
removeBookmark :: SectionSpec -> IO ()
removeBookmark sectionSpec = do
  userBookmarks <- getUserBookmarks
  outPath <- getUserBookmarksPath
  let outCp = case remove_section userBookmarks sectionSpec of
                Left operationError -> error (show operationError)
                Right cp            -> cp
  writeFile outPath (to_string outCp)
