-- FIXME, TODO: history shouldn't be in here. who knows if you're refreshing or not! It's just
-- a weird mixture of responsibilities! otherwise just make a toggle for it as an arg? or bool?
-- Can we just use temp files created for caching? maybe in that case we should also stop
-- using memory string and always download temp files for everything...
-- TODO: could even implement the loader in the status bar instead of new screen... what about cancelling loading also
-- TODO: document

-- | Handle indication of download progress for various UI.Util.RenderMode types, like downloading
-- menus, text files, and binary file downloads.
module UI.Progress where

import           Data.Maybe
import qualified Data.ByteString               as ByteString
import           Control.Concurrent             ( forkIO )
import           System.Directory               ( renameFile )

import           System.FilePath                ( takeFileName )
import           Network.Simple.TCP
import qualified Brick.Widgets.FileBrowser     as FB
import           Brick.Widgets.Core             ( str )
import qualified Brick.Main                    as M
import qualified Brick.BChan
import qualified Data.ByteString.Char8         as B8
import qualified Brick.Types                   as T
import           System.IO.Temp                 ( emptySystemTempFile )
import qualified Data.ByteString.UTF8          as U8
import qualified Codec.Binary.UTF8.String      as UString

import           UI.Util
import           UI.History--TODO: make this in top level not just UI? or it's all app state so idk
import           UI.Representation
import           GopherClient

selectNothing :: FB.FileInfo -> Bool
selectNothing _ = False

initProgressMode :: GopherBrowserState -> Maybe History -> Location -> IO GopherBrowserState
initProgressMode gbs history location@(_, _, _, mode) =
  let
    (downloader, message) = case mode of
      TextFileMode    -> (progressCacheable, "text file 📄")
      MenuMode        -> (progressCacheable, "menu 📂")
      FileBrowserMode -> (progressDownloadBytes, "binary file")
      m -> error $ "Unsupported mode requested for progress mode: " ++ show m
    initialProgGbs = gbs
      { gbsRenderMode = ProgressMode
      , gbsBuffer     = ProgressBuffer $ Progress
                          { pbBytesDownloaded = 0
                          , pbInitGbs         = gbs
                          , pbConnected       = False
                          , pbMessage         = "Downloading a " ++ message
                          }
      }
  in
    forkIO (downloader initialProgGbs history location) >> pure initialProgGbs

addProgBytes :: GopherBrowserState -> Int -> GopherBrowserState
addProgBytes gbs' nbytes =
  let cb x = x
        { pbBytesDownloaded = pbBytesDownloaded (getProgress gbs') + nbytes
        , pbConnected       = True
        }
  in  updateProgressBuffer gbs' cb

-- | Download bytes via Gopher, using progress events to report status. Eventually
-- gives back the path to the new temporary file it has created. This is used to
-- download bytes and create a new temp/cache file based on the download, while
-- handling progress events. This is not for downloading binary files, but instead
-- for downloading textual data to be displayed by Waffle.
--
-- If the history argument is Nothing, then the new history will be updated with the
-- new location. Otherwise the history supplied will be used in the application state.
-- This is important for refreshing or navigating history (you don't want to update
-- the history in those cases, so you supply Nothing).
progressGetBytes :: GopherBrowserState -> Maybe History -> Location -> IO ()
progressGetBytes initialProgGbs history location@(host, port, resource, mode)
  = connect host (show port) $ \(connectionSocket, _) -> do
    -- Send the magic/selector string (request a path) to the websocket we're connected to.
    -- This allows us to later receive the bytes located at this "path."
    send connectionSocket (B8.pack $ resource ++ "\r\n")
    -- Send the first event which is just the GBS we received to begin with... IDK, actually,
    -- why I even bother to do this!
    let chan = gbsChan initialProgGbs
    Brick.BChan.writeBChan chan (NewStateEvent initialProgGbs)
    -- Now we fill a temporary file with the contents we receive via TCP, as mentioned earlier,
    -- since we've selected the remote file with the selector string. We get back the path
    -- to the temporary file and we also get its contents. The file path is used for the cache.
    -- The contents is used to update GBS with the appropriate mode (as a UTF8 string).
    tempFilePath <- emptySystemTempFile "waffle.cache.tmp"-- TODO: needs better template/pattern filename
    writeAllString initialProgGbs connectionSocket tempFilePath
    -- NOTE: it's a bit silly to write all bytes and then read from the file we wrote, but
    -- I'll mark this fix as a TODO, because I just did a major refactor and it's not a huge
    -- deal...
    contents <- ByteString.readFile tempFilePath
    -- Prepare the cache with this new temporary file that was created above.
    -- FIXME: what if location already exists? like if we're refreshing?
    let newCache = cacheInsert location tempFilePath (gbsCache initialProgGbs)
    -- Finally we setup the final event with a GBS of the specified render mode.
    doFinalEvent chan initialProgGbs history location (U8.toString contents) newCache
    -- TODO: this could be modularized; it's re-used!

-- | This is for final events that change the render mode based on the contents
doFinalEvent
  :: Brick.BChan.BChan CustomEvent
  -> GopherBrowserState
  -> Maybe History
  -> Location
  -> String
  -> Cache
  -> IO ()
doFinalEvent chan initialProgGbs history location@(host, port, resource, mode) contents newCache = do
  let
    finalState = case mode of
      TextFileMode -> initialProgGbs
        { gbsLocation   = location
        , gbsBuffer     = TextFileBuffer $ TextFile { tfContents = clean contents, tfTitle = locationAsString location }
        , gbsRenderMode = TextFileMode
        , gbsHistory    = maybeHistory
        , gbsCache      = newCache
        }
      MenuMode -> newStateForMenu
        chan
        (makeGopherMenu contents)--FIXME: doesn't this need clean first? or is this handled by newStateForMenu?
        location
        maybeHistory
        newCache
      m -> error $ "Cannot create a final progress state for: " ++ show m
  -- The final progress event, which changes the state to the render mode specified, using
  -- the GBS created above.
  Brick.BChan.writeBChan chan (NewStateEvent finalState)
  pure ()
  where
   maybeHistory = case history of
                    (Just h) -> h
                    Nothing  -> newChangeHistory initialProgGbs location

-- FIXME: the initial message should say something about loading cache if it is loading from cache
-- | The progress downloader for resources we want to cache, which also end
-- in a render mode associated with the resource requested. Not for save mode.
progressCacheable :: GopherBrowserState -> Maybe History -> Location -> IO ()
progressCacheable gbs history location@(host, port, resource, _) = do
  let cacheResult = cacheLookup location (gbsCache gbs)
  case cacheResult of
    (Just pathToCachedFile) -> do
      contents <- ByteString.readFile pathToCachedFile
      doFinalEvent (gbsChan gbs) gbs history location (U8.toString contents) (gbsCache gbs)
    Nothing -> progressGetBytes gbs history location

-- TODO: make a version of this for huge text files, or even huge menus!
-- | Emits events of a new application state (GBS). Starts by only
-- updating the progress buffer until the download is finished. When finished, 
-- a new application state is given which uses the NextState info which contains
-- the new RenderMode and Buffer, which is the final event emitted.
-- | Download a binary file to a temporary locationkk
-- Emits an Brick.T.AppEvent 
progressDownloadBytes :: GopherBrowserState -> Maybe History -> Location -> IO ()
progressDownloadBytes gbs _ (host, port, resource, _) =
  connect host (show port) $ \(connectionSocket, _) -> do
    let chan              = gbsChan gbs
        formerBufferState = gbsBuffer $ pbInitGbs (getProgress gbs) -- FIXME: not needed
    send connectionSocket (B8.pack $ resource ++ "\r\n")
    -- FIXME: what if this is left over from last time?
    tempFilePath <- emptySystemTempFile "waffle.download.tmp"
    Brick.BChan.writeBChan chan (NewStateEvent gbs)
    -- need to only fetch as many bytes as it takes to get period on a line by itself to
    -- close the connection.
    writeAllBytes gbs connectionSocket tempFilePath
    -- when exist should just emit a final event which has contents?
    -- will you need transactional buffer? how else can  you put into next state?
    -- you COULD overwrite next state with new content as pwer writebytes o in callback
    -- of save :) easy peasy
    --pure $ wow
    -- THE FINAL EVENT...
    x <- FB.newFileBrowser selectNothing MyViewport Nothing
    let finalState = gbs
          { gbsRenderMode = FileBrowserMode
          , gbsBuffer     = FileBrowserBuffer $ SaveBrowser
                              { fbFileBrowser       = x -- FIXME
                                                                       -- FIXME: move temp to specified location
                              , fbCallBack = (tempFilePath `renameFile`)
                              , fbIsNamingFile      = False
                              , fbFileOutPath       = ""
                              , fbOriginalFileName  = takeFileName resource
                              , fbFormerBufferState = formerBufferState
                              }
          }
    Brick.BChan.writeBChan chan (NewStateEvent finalState)
    pure ()

-- Higher order function to replace writeAllString and writeAllBytes TODO
--writeAll :: GopherBrowserState -> Socket -> (a -> IO ()) -> uh(ByteString.ByteString -> b) String -> IO ()

-- | Forces UTF-8.
writeAllString :: GopherBrowserState -> Socket -> String -> IO ()
writeAllString gbs' connectionSocket tempFilePath = do
  gosh <- recv connectionSocket recvChunkSize
  let newGbs = addProgBytes gbs' recvChunkSize
  Brick.BChan.writeBChan (gbsChan gbs') (NewStateEvent newGbs)
  case gosh of
    Nothing -> pure ()
    -- Doesn't set to started in status TODO FIXME
    Just chnk ->
      ByteString.appendFile tempFilePath chnk
        >> writeAllBytes newGbs connectionSocket tempFilePath
  where
   recvChunkSize = 1024

-- | This is for... FIXME
writeAllBytes :: GopherBrowserState -> Socket -> String -> IO ()
writeAllBytes gbs' connectionSocket tempFilePath = do
  gosh <- recv connectionSocket recvChunkSize
  let newGbs = addProgBytes gbs' recvChunkSize
  Brick.BChan.writeBChan (gbsChan gbs') (NewStateEvent newGbs)
  case gosh of
    Nothing -> pure ()
    -- Doesn't set to started in status TODO FIXME
    Just chnk ->
      ByteString.appendFile tempFilePath chnk
        >> writeAllBytes newGbs connectionSocket tempFilePath
  where
   recvChunkSize = 1024

-- TODO: progressUI...
drawProgressUI :: GopherBrowserState -> [T.Widget MyName]
drawProgressUI gbs = [a]
 where
  -- FIXME: "downloaded" isn't necessarily correct. You can request more bytes than is left...
  bytesDownloaded = show (pbBytesDownloaded (getProgress gbs))
  bytesMessage = "Downloaded bytes: " ++ bytesDownloaded
  downloadingWhat = pbMessage (getProgress gbs)
  connectMessage =
    if pbConnected (getProgress gbs) then bytesMessage else "⏳ Connnecting..."
  a = str $ downloadingWhat ++ "\n" ++ connectMessage

-- TODO: handleProgressEvents
progressEventHandler
  :: GopherBrowserState
  -> T.BrickEvent MyName CustomEvent
  -> T.EventM MyName (T.Next GopherBrowserState)
progressEventHandler gbs e = case e of
  T.AppEvent (NewStateEvent gbs') -> M.continue gbs'
  _                               -> M.continue gbs

-- FIXME: this is a hacky way to avoid circular imports
-- FIXME: the only reason not using progress is because of progress auto history
-- FIXME: can get an index error! should resolve with a dialog box.
-- Shares similarities with menu item selection
goHistory :: GopherBrowserState -> Int -> IO GopherBrowserState
goHistory gbs when = do
  let
    (history, historyMarker) = gbsHistory gbs
    unboundIndex             = historyMarker + when
    historyLastIndex         = length history - 1
    newHistoryMarker
      | unboundIndex > historyLastIndex = historyLastIndex
      | unboundIndex < 0 = 0
      | otherwise = unboundIndex
    location@(host, port, magicString, renderMode) =
      history !! newHistoryMarker
    newHistory = (history, newHistoryMarker)
  initProgressMode gbs (Just newHistory) location

-- | Create a new history after visiting a new page.
--
-- The only way to change the list of locations in history. Everything after
-- the current location is dropped, then the new location is appended, and
-- the history index increased. Thus, the new location is as far "forward"
-- as the user can now go.
--
-- See also: GopherBrowserState.
newChangeHistory :: GopherBrowserState -> Location -> History
newChangeHistory gbs newLoc =
  let (history, historyMarker) = gbsHistory gbs
      newHistory               = take (historyMarker + 1) history ++ [newLoc]
      newHistoryMarker         = historyMarker + 1
  in  (newHistory, newHistoryMarker)

-- This should go in progress, too, and use initProgress... FIXME
-- | Change the state to the parent menu by network request.
goParentDirectory :: GopherBrowserState -> IO GopherBrowserState
goParentDirectory gbs = do
  let (host, port, magicString, _) = gbsLocation gbs
      parentMagicString            = fromMaybe "/" (parentDirectory magicString)
  o <- gopherGet host (show port) parentMagicString
  let newMenu     = makeGopherMenu o
      newLocation = (host, port, parentMagicString, MenuMode)
  pure $ newStateForMenu (gbsChan gbs)
                         newMenu
                         newLocation
                         (newChangeHistory gbs newLocation)
                         (gbsCache gbs)
