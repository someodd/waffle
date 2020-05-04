{-# LANGUAGE OverloadedStrings #-}

-- FIXME: implement network error dialog boxes here and return state if fail?
-- this would imply the need to have a fallback state, right?
-- | Handle indication of download progress for various UI.Util.RenderMode types, like downloading
-- menus, text files, and binary file downloads.
module UI.Progress
  ( initProgressMode
  , modeTransition
  , drawProgressUI
  , progressEventHandler
  , goHistory
  , goParentDirectory
  ) where

import           Control.Exception
import           Data.Text.Encoding.Error       (lenientDecode)
import           Data.Text.Encoding            as E
import qualified Data.Text                     as T
import           Data.Foldable
import           Data.Maybe
import qualified Data.ByteString               as ByteString
import           Control.Concurrent             ( forkIO )
import           System.Directory               ( renameFile )

import           System.FilePath                ( takeFileName )
import           Network.Simple.TCP
import qualified Brick.Widgets.FileBrowser     as FB
import           Brick.Widgets.Core             ( txt )
import qualified Brick.Main                    as M
import qualified Brick.BChan
import qualified Data.ByteString.Char8         as B8
import qualified Brick.Types                   as T
import           System.IO.Temp                 ( emptySystemTempFile )
import qualified Data.ByteString.UTF8          as U8
import qualified Graphics.Vty                  as V

import           UI.Util
import           UI.Representation
import           Gopher
import           GopherNet                      ( writeAllBytes' )

-- FIXME: also used by save.hs
selectNothing :: FB.FileInfo -> Bool
selectNothing _ = False

-- FIXME: need to combine vScroll and hScroll into a single event! because otherwise
-- it's only giving back the event for hScroll!
-- Things to do when switching modes! Namely reset viewports...
modeTransition :: T.EventM MyName ()
modeTransition = do
  --M.vScrollToBeginning myNameScroll
  traverse_ M.vScrollToBeginning [myNameScroll, mainViewportScroll, menuViewportScroll, textViewportScroll]
  traverse_ M.hScrollToBeginning [myNameScroll, mainViewportScroll, menuViewportScroll, textViewportScroll]

-- FIXME: could reset the scroll here...?
-- | The entrypoint for using "progress mode" which...
initProgressMode :: GopherBrowserState -> Maybe History -> Location -> IO GopherBrowserState
initProgressMode gbs history location@(_, _, _, mode) =
  let
    (downloader, message) = case mode of
      TextFileMode    -> (progressCacheable, "text file 📄")
      MenuMode        -> (progressCacheable, "menu 📂")
      FileBrowserMode -> (progressDownloadBytes, "binary file")
      -- This error should be a dialog box instead...
      m -> error $ "Unsupported mode requested for progress mode: " ++ show m
    initialProgGbs = gbs
      { gbsRenderMode = ProgressMode
      , gbsBuffer     = ProgressBuffer $ Progress
                          { pbBytesDownloaded = 0
                          , pbInitGbs         = gbs
                          , pbConnected       = False
                          , pbIsFromCache     = isCached location (gbsCache gbs)
                          , pbMessage         = "Downloading a " <> message
                          }
      }
  -- Should catch network error in a popup (representational).
  in forkIO (downloader initialProgGbs history location) >> pure initialProgGbs

-- THIS IS A CALLBACK FOR GOPHERNET
counterMutator :: GopherBrowserState -> Maybe ByteString.ByteString -> IO GopherBrowserState
counterMutator gbs someBytes =
  let bytesReceived = case someBytes of
        Nothing  -> 0
        -- We count the bytes each time because the second-to-last response can have
        -- under the recvChunkSize. The last response will always be Nothing.
        (Just n) -> ByteString.length n
      newGbs        = addProgBytes' gbs bytesReceived
  in  Brick.BChan.writeBChan (gbsChan gbs) (NewStateEvent newGbs) >> pure newGbs
  where
  --addProgBytes :: GopherBrowserState -> Int -> GopherBrowserState
  addProgBytes' gbs' nbytes =
    let cb x = x
          { pbBytesDownloaded = pbBytesDownloaded (getProgress gbs') + nbytes
          , pbConnected       = True
          }
    in  updateProgressBuffer gbs' cb

-- MUST DEPRECATE FIXME TODO
addProgBytes :: GopherBrowserState -> Int -> GopherBrowserState
addProgBytes gbs' nbytes =
  let cb x = x
        { pbBytesDownloaded = pbBytesDownloaded (getProgress gbs') + nbytes
        , pbConnected       = True
        }
  in  updateProgressBuffer gbs' cb

-- FIXME: bad doc, bad name
-- | Handle a connection, including reporting exceptions...
gracefulSock :: GopherBrowserState -> Location -> ((Socket, SockAddr) -> IO ()) -> IO ()
gracefulSock gbs location@(host, port, resource, _) handler = do
  result <- try $ connectSock (T.unpack host) (show port) :: IO (Either SomeException (Socket, SockAddr))
  case result of
    Left ex   -> makePopup gbs $ T.pack (show ex)
    Right val -> handler val
  where
    makePopup gbs exMsg =
      let formerGbs         = pbInitGbs (getProgress gbs)
          formerMode        = case gbsRenderMode formerGbs of
                                GotoMode -> seFormerMode $ fromJust $ gbsStatus formerGbs-- FIXME: fromJust horrible
                                x        -> x
          newBuffState      = formerGbs { gbsRenderMode = formerMode, gbsStatus = Nothing }
          errorPopup        = Just $ Popup { pLabel = "Network/Goto Error", pWidgets = [txt exMsg], pHelp = "Couldn't reach supplied address. ESC to return..."}
          finalState        = newBuffState { gbsPopup = errorPopup, gbsStatus = Nothing }-- TODO, FIXME: deactivate status
          chan              = gbsChan finalState
      in  Brick.BChan.writeBChan chan (FinalNewStateEvent finalState)

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
progressGetBytes initialProgGbs history location@(host, port, resource, _) =
  gracefulSock initialProgGbs location handleResult
  where
    handleResult (connectionSocket, _) = do
      -- Send the magic/selector string (request a path) to the websocket we're connected to.
      -- This allows us to later receive the bytes located at this "path."
      send connectionSocket (B8.pack $ T.unpack resource ++ "\r\n")
      -- Send the first event which is just the GBS we received to begin with... IDK, actually,
      -- why I even bother to do this!
      let chan = gbsChan initialProgGbs
      Brick.BChan.writeBChan chan (NewStateEvent initialProgGbs)
      -- Now we fill a temporary file with the contents we receive via TCP, as mentioned earlier,
      -- since we've selected the remote file with the selector string. We get back the path
      -- to the temporary file and we also get its contents. The file path is used for the cache.
      -- The contents is used to update GBS with the appropriate mode (as a UTF8 string).
      tempFilePath <- emptySystemTempFile "waffle.cache.tmp"-- TODO: needs better template/pattern filename
      writeAllBytes' (Just counterMutator) (Just initialProgGbs) connectionSocket tempFilePath
      -- NOTE: it's a bit silly to write all bytes and then read from the file we wrote, but
      -- I'll mark this fix as a TODO, because I just did a major refactor and it's not a huge
      -- deal...
      contents <- ByteString.readFile tempFilePath
      -- Prepare the cache with this new temporary file that was created above.
      -- FIXME: what if location already exists? like if we're refreshing?
      let newCache = cacheInsert location tempFilePath (gbsCache initialProgGbs)
      -- We setup the final event with a GBS of the specified render mode.
      doFinalEvent chan initialProgGbs history location (E.decodeUtf8With lenientDecode contents) newCache
      -- Finally we close the socket! We're done!
      closeSock connectionSocket

-- | This is for final events that change the render mode based on the contents.
doFinalEvent
  :: Brick.BChan.BChan CustomEvent
  -> GopherBrowserState
  -> Maybe History
  -> Location
  -> T.Text
  -> Cache
  -> IO ()
doFinalEvent chan initialProgGbs history location@(_, _, _, mode) contents newCache = do
  let
    finalState = case mode of
      TextFileMode -> initialProgGbs
        { gbsLocation   = location
        , gbsBuffer     = TextFileBuffer $ TextFile { tfContents = cleanAll contents, tfTitle = locationAsString location }
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
  Brick.BChan.writeBChan chan (FinalNewStateEvent finalState)
  pure ()
  where
   maybeHistory = case history of
                    (Just h) -> h
                    Nothing  -> newChangeHistory initialProgGbs location

-- FIXME: the initial message should say something about loading cache if it is loading from cache
-- | The progress downloader for resources we want to cache, which also end
-- in a render mode associated with the resource requested. Not for save mode.
progressCacheable :: GopherBrowserState -> Maybe History -> Location -> IO ()
progressCacheable gbs history location@(_, _, _, _) =
  case cacheLookup location $ gbsCache gbs of
    -- There is a cache for the requested location, so let's load that, instead...
    (Just pathToCachedFile) -> do
      contents <- ByteString.readFile pathToCachedFile
      -- We use "doFinalEvent" because it will switch the mode/state for the content of the cache file!
      doFinalEvent (gbsChan gbs) gbs history location (E.decodeUtf8With lenientDecode contents) (gbsCache gbs)
    -- There is no cache for the requested location, so we must make a request and cache it!
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
  connect (T.unpack host) (show port) $ \(connectionSocket, _) -> do
    let chan              = gbsChan gbs
        formerBufferState = gbsBuffer $ pbInitGbs (getProgress gbs) -- FIXME: not needed
    send connectionSocket (B8.pack $ T.unpack $ resource <> "\r\n")
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
                              , fbOriginalFileName  = takeFileName $ T.unpack resource
                              , fbFormerBufferState = formerBufferState
                              }
          }
    -- We don't use doFinalEvent, because the file saver (which this is for) works a bit differently!
    Brick.BChan.writeBChan chan (FinalNewStateEvent finalState)
    pure ()

-- | This is for... FIXME
writeAllBytes :: GopherBrowserState -> Socket -> String -> IO ()
writeAllBytes gbs' connectionSocket tempFilePath = do
  gosh <- recv connectionSocket recvChunkSize
  let bytesReceived = case gosh of
        Nothing  -> 0
        -- We count the bytes each time because the second-to-last response can have
        -- under the recvChunkSize. The last response will always be Nothing.
        (Just n) -> ByteString.length n
      newGbs        = addProgBytes gbs' bytesReceived
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
  bytesDownloaded = T.pack $ show (pbBytesDownloaded (getProgress gbs))
  bytesMessage = "Downloaded bytes: " <> bytesDownloaded
  downloadingWhat = pbMessage (getProgress gbs)

  connectMessage :: T.Text
  connectMessage
    | pbIsFromCache (getProgress gbs) = "⏳ Loading from cache..."
    | pbConnected (getProgress gbs)   = bytesMessage
    | otherwise                       = "⏳ Connecting..."

  a = txt $ downloadingWhat <> "\n" <> connectMessage

-- FIXME: maybe this needs to just have generic B.BrickEvent MyName CustomEvent
-- and match from there
-- TODO: handleProgressEvents
-- FIXME: no need for this left/right nonsense because they're both
-- B.BrickEvent MyName CustomEvent and you can decipher from there like in UI...
-- should do this soon...
progressEventHandler
  :: GopherBrowserState
  -> Either (T.BrickEvent MyName CustomEvent) V.Event
  -> T.EventM MyName (T.Next GopherBrowserState)
progressEventHandler gbs (Left e)  = case e of
  -- This is extremely hacky!
  T.AppEvent (NewStateEvent gbs')       -> M.continue gbs'
  T.AppEvent (FinalNewStateEvent gbs')  -> modeTransition >> M.continue gbs'
  _                                     -> M.continue gbs
progressEventHandler gbs (Right _) = M.continue gbs

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
    location = history !! newHistoryMarker
    newHistory = (history, newHistoryMarker)
  if historyMarker == newHistoryMarker
    then pure gbs
    else initProgressMode gbs (Just newHistory) location

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

-- | Go up a directory; go to the parent menu of whatever the current selector is.
goParentDirectory :: GopherBrowserState -> IO GopherBrowserState
goParentDirectory gbs =
  let (host, port, magicString, _) = gbsLocation gbs
      parentMagicString            = fromMaybe "/" (parentDirectory magicString)
      newLocation = (host, port, parentMagicString, MenuMode)
  in  initProgressMode gbs Nothing newLocation
