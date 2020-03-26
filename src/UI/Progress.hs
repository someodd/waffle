-- FIXME, TODO: history shouldn't be in here. who knows if you're refreshing or not! It's just
-- a weird mixture of responsibilities!
-- Can we just use temp files created for caching? maybe in that case we should also stop
-- using memory string and always download temp files for everything...
-- TODO: could even implement the loader in the status bar instead of new screen... what about cancelling loading also
-- TODO: document

-- | Handle indication of download progress for various UI.Util.RenderMode types, like downloading
-- menus, text files, and binary file downloads.
module UI.Progress where

import qualified Data.ByteString as ByteString
import Control.Concurrent (forkIO)
import System.Directory (renameFile)

import System.FilePath (takeFileName)
import Network.Simple.TCP
import qualified Brick.Widgets.FileBrowser as FB
import Brick.Widgets.Core (str)
import qualified Brick.Main as M
import qualified Brick.BChan
import qualified Data.ByteString.Char8 as B8
import qualified Brick.Types as T
import System.IO.Temp (emptySystemTempFile)
import qualified Data.ByteString.UTF8 as U8

import UI.Util
import UI.History--TODO: make this in top level not just UI? or it's all app state so idk
import GopherClient

selectNothing :: FB.FileInfo -> Bool
selectNothing _ = False

initProgressMode :: GopherBrowserState -> Location -> IO GopherBrowserState
initProgressMode gbs location@(_, _, _, mode) =
  let (downloader, message) = case mode of
                                TextFileMode -> (progressDownloadMemoryString, "text file")
                                MenuMode -> (progressDownloadMemoryString, "menu")
                                FileBrowserMode -> (progressDownloadBytes, "binary file")
                                m -> error $ "Unspported mode requested for progress mode: " ++ show m
      initialProgGbs = gbs { gbsRenderMode = ProgressMode
                           , gbsBuffer = ProgressBuffer
                               { pbBytesDownloaded = 0
                               , pbInitGbs = gbs
                               , pbMessage = "Downloading a " ++ message
                              }
                           }
  in forkIO (downloader initialProgGbs location) >> pure initialProgGbs

addProgBytes :: GopherBrowserState -> Int -> GopherBrowserState
addProgBytes gbs' nbytes = gbs' { gbsBuffer = (gbsBuffer gbs') { pbBytesDownloaded = pbBytesDownloaded (gbsBuffer gbs') + nbytes } }

-- TODO: maybe make updating history optional? for reload. could be argument
-- | Download something from a GopherHole to a string in memory, while sending progress
-- events which replace the GopherBrowserState. The final event transitions to the
-- UI.Util.RenderMode corresponding to what is being downloaded.
progressDownloadMemoryString :: GopherBrowserState -> Location -> IO ()
progressDownloadMemoryString initialProgGbs location@(host, port, resource, mode) =
  connect host (show port) $ \(connectionSocket, _) -> do
    let chan = gbsChan initialProgGbs
    send connectionSocket (B8.pack $ resource ++ "\r\n")
    Brick.BChan.writeBChan chan (NewStateEvent initialProgGbs)
    o <- getAllBytes (pure $ Just $ GetAllBytesCallback (getAllBytesCallback, initialProgGbs)) 1024 (pure B8.empty) connectionSocket
    let textFile = clean (U8.toString o)
        finalState = case mode of
                       TextFileMode -> initialProgGbs { gbsLocation = location
                                                      , gbsBuffer = TextFileBuffer $ textFile
                                                      , gbsRenderMode = TextFileMode
                                                      , gbsHistory = newChangeHistory initialProgGbs location
                                                      }
                       MenuMode -> newStateForMenu chan (makeGopherMenu $ U8.toString o) location (newChangeHistory initialProgGbs location)
                       m -> error $ "Cannot download as a string into memory for: " ++ show m
    -- The final progress event, which changes the state to the render mode specified;
    -- utilizing the string downloaded into memory.
    Brick.BChan.writeBChan chan (NewStateEvent finalState)
    pure ()
  where
    -- FIXME: this doesn't use chan!
    getAllBytesCallback :: GopherBrowserState -> B8.ByteString -> IO GopherBrowserState
    getAllBytesCallback gbs' chnk = do
      let newGbs = addProgBytes gbs' (B8.length chnk)
      Brick.BChan.writeBChan (gbsChan gbs') (NewStateEvent newGbs)
      pure newGbs

-- TODO: make a version of this for huge text files, or even huge menus!
-- | Emits events of a new application state (GBS). Starts by only
-- updating the progress buffer until the download is finished. When finished, 
-- a new application state is given which uses the NextState info which contains
-- the new RenderMode and Buffer, which is the final event emitted.
-- | Download a binary file to a temporary locationkk
-- Emits an Brick.T.AppEvent 
progressDownloadBytes :: GopherBrowserState -> Location -> IO ()
progressDownloadBytes gbs (host, port, resource, _) =
  connect host (show port) $ \(connectionSocket, _) -> do
    let chan = gbsChan gbs
        formerBufferState = gbsBuffer $ pbInitGbs (gbsBuffer gbs) -- FIXME: not needed
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
    let finalState = gbs { gbsRenderMode = FileBrowserMode
                         , gbsBuffer = FileBrowserBuffer { fbFileBrowser = x -- FIXME
                                                         -- FIXME: move temp to specified location
                                                         , fbCallBack = (tempFilePath `renameFile`)
                                                         , fbIsNamingFile = False
                                                         , fbFileOutPath = ""
                                                         , fbOriginalFileName = takeFileName resource
                                                         , fbFormerBufferState = formerBufferState
                                                         }
                         }
    Brick.BChan.writeBChan chan (NewStateEvent finalState)
    pure ()
  where
    -- FIXME: could make gopherclient thing
    recvChunks = 1024
    writeAllBytes :: GopherBrowserState -> Socket -> String -> IO ()
    writeAllBytes gbs' connectionSocket tempFilePath = do
      gosh <- recv connectionSocket recvChunks
      let newGbs = addProgBytes gbs' recvChunks
      Brick.BChan.writeBChan (gbsChan gbs') (NewStateEvent newGbs)
      case gosh of
        Nothing -> pure ()
        -- Doesn't set to started in status TODO FIXME
        Just chnk -> ByteString.appendFile tempFilePath chnk >> writeAllBytes newGbs connectionSocket tempFilePath

-- TODO: progressUI...
drawProgressUI :: GopherBrowserState -> [T.Widget MyName]
drawProgressUI gbs = [a]
  where
    -- FIXME: "downloaded" isn't necessarily correct. You can request more bytes than is left...
    a = str $ "Downloaded bytes: " ++ show (pbBytesDownloaded (gbsBuffer gbs))

-- TODO: handleProgressEvents
progressEventHandler :: GopherBrowserState -> T.BrickEvent MyName CustomEvent -> T.EventM MyName (T.Next GopherBrowserState)
progressEventHandler gbs e =
  case e of
    T.AppEvent (NewStateEvent gbs') -> M.continue gbs'
    _ -> M.continue gbs
