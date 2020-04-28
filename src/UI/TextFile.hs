{-# LANGUAGE OverloadedStrings #-}

-- | Everything related to the UI for viewing text files.
module UI.TextFile
  ( textFileModeUI
  , basicTextFileEventHandler
  , textFileEventHandler
  ) where

import           Control.Monad.IO.Class

import qualified Graphics.Vty                  as V
import qualified Brick.Types                   as T
import           Brick.Widgets.Core             ( viewport
                                                , txt
                                                )
import qualified Brick.Main                    as M

import           UI.Util
import           UI.Progress
import           UI.Representation

textFileModeUI :: GopherBrowserState -> [T.Widget MyName]
textFileModeUI gbs = defaultBrowserUI gbs (viewport TextViewport T.Both) titleWidget mainWidget statusWidget
  where
   mainWidget   = let textFileContents = tfContents $ getTextFile gbs
                  in  txt $ clean textFileContents
   titleWidget  = txt $ tfTitle $ getTextFile gbs
   statusWidget = txt "? for help. Text file mode."

-- | Basic text file controls, modularized so that the Help screen can use
-- too, without including the history stuff. See the Help module.
basicTextFileEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM MyName (T.Next GopherBrowserState)
basicTextFileEventHandler gbs e = case e of
  -- What about left and right?!
  V.EvKey (V.KChar 'j') [] -> M.vScrollBy textViewportScroll 1 >> M.continue gbs
  V.EvKey (V.KChar 'k') [] -> M.vScrollBy textViewportScroll (-1) >> M.continue gbs
  V.EvKey (V.KChar 'l') [] -> M.hScrollBy textViewportScroll 1 >> M.continue gbs
  V.EvKey (V.KChar 'h') [] -> M.hScrollBy textViewportScroll (-1) >> M.continue gbs
  _                        -> M.continue gbs

-- | Event handler for a text file location in gopherspace.
textFileEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM MyName (T.Next GopherBrowserState)
textFileEventHandler gbs e = case e of
  V.EvKey (V.KChar 'u') [] -> liftIO (goParentDirectory gbs) >>= M.continue
  V.EvKey (V.KChar 'f') [] -> liftIO (goHistory gbs 1) >>= M.continue
  V.EvKey (V.KChar 'b') [] -> liftIO (goHistory gbs (-1)) >>= M.continue
  _                        -> basicTextFileEventHandler gbs e
