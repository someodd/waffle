-- | Everything related to the UI for viewing text files.
module UI.TextFile where

import           Control.Monad.IO.Class

import qualified Graphics.Vty                  as V
import qualified Brick.Types                   as T
import           Brick.Widgets.Center           ( vCenter
                                                , hCenter
                                                )
import           Brick.Widgets.Border           ( borderWithLabel )
import           Brick.AttrMap                  ( applyAttrMappings )
import           Brick.Widgets.Core             ( viewport
                                                , vBox
                                                , str
                                                , hLimitPercent
                                                , withAttr
                                                , withBorderStyle
                                                , vLimit
                                                , updateAttrMap
                                                )
import qualified Brick.Main                    as M

import           UI.Util
import           UI.Progress
import           UI.Representation
import           UI.Style

-- FIXME: could even put a default view box in Style.hs? maybe DefaultView.hs?
-- FIXME: I want a file title in the title/label
-- | The UI for rendering and viewing a text file.
-- This is also used in the help screen/used by Help module.
textFileModeUI :: GopherBrowserState -> [T.Widget MyName]
textFileModeUI gbs = ui
  where
    textFileContents = tfContents $ getTextFile gbs
    textFileTitle = tfTitle $ getTextFile gbs
    box =
      updateAttrMap (applyAttrMappings borderMappings)
        $ withBorderStyle customBorder
        $ borderWithLabel (withAttr titleAttr $ str textFileTitle)
        $ viewport MyViewport T.Both
        $ hLimitPercent 100
        $ str (clean textFileContents)
    view = vBox
      [ box
      , vLimit 1 $ str "? for help."
      ]
    ui = [hCenter $ vCenter view]

-- | Basic text file controls, modularized so that the Help screen can use
-- too, without including the history stuff. See the Help module.
basicTextFileEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM MyName (T.Next GopherBrowserState)
basicTextFileEventHandler gbs e = case e of
  -- What about left and right?!
  V.EvKey (V.KChar 'j') [] -> M.vScrollBy myNameScroll 1 >> M.continue gbs
  V.EvKey (V.KChar 'k') [] -> M.vScrollBy myNameScroll (-1) >> M.continue gbs
  V.EvKey (V.KChar 'l') [] -> M.hScrollBy myNameScroll 1 >> M.continue gbs
  V.EvKey (V.KChar 'h') [] -> M.hScrollBy myNameScroll (-1) >> M.continue gbs
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
