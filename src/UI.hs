-- TODO: have event handlers broken into the appropriate module
-- | The main parts of the UI: event handling and the Brick app.
-- The UI uses Brick and Vty extensively.

module UI (uiMain) where

import Control.Monad (void)

import qualified Brick.Main as M
import qualified Brick.Types as T

import UI.Util
import UI.Menu
import UI.TextFile
import UI.Save
import UI.Search
import UI.Style
import GopherClient

-- | The draw handler which will choose a UI based on the browser's mode.
drawUI :: GopherBrowserState -> [T.Widget MyName]
drawUI gbs = case gbsRenderMode gbs of
  MenuMode -> menuModeUI gbs
  TextFileMode -> textFileModeUI gbs
  FileBrowserMode -> fileBrowserUi gbs
  SearchMode -> searchInputUI gbs

-- FIXME: shouldn't history be handled top level and not in individual handlers? or are there
-- some cases where we don't want history available?
-- TODO: implement backspace as back in history which trims it
appEvent :: GopherBrowserState -> T.BrickEvent MyName e -> T.EventM MyName (T.Next GopherBrowserState)
-- This should be backspace
-- check gbs if the state says we're handling a menu (list) or a text file (viewport)
appEvent gbs (T.VtyEvent e)
  | gbsRenderMode gbs == MenuMode = menuEventHandler gbs e
  -- viewport stuff here
  | gbsRenderMode gbs == TextFileMode = textFileEventHandler gbs e
  | gbsRenderMode gbs == FileBrowserMode = saveEventHandler gbs e
  -- FIXME
  | gbsRenderMode gbs == SearchMode = searchEventHandler gbs e
  | otherwise = error "Unrecognized mode in event."
  -- TODO FIXME: the MenuBuffer should be record syntax
appEvent gbs _ = M.continue gbs

theApp :: M.App GopherBrowserState e MyName
theApp =
  M.App { M.appDraw = drawUI
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return
        , M.appAttrMap = const theMap
        }

-- FIXME: isn't there a way to infer a location's type? Assuming first
-- link is a menu is a horrible hack...
-- | This is called in order to start the UI.
uiMain :: GopherMenu -> (String, Int, String) -> IO ()
uiMain gm (host, port, magicString) =
  let trueLocationType = (host, port, magicString, MenuMode)
  in void $ M.defaultMain theApp (newStateForMenu gm trueLocationType ([trueLocationType], 0))
