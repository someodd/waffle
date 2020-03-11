-- | Stitch together the Brick app from the disparate parts of the UI.
--
-- Build initiate the Brick.Main.App. All UI modules/modes, such as UI.Save and
-- UI.Menu, converge here, into drawUI (for the Brick app's Brick.Main.appDraw)
-- which picks which module/mode's drawing function to use and likewise into appEvent
-- (for the Brick app's Brick.Main.appHandleEvent) which picks which module/mode's event
-- handler to use, both are picked based on the current gbsRenderMode set in the Brick
-- application state (GopherBrowserState).
module UI (uiMain) where

import Control.Monad (void)

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Graphics.Vty as V

import UI.Util
import UI.Menu
import UI.TextFile
import UI.Save
import UI.Search
import UI.Style
import GopherClient

-- | The draw handler which will choose a UI based on the browser's mode.
-- | Picks a UI/draw function based on the current gbsRenderMode.
--
-- Used as Brick.Main.appDraw when constructing the Brick app.
drawUI :: GopherBrowserState -> [T.Widget MyName]
drawUI gbs = case gbsRenderMode gbs of
  MenuMode -> menuModeUI gbs
  TextFileMode -> textFileModeUI gbs
  FileBrowserMode -> fileBrowserUi gbs
  SearchMode -> searchInputUI gbs

-- FIXME: shouldn't history be handled top level and not in individual handlers? or are there
-- some cases where we don't want history available
--
-- | The Brick application event handler which chooses which event handler to use based
-- on the current gbsRenderMode.
--
-- Used for Brick.Main.appHandleEvent.
appEvent :: GopherBrowserState -> T.BrickEvent MyName e -> T.EventM MyName (T.Next GopherBrowserState)
appEvent gbs (T.VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = M.halt gbs
appEvent gbs (T.VtyEvent e)
  | gbsRenderMode gbs == MenuMode = menuEventHandler gbs e
  | gbsRenderMode gbs == TextFileMode = textFileEventHandler gbs e
  | gbsRenderMode gbs == FileBrowserMode = saveEventHandler gbs e
  | gbsRenderMode gbs == SearchMode = searchEventHandler gbs e
  | otherwise = error "Unrecognized mode in event."
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
--
-- | Start the Brick app at a specific Gopher menu in Gopherspace.
uiMain :: GopherMenu -> (String, Int, String) -> IO ()
uiMain gm (host, port, magicString) =
  let trueLocationType = (host, port, magicString, MenuMode)
  in void $ M.defaultMain theApp (newStateForMenu gm trueLocationType ([trueLocationType], 0))
