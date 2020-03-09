-- | The main parts of the UI: event handling and the Brick app.
-- The UI uses Brick and Vty extensively.

module UI (uiMain) where

import Control.Monad.IO.Class
import Control.Monad (void)
import Data.Maybe

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.Widgets.List as L

import UI.Util
import UI.Menu
import UI.History
import UI.TextFile
import UI.Save
import UI.Search
import UI.Style
import GopherClient

-- | Change the state to the parent menu by network request.
goParentDirectory :: GopherBrowserState -> IO GopherBrowserState
goParentDirectory gbs = do
  let (host, port, magicString, _) = gbsLocation gbs
      parentMagicString = fromMaybe ("/") (parentDirectory magicString)
  o <- gopherGet host (show port) parentMagicString
  let newMenu = makeGopherMenu o
      newLocation = (host, port, parentMagicString, MenuMode)
  pure $ newStateForMenu newMenu newLocation (newChangeHistory gbs newLocation)

-- | The draw handler which will choose a UI based on the browser's mode.
drawUI :: GopherBrowserState -> [T.Widget MyName]
drawUI gbs = case gbsRenderMode gbs of
  MenuMode -> menuModeUI gbs
  TextFileMode -> textFileModeUI gbs
  FileBrowserMode -> fileBrowserUi gbs
  SearchMode -> searchInputUI gbs

-- TODO: implement backspace as back in history which trims it
appEvent :: GopherBrowserState -> T.BrickEvent MyName e -> T.EventM MyName (T.Next GopherBrowserState)
-- This should be backspace
-- check gbs if the state says we're handling a menu (list) or a text file (viewport)
appEvent gbs (T.VtyEvent e)
  | gbsRenderMode gbs == MenuMode = case e of
      V.EvKey V.KEnter [] -> liftIO (newStateFromSelectedMenuItem gbs) >>= M.continue
      V.EvKey (V.KChar 'n') [] -> M.continue $ jumpNextLink gbs
      V.EvKey (V.KChar 'p') [] -> M.continue $ jumpPrevLink gbs
      V.EvKey (V.KChar 'u') [] -> liftIO (goParentDirectory gbs) >>= M.continue
      V.EvKey (V.KChar 'f') [] -> liftIO (goHistory gbs 1) >>= M.continue
      V.EvKey (V.KChar 'b') [] -> liftIO (goHistory gbs (-1)) >>= M.continue
      V.EvKey V.KEsc [] -> M.halt gbs
-- check gbs if the state says we're handling a menu (list) or a text file (viewport)
      ev -> M.continue =<< updateMenuList <$> L.handleListEventVi L.handleListEvent ev (getMenuList gbs)
  -- viewport stuff here
  | gbsRenderMode gbs == TextFileMode = case e of
    V.EvKey (V.KChar 'j')  [] -> M.vScrollBy myNameScroll 1 >> M.continue gbs
    V.EvKey (V.KChar 'k')  [] -> M.vScrollBy myNameScroll (-1) >> M.continue gbs
    V.EvKey (V.KChar 'u') [] -> liftIO (goParentDirectory gbs) >>= M.continue
    V.EvKey (V.KChar 'f') [] -> liftIO (goHistory gbs 1) >>= M.continue
    V.EvKey V.KEsc [] -> M.halt gbs
    V.EvKey (V.KChar 'b') [] -> liftIO (goHistory gbs (-1)) >>= M.continue
    _ -> M.continue gbs
  | gbsRenderMode gbs == FileBrowserMode = case e of
    -- instances of 'b' need to tap into gbsbuffer
    V.EvKey V.KEsc [] | not (FB.fileBrowserIsSearching $ fromFileBrowserBuffer (gbsBuffer gbs)) ->
      M.continue $ returnFormerState gbs
    _ -> do
      let (gbs', bUnOpen') = handleFileBrowserEvent' gbs e (fromFileBrowserBuffer $ gbsBuffer gbs)
      b' <- bUnOpen'
      -- If the browser has a selected file after handling the
      -- event (because the user pressed Enter), shut down.
      let fileOutPath = fbFileOutPath (gbsBuffer gbs')
      if (isNamingFile gbs') then
        M.continue (updateFileBrowserBuffer gbs' b')
      -- this errors now
      else if not (null $ getOutFilePath gbs') then
        (liftIO (doCallBack fileOutPath) >>= M.continue)
      else
        M.continue (updateFileBrowserBuffer gbs' b')
  -- FIXME
  | gbsRenderMode gbs == SearchMode = case e of
    V.EvKey V.KEsc [] -> M.continue $ returnSearchFormerState gbs
    -- FIXME: needs to make search request
    V.EvKey V.KEnter [] -> liftIO (mkSearchResponseState gbs) >>= M.continue
    V.EvKey V.KBS [] -> M.continue $ updateQuery $ take (length curQuery - 1) curQuery
    V.EvKey (V.KChar c) [] ->
      M.continue $ gbs { gbsBuffer = (gbsBuffer gbs) { sbQuery = curQuery ++ [c] } }
    _ -> M.continue gbs
  | otherwise = error "Unrecognized mode in event."
  -- TODO FIXME: the MenuBuffer should be record syntax
  where
    curQuery = sbQuery $ gbsBuffer gbs
    updateQuery s = gbs { gbsBuffer = (gbsBuffer gbs) { sbQuery = s } }
    returnFormerState g = g {gbsBuffer = (fbFormerBufferState $ gbsBuffer g), gbsRenderMode = MenuMode}
    returnSearchFormerState g = g {gbsBuffer = (sbFormerBufferState $ gbsBuffer g), gbsRenderMode = MenuMode}
    getOutFilePath g = fbFileOutPath (gbsBuffer g)
    isNamingFile g = fbIsNamingFile (gbsBuffer g)
    doCallBack a = do
      fbCallBack (gbsBuffer gbs) a
      pure $ gbs {gbsBuffer = (fbFormerBufferState $ gbsBuffer gbs), gbsRenderMode = MenuMode}
    fromFileBrowserBuffer x = fbFileBrowser x
    updateFileBrowserBuffer g bu = g { gbsBuffer = (gbsBuffer g) { fbFileBrowser = bu }  }
    getMenuList x =
      let (MenuBuffer (_, gl, _)) = gbsBuffer x
      in gl
    updateMenuList x =
      let (MenuBuffer (gm, _, fl)) = gbsBuffer gbs
      in gbs {gbsBuffer=MenuBuffer (gm, x, fl)}
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
