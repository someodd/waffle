module BrickApp.Handle.Bookmarks where

import           Data.Maybe                               ( fromJust )
import           Control.Monad.IO.Class

import qualified Brick.Main                    as B
import qualified Brick.Types                   as T
import qualified Graphics.Vty                  as V
import           Brick.Widgets.Edit            as E

import           BrickApp.ModeAction.Progress             ( initProgressMode )
import           BrickApp.Handle.Menu                     ( menuEventHandler )
import           BrickApp.Types
import           BrickApp.Types.Names
import           BrickApp.ModeAction.Bookmarks

-- REDUNDANT FIXME (move to Utils/Status mayb?
formerMode :: GopherBrowserState -> GopherBrowserState
formerMode g = g { gbsRenderMode = seFormerMode $ fromJust $ gbsStatus g, gbsStatus = Nothing }

-- | The handler for the bookmark viewer, which just extends the menu's handler, since the bookmark viewer
-- is a menu!
bookmarksEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM AnyName (T.Next GopherBrowserState)
bookmarksEventHandler gbs e =
  case e of
    V.EvKey V.KEsc   []      -> do
      -- LOL all this is all done so we can have the display string in title since we're
      -- using the menu's draw code (lazy, but DRY)
      if let (_, historyIndex) = gbsHistory gbs in historyIndex == (-1)
        then B.continue gbs
        else let (historyStack, historyIndex) = gbsHistory gbs
                 currentLocation              = historyStack !! historyIndex
             in  liftIO (initProgressMode gbs (Just $ gbsHistory gbs) currentLocation) >>= B.continue
    V.EvKey (V.KChar 'd') [] -> liftIO (removeSelectedBookmark gbs) >>= B.continue
    _                        -> menuEventHandler gbs e

-- | The handler for the add a bookmark dialog.
addBookmarkEventHandler
  :: GopherBrowserState -> V.Event -> T.EventM AnyName (T.Next GopherBrowserState)
addBookmarkEventHandler gbs e = case e of
  V.EvKey V.KEsc   [] -> B.continue $ formerMode gbs
  -- On enter save bookmark with the name inputted
  V.EvKey V.KEnter [] -> liftIO (bookmarkCurrentLocation gbs) >>= B.continue
  _                   -> B.continue =<< editorEventHandler gbs e
 where
  -- | A modification of the default Brick.Widgets.Edit event handler; changed to
  -- return a GopherBrowserState instead of just an editor state.
  editorEventHandler
    :: GopherBrowserState -> V.Event -> T.EventM AnyName GopherBrowserState
  -- TODO: e' is unused!
  editorEventHandler _ e' =
    -- Maybe this should be a general function in Representation.
    let updateEditorInStatus x = gbs { gbsStatus = Just $ (fromJust $ gbsStatus gbs) { seEditorState = x } }
    in  updateEditorInStatus
          <$> E.handleEditorEvent e' (seEditorState $ fromJust $ gbsStatus gbs)
