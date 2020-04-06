-- | Search mode for the UI. Index search implementation for UI.
--
-- Handle drawing the search UI, modifying the application state
-- for the search UI, and handling Brick events for search.
module UI.Search
  ( searchInputUI
  , searchEventHandler
  )
where

import           Control.Monad.IO.Class

import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Widgets.Edit            as E
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events      ( Event )

import           GopherClient                   ( makeGopherMenu
                                                , searchGet
                                                )
import           UI.History
import           UI.Popup
import           UI.Util
import           UI.Representation

-- | Draw the search prompt. Used by UI.drawUI if the gbsRenderMode
-- is SearchMode.
searchInputUI :: GopherBrowserState -> [T.Widget MyName]
searchInputUI gbs = inputPopupUI editorBuffer labelText helpText
 where
  searchBuffer = getSearch gbs
  editorBuffer = sbEditorState (getSearch gbs)
  labelText    = "Search: " ++ sbHost searchBuffer
  helpText     = "Press ENTER to search"

-- | Form a new application state based on a Gopher search request.
mkSearchResponseState :: GopherBrowserState -> IO GopherBrowserState
mkSearchResponseState gbs = do
  let host     = sbHost $ getSearch gbs
      port     = sbPort $ getSearch gbs
      resource = sbSelector $ getSearch gbs
      query    = unlines (E.getEditContents $ sbEditorState $ getSearch gbs)
  (o, selector) <- searchGet host (show port) resource query
  let newMenu  = makeGopherMenu o
      location = (host, port, selector, MenuMode)
  pure $ newStateForMenu (gbsChan gbs)
                         newMenu
                         location
                         (newChangeHistory gbs location)

-- | The Brick application event handler for search mode. See: UI.appEvent and
--- Brick.Main.appHandleEvent.
searchEventHandler
  :: GopherBrowserState -> Event -> T.EventM MyName (T.Next GopherBrowserState)
searchEventHandler gbs e = case e of
    -- FIXME: esc quits! Change key...
  V.EvKey V.KEsc   [] -> M.continue $ returnSearchFormerState gbs
  V.EvKey V.KEnter [] -> liftIO (mkSearchResponseState gbs) >>= M.continue
  _                   -> M.continue =<< editorEventHandler gbs e
 where
  returnSearchFormerState g = g { gbsBuffer = sbFormerBufferState $ getSearch g
                                , gbsRenderMode = MenuMode
                                }

  -- | A modification of the default Brick.Widgets.Edit event handler; changed to
  -- return a GopherBrowserState instead of just an editor state.
  editorEventHandler
    :: GopherBrowserState -> Event -> T.EventM MyName GopherBrowserState
  editorEventHandler gbs' e' =
    let updateEditorInBuffer x =
            updateSearchBuffer gbs' (\s -> s { sbEditorState = x })
    in  updateEditorInBuffer
          <$> E.handleEditorEvent e' (sbEditorState $ getSearch gbs)
