module UI.Search where

import qualified Brick.Types as T
import Brick.Widgets.Edit as E
import Graphics.Vty.Input.Events (Event)
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import Control.Monad.IO.Class

import UI.Util
import GopherClient (searchGet, makeGopherMenu)
import UI.History
import UI.InputDialog

searchInputUI :: GopherBrowserState -> [T.Widget MyName]
searchInputUI gbs = inputDialogUI editorBuffer labelText helpText
  where
    searchBuffer = gbsBuffer gbs
    editorBuffer = sbEditorState (gbsBuffer gbs)
    labelText = "Search: " ++ (sbHost searchBuffer)
    helpText = "Press ENTER to search"

-- XXX: how will location be done? FIXME this is broke currently...
mkSearchResponseState :: GopherBrowserState -> IO GopherBrowserState
mkSearchResponseState gbs = do
  let host = (sbHost $ gbsBuffer gbs)
      port = (sbPort $ gbsBuffer gbs)
      resource = (sbSelector $ gbsBuffer gbs)
      query = unlines (E.getEditContents $ sbEditorState $ gbsBuffer gbs)
  (o, selector) <- searchGet host (show port) resource query
  let newMenu = makeGopherMenu o
      location = (host, port, selector, MenuMode)
  pure $ newStateForMenu newMenu location (newChangeHistory gbs location)
  -- XXX finish

--editorEventHandler :: GopherBrowserState -> Event -> T.EventM MyName Buffer
editorEventHandler :: GopherBrowserState -> Event -> T.EventM MyName GopherBrowserState
editorEventHandler gbs e = updateEditorInBuffer <$> E.handleEditorEvent e (sbEditorState $ gbsBuffer gbs)
  where
    updateEditorInBuffer x = gbs { gbsBuffer = (gbsBuffer gbs) { sbEditorState = x } }

searchEventHandler :: GopherBrowserState -> Event -> T.EventM MyName (T.Next GopherBrowserState)
searchEventHandler gbs e =
  case e of
    V.EvKey V.KEsc [] -> M.continue $ returnSearchFormerState gbs
    -- FIXME: needs to make search request
    V.EvKey V.KEnter [] -> liftIO (mkSearchResponseState gbs) >>= M.continue
    _ -> M.continue =<< editorEventHandler gbs e
    --V.EvKey V.KBS [] -> M.continue $ updateQuery $ take (length curQuery - 1) curQuery
    --V.EvKey (V.KChar c) [] ->
    --  M.continue $ gbs { gbsBuffer = (gbsBuffer gbs) { sbQuery = curQuery ++ [c] } }
    --_ -> M.continue gbs
  where
    returnSearchFormerState g = g {gbsBuffer = (sbFormerBufferState $ gbsBuffer g), gbsRenderMode = MenuMode}
