-- | Dialog for opening a Gopher URI dialog/UI.
module UI.Goto where

import           Control.Monad.IO.Class
import           Data.List

import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Widgets.Edit            as E
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events      ( Event )
import           Network.URI

import           UI.Popup
import           UI.Representation
import           UI.Progress

initGotoMode :: GopherBrowserState -> GopherBrowserState
initGotoMode gbs = gbs
  { gbsRenderMode = GotoMode
  , gbsBuffer     = GotoBuffer $ Goto
                      { gFormerBufferState = gbsBuffer gbs
                      , gEditorState       = E.editor MyViewport Nothing ""
                      }
  }

-- | Draw the search prompt. Used by UI.drawUI if the gbsRenderMode
-- is SearchMode.
gotoInputUI :: GopherBrowserState -> [T.Widget MyName]
gotoInputUI gbs = inputPopupUI editorBuffer labelText helpText
 where
  editorBuffer = gEditorState (getGoto gbs)
  labelText    = "Go To"
  helpText     = "Press ENTER to open the Gopher URI."

-- FIXME: what if bad input?! what if can't resolve? errors in network need better handling
mkGotoResponseState :: GopherBrowserState -> IO GopherBrowserState
mkGotoResponseState gbs = do
  -- get the host, port, selector
  let unparsedURI = filter (/= '\n')
        $ unlines (E.getEditContents $ gEditorState $ getGoto gbs)
      unparsedWithScheme | "gopher://" `isPrefixOf` unparsedURI = unparsedURI
                         | otherwise = "gopher://" ++ unparsedURI
      maybeParsedURI = parseURI unparsedWithScheme
      parsedURI      = case maybeParsedURI of
        (Just u) -> u
        Nothing  -> error $ "Invalid URI: " ++ show unparsedWithScheme
      authority' = case uriAuthority parsedURI of
        (Just a) -> a
        Nothing ->
          error $ "Invalid URI (no authority): " ++ show unparsedWithScheme
      port = case uriPort authority' of
        ""  -> 70
        (p) -> read $ tail p :: Int
      host = case uriRegName authority' of
        ""  -> error $ "Invalid URI (no host): " ++ show unparsedWithScheme
        (h) -> h
      resource = case uriPath parsedURI of
        ""  -> ""
        (r) -> r
  -- What if it's not a menu? FIXME
  initProgressMode gbs (host, port, resource, MenuMode)
  -- FIXME: TODO: Must return a better dummy state...
  -- this is already done by init progress
  -- Might have to return progress mode or something idk I'll have to look at
  -- other source... I woryr if I don't then there will be a memory leak...

-- | The Brick application event handler for search mode. See: UI.appEvent and
--- Brick.Main.appHandleEvent.
gotoEventHandler
  :: GopherBrowserState -> Event -> T.EventM MyName (T.Next GopherBrowserState)
gotoEventHandler gbs e = case e of
    -- FIXME: esc quits! Change key...
  V.EvKey V.KEsc   [] -> M.continue $ returnGotoFormerState gbs
  V.EvKey V.KEnter [] -> liftIO (mkGotoResponseState gbs) >>= M.continue
  _                   -> M.continue =<< editorEventHandler gbs e
 where
  returnGotoFormerState g =
    g { gbsBuffer = gFormerBufferState $ getGoto g, gbsRenderMode = MenuMode }

  -- | A modification of the default Brick.Widgets.Edit event handler; changed to
  -- return a GopherBrowserState instead of just an editor state.
  editorEventHandler
    :: GopherBrowserState -> Event -> T.EventM MyName GopherBrowserState
  editorEventHandler gbs' e' =
    let updateEditorInBuffer x =
            updateGotoBuffer gbs' (\s -> s { gEditorState = x })
    in  updateEditorInBuffer
          <$> E.handleEditorEvent e' (gEditorState $ getGoto gbs)
