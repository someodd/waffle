-- | Dialog for opening a Gopher URI dialog/UI.
module UI.Goto where

import           Data.Maybe
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
  , gbsStatus     = Just $ StatusEditor { seLabel = "Goto: "
                                        , seEditorState = E.editor EditorViewport Nothing ""
                                        , seFormerMode = gbsRenderMode gbs
                                        }
  }

-- FIXME: what if bad input?! what if can't resolve? errors in network need better handling
-- FIXME: what if NOT a menu!
mkGotoResponseState :: GopherBrowserState -> IO GopherBrowserState
mkGotoResponseState gbs =
  -- get the host, port, selector
  let unparsedURI = filter (/= '\n')
        $ unlines (E.getEditContents $ seEditorState $ fromJust $ gbsStatus gbs)
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
        p -> read $ tail p :: Int
      host = case uriRegName authority' of
        ""  -> error $ "Invalid URI (no host): " ++ show unparsedWithScheme
        h -> h
      resource = case uriPath parsedURI of
        ""  -> ""
        r -> r
  -- What if it's not a menu? FIXME
  in initProgressMode gbs Nothing (host, port, resource, MenuMode)
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
  V.EvKey V.KEsc   [] -> M.continue $ formerMode gbs
  V.EvKey V.KEnter [] -> liftIO (mkGotoResponseState gbs) >>= M.continue
  _                   -> M.continue =<< editorEventHandler gbs e
 where
  -- FIXME: should also reset status
  formerMode g = g { gbsRenderMode = seFormerMode $ fromJust $ gbsStatus g, gbsStatus = Nothing }

  -- | A modification of the default Brick.Widgets.Edit event handler; changed to
  -- return a GopherBrowserState instead of just an editor state.
  editorEventHandler
    :: GopherBrowserState -> Event -> T.EventM MyName GopherBrowserState
  editorEventHandler gbs' e' =
    -- Maybe this should be a general function in Representation.
    let updateEditorInStatus x = gbs { gbsStatus = Just $ (fromJust $ gbsStatus gbs) { seEditorState = x } }
    in  updateEditorInStatus
          <$> E.handleEditorEvent e' (seEditorState $ fromJust $ gbsStatus gbs)
