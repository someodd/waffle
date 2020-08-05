module BrickApp.Handle.Menu.Find where

import           Data.Maybe                               ( fromJust )

import qualified Brick.Main                    as B
import qualified Brick.Types                   as T
import qualified Graphics.Vty                  as V
import           Brick.Widgets.Edit            as E

-- import           BrickApp.ModeAction.Progress             ( initProgressMode )
import           BrickApp.ModeAction.Menu.Find
import           BrickApp.Types
import           BrickApp.Utils
import           BrickApp.Types.Names

-- REDUNDANT FIXME (move to Utils/Status mayb?

menuFindEventHandler
  :: GopherBrowserState -> V.Event -> T.EventM AnyName (T.Next GopherBrowserState)
menuFindEventHandler gbs e = case e of
    -- FIXME: esc quits! Change key...
  V.EvKey V.KEsc   [] -> B.continue $ statusEditorFormerMode gbs
  -- On enter save bookmark with the name inputted
  -- V.EvKey V.KEnter [] -> liftIO (bookmarkCurrentLocation gbs) >>= B.continue
  _                   -> B.continue =<< editorEventHandler gbs e
 where
  -- FIXME: this should just be in utils!
  -- | A modification of the default Brick.Widgets.Edit event handler; changed to
  -- return a GopherBrowserState instead of just an editor state.
  editorEventHandler
    :: GopherBrowserState -> V.Event -> T.EventM AnyName GopherBrowserState
  -- TODO: e' is unused!
  editorEventHandler _ e' =
    -- Maybe this should be a general function in Representation.
    let updateEditorInStatus x = selectFirstFound $ gbs { gbsStatus = Just $ (fromJust $ gbsStatus gbs) { seEditorState = x } }
    in  updateEditorInStatus
          <$> E.handleEditorEvent e' (seEditorState $ fromJust $ gbsStatus gbs)
