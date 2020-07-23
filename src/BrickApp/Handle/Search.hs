{-# LANGUAGE OverloadedStrings #-}

module BrickApp.Handle.Search where

import           Control.Monad.IO.Class

import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Widgets.Edit            as E
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events      ( Event )

import BrickApp.Types
import BrickApp.Types.Names
import BrickApp.Types.Helpers
import BrickApp.ModeAction.Search

-- | The Brick application event handler for search mode. See: UI.appEvent and
--- Brick.Main.appHandleEvent.
searchEventHandler
  :: GopherBrowserState -> Event -> T.EventM AnyName (T.Next GopherBrowserState)
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
    :: GopherBrowserState -> Event -> T.EventM AnyName GopherBrowserState
  editorEventHandler gbs' e' =
    let updateEditorInBuffer x =
            updateSearchBuffer gbs' (\s -> s { sbEditorState = x })
    in  updateEditorInBuffer
          <$> E.handleEditorEvent e' (sbEditorState $ getSearch gbs)
