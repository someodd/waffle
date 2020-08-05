{-# LANGUAGE OverloadedStrings #-}

-- | Event handling for `MenuMode`.
module BrickApp.Handle.Menu where

import           Data.Char                      ( isDigit, digitToInt )
import           Control.Monad.IO.Class

import qualified Graphics.Vty                  as V
import qualified Brick.Main                    as M
import qualified Brick.Widgets.Edit            as B
import qualified Brick.Widgets.List            as L
import qualified Brick.Types                   as T

import BrickApp.Types
import BrickApp.Types.Names
import BrickApp.Types.Helpers
import BrickApp.Handle.Menu.Jump
import BrickApp.ModeAction.Progress
import BrickApp.ModeAction.Menu
import BrickApp.ModeAction.Menu.State
import BrickApp.Utils

-- This belongs in ModeAction FIXME
initMenuFindMode :: GopherBrowserState -> GopherBrowserState
initMenuFindMode gbs = gbs
  { gbsRenderMode = MenuFindMode
  , gbsStatus     = Just $ StatusEditor { seLabel = "Find item: "
                                        , seEditorState = B.editor (MyName EditorViewport) Nothing ""
                                        , seFormerMode = gbsRenderMode gbs
                                        }
  }

menuEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM AnyName (T.Next GopherBrowserState)
menuEventHandler gbs e
  |
  -- Handle a popup (esc key to dismiss) while there is a popup present...
    hasPopup gbs = case e of
    V.EvKey V.KEsc [] -> M.continue $ closePopup gbs
    _                 -> M.continue gbs
  |
  --- Handle controlling the menu.
    otherwise = case e of
    V.EvKey (V.KChar 'i') [] -> M.continue $ lineInfoPopup gbs
    V.EvKey V.KEnter [] ->
      liftIO (newStateFromSelectedMenuItem gbs) >>= M.continue
    V.EvKey (V.KChar 'o') [] ->
      liftIO (newStateFromOpenItem gbs) >>= M.continue
    V.EvKey (V.KChar 'l') [] ->
      M.hScrollBy menuViewportScroll 1 >> M.continue gbs
    V.EvKey (V.KChar 'h') [] ->
      M.hScrollBy menuViewportScroll (-1) >> M.continue gbs
    V.EvKey (V.KChar 'j') [] ->
      M.continue $ newMenuBuffer gbs $ nextLine (getMenu gbs)
    V.EvKey (V.KChar 'k') [] ->
      M.continue $ newMenuBuffer gbs $ previousLine (getMenu gbs)
    V.EvKey V.KDown []       ->
      M.continue $ newMenuBuffer gbs $ nextLine (getMenu gbs)
    V.EvKey V.KUp []         ->
      M.continue $ newMenuBuffer gbs $ previousLine (getMenu gbs)
    V.EvKey (V.KChar 'n') [] ->
      M.continue $ newMenuBuffer gbs $ jumpNextLink (getMenu gbs)
    V.EvKey (V.KChar 'p') [] ->
      M.continue $ newMenuBuffer gbs $ jumpPrevLink (getMenu gbs)
    V.EvKey (V.KChar 'u') [] -> liftIO (goParentDirectory gbs) >>= M.continue
    V.EvKey (V.KChar 'f') [] -> liftIO (goHistory gbs 1) >>= M.continue
    V.EvKey (V.KChar 'b') [] -> liftIO (goHistory gbs (-1)) >>= M.continue
    V.EvKey (V.KChar '/') [] -> M.continue $ initMenuFindMode gbs
    -- FIXME: Implement jump to link # here...
    V.EvKey (V.KChar c) []   ->
      if isDigit c
        then initJumpMode gbs (digitToInt c)
        else M.continue gbs
    -- The following catch-all is to hand off the event to Brick's list handler (the special one with vi controls).
    ev -> M.continue =<< updateMenuList <$> L.handleListEventVi
      L.handleListEvent
      ev
      (getMenuList gbs)
 where
  getMenuList x = let (Menu (_, gl, _)) = getMenu x in gl
  updateMenuList x =
    let (Menu (gm, _, fl)) = getMenu gbs
    in  gbs { gbsBuffer = MenuBuffer $ Menu (gm, x, fl) }
