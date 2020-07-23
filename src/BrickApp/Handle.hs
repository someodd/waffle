-- | Brick application event handlers, depending on the current `RenderMode`, as used
-- by this Brick application's `Brick.Main.appHandleEvent`.
module BrickApp.Handle where

import           Control.Monad.IO.Class

import qualified Brick.Main                    as B
import qualified Brick.Types                   as B
import qualified Graphics.Vty                  as V

import BrickApp.Types.Names
import BrickApp.Types.Helpers
import BrickApp.Types
import BrickApp.ModeAction.Help
import BrickApp.ModeAction.Goto
import BrickApp.ModeAction.Open
import BrickApp.Handle.Open
import BrickApp.Handle.Progress
import BrickApp.Handle.Menu
import BrickApp.Handle.Menu.Jump
import BrickApp.Handle.Goto
import BrickApp.Handle.TextFile
import BrickApp.Handle.Search
import BrickApp.Handle.Help
import BrickApp.Handle.Save

appropriateHandler :: GopherBrowserState -> V.Event -> B.EventM AnyName (B.Next GopherBrowserState)
appropriateHandler gbs e = case gbsRenderMode gbs of
  MenuMode -> menuEventHandler gbs e
  TextFileMode -> textFileEventHandler gbs e
  HelpMode -> helpEventHandler gbs e
  FileBrowserMode -> saveEventHandler gbs e
  SearchMode -> searchEventHandler gbs e
  GotoMode -> gotoEventHandler gbs e
  MenuJumpMode -> jumpEventHandler gbs e
  OpenConfigMode -> openConfigEventHandler gbs e
  -- FIXME: two separate ones because of the way we pass events and pattern match
  -- i.e., one for vtyhandler and one for the custom app events, which we should
  -- soon conflate by not matching specifically for VtyEvent (thus passing all events
  -- to the appropriate mode's handler)
  ProgressMode -> progressEventHandler gbs (Right e)

eventDependingMode
  :: GopherBrowserState
  -> RenderMode
  -> B.EventM AnyName (B.Next GopherBrowserState)
  -> B.EventM AnyName (B.Next GopherBrowserState)
  -> B.EventM AnyName (B.Next GopherBrowserState)
eventDependingMode gbs someRenderMode doThisIfMode doThisIfNotMode
  | gbsRenderMode gbs /= someRenderMode = doThisIfNotMode
  | otherwise          = doThisIfMode

-- FIXME: shouldn't history be handled top level and not in individual handlers? or are there
-- some cases where we don't want history available
--
-- | The Brick application event handler which chooses which event handler to use based
-- on the current gbsRenderMode.
--
-- Used for Brick.Main.appHandleEvent.
appEvent
  :: GopherBrowserState
  -> B.BrickEvent AnyName CustomEvent
  -> B.EventM AnyName (B.Next GopherBrowserState)
appEvent gbs (B.VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = B.halt gbs
-- Close a popup if there is one, otherwise forward to appropriate handler!
appEvent gbs (B.VtyEvent e@(V.EvKey V.KEsc []))
  | hasPopup gbs = B.continue $ closePopup gbs
  | otherwise   = appropriateHandler gbs e
-- FIXME
-- This is the config mode, which currently just goes right into the menu item
-- command association editor.
appEvent gbs (B.VtyEvent e@(V.EvKey (V.KChar 'c') [V.MCtrl])) =
  eventDependingMode gbs OpenConfigMode (openConfigEventHandler gbs e) (initConfigOpenMode gbs)
appEvent gbs (B.VtyEvent e@(V.EvKey (V.KChar 'g') [V.MCtrl])) =
  eventDependingMode gbs GotoMode (appropriateHandler gbs e) (B.continue $ initGotoMode gbs)
-- TODO: needs to reset viewport
appEvent gbs (B.VtyEvent e@(V.EvKey (V.KChar '?') [])) =
  eventDependingMode gbs HelpMode (appropriateHandler gbs e) (liftIO (modifyGbsForHelp gbs) >>= B.continue)
-- FIXME: this could be easily fixed just by doing appEvent gbs e instead of vtyevent
-- and leaving it up to eventhandlers
-- What about above FIXME... event types should be deicphered by event handler?
-- FIXME: just do vague event type discerning and don't say B.VtyEvent so it leaves it
-- to the event handlers in case they want custom events
appEvent gbs (B.VtyEvent e) = appropriateHandler gbs e
-- Seems hacky FIXME (for customevent)
appEvent gbs (B.AppEvent (ClearCacheEvent cce)) = cce >> B.continue gbs
appEvent gbs e
  | gbsRenderMode gbs == ProgressMode = progressEventHandler gbs (Left e)
  | otherwise                         = B.continue gbs
