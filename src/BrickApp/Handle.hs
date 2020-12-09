{-# LANGUAGE OverloadedStrings #-}
-- | Brick application event handlers, depending on the current `RenderMode`, as used
-- by this Brick application's `Brick.Main.appHandleEvent`.
module BrickApp.Handle where

import           Control.Monad.IO.Class

import qualified Brick.Main                    as B
import qualified Brick.Types                   as B
import qualified Graphics.Vty                  as V

import BrickApp.Utils                           ( cacheRemove )
import BrickApp.Utils.Popup                     ( popupDialogEventHandler)
import BrickApp.Types.Names
import BrickApp.Types.Helpers
import BrickApp.Types
import BrickApp.ModeAction.Homepage             ( goHome, createHomeDialog )
import BrickApp.ModeAction.Help
import BrickApp.ModeAction.Goto
import BrickApp.ModeAction.Progress
import BrickApp.ModeAction.Open
import BrickApp.ModeAction.Bookmarks
import BrickApp.Handle.Open
import BrickApp.Handle.Bookmarks
import BrickApp.Handle.Progress
import BrickApp.Handle.Menu
import BrickApp.Handle.Menu.Jump
import BrickApp.Handle.Menu.Find
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
  BookmarksMode -> bookmarksEventHandler gbs e
  AddBookmarkMode -> addBookmarkEventHandler gbs e
  MenuFindMode -> menuFindEventHandler gbs e
  -- FIXME: two separate ones because of the way we pass events and pattern match
  -- i.e., one for vtyhandler and one for the custom app events, which we should
  -- soon conflate by not matching specifically for VtyEvent (thus passing all events
  -- to the appropriate mode's handler)
  ProgressMode -> progressEventHandler gbs (Right e)

-- | Preform the successEvent if the function produces
-- True when applied to the current RenderMode.
--
-- >>> doEventIfModeTrue gbs (`elem` [GotoMode, OpenConfigMode]) ...
doEventIfModeTrue
  :: GopherBrowserState
  -> (RenderMode -> Bool)
  -> B.EventM AnyName (B.Next GopherBrowserState)
  -> B.EventM AnyName (B.Next GopherBrowserState)
  -> B.EventM AnyName (B.Next GopherBrowserState)
doEventIfModeTrue gbs func successEvent failEvent
  | func $ gbsRenderMode gbs = successEvent
  | otherwise = failEvent


-- FIXME: the way I'm handling input here is bad. It relies on whitelisting which modes are allowed
-- to initiate a new mode/use a command. So for example, if I ever add a new thing that handles input,
-- the homepage event handler might active, unless homepage event handler is whitelisted, which then
-- a problem occurs where homepagemode might not be tripped if and when it actually should be during
-- a new mode that gets added!
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
appEvent gbs (B.VtyEvent e@(V.EvKey (V.KFun 5) [])) =
  let newCache = cacheRemove (gbsLocation gbs) (gbsCache gbs)
      newGbs   = gbs { gbsCache = newCache }
  in  doEventIfModeTrue gbs (`elem` [TextFileMode, MenuMode]) (liftIO (initProgressMode newGbs (Just $ gbsHistory gbs) (gbsLocation gbs)) >>= B.continue) (appropriateHandler gbs e)
-- popup logic catching
appEvent gbs@(GopherBrowserState{gbsPopup=(Just n)}) e = popupDialogEventHandler gbs n e
-- Close a popup if there is one, otherwise forward to appropriate handler!
appEvent gbs (B.VtyEvent e@(V.EvKey V.KEsc []))
  | hasPopup gbs = B.continue $ closePopup gbs
  | otherwise   = appropriateHandler gbs e
-- add new bookmark
appEvent gbs (B.VtyEvent e@(V.EvKey (V.KChar '+') [])) =
  doEventIfModeTrue gbs (`elem` [TextFileMode, MenuMode]) (B.continue $ initAddBookmarkMode gbs) (appropriateHandler gbs e)
-- FIXME
-- This is the config mode, which currently just goes right into the menu item
-- command association editor.
appEvent gbs (B.VtyEvent e@(V.EvKey (V.KChar 'c') [V.MCtrl])) =
  -- Why not just have this function defer to the appropriateHandler on failure?
  doEventIfModeTrue gbs (== OpenConfigMode) (openConfigEventHandler gbs e) (liftIO (initConfigOpenMode gbs) >>= B.continue)
-- Bookmark mode!
appEvent gbs (B.VtyEvent (V.EvKey (V.KChar 'b') [V.MCtrl])) =
  let successEvent = liftIO (initBookmarksMode gbs) >>= B.continue
      failureEvent = B.continue gbs
  in doEventIfModeTrue gbs (/= BookmarksMode) successEvent failureEvent
-- GotoMode
appEvent gbs (B.VtyEvent e@(V.EvKey (V.KChar 'g') [V.MCtrl])) =
  doEventIfModeTrue gbs (`elem` [BookmarksMode, MenuMode, TextFileMode, HelpMode]) (B.continue $ initGotoMode gbs) (appropriateHandler gbs e)
-- TODO: needs to reset viewport
appEvent gbs (B.VtyEvent e@(V.EvKey (V.KChar '?') [])) =
  doEventIfModeTrue gbs (== HelpMode) (appropriateHandler gbs e) (liftIO (modifyGbsForHelp gbs) >>= B.continue)
-- Go to the homepage
appEvent gbs (B.VtyEvent e@(V.EvKey (V.KChar 'h') [])) =
  doEventIfModeTrue gbs (`elem` [BookmarksMode, MenuMode, TextFileMode, HelpMode]) ((liftIO $ goHome gbs) >>= B.continue) (appropriateHandler gbs e)
-- Set the homepage
-- FIXME: why does ctrl h not work?
appEvent gbs (B.VtyEvent (V.EvKey (V.KChar 'z') [V.MCtrl])) =
  -- TODO/FIXME: bring up prompt about setting homepage
  B.continue (createHomeDialog gbs)
--
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
