{-# LANGUAGE OverloadedStrings #-}

-- | Stitch together the Brick app from the disparate parts of the UI.
--
-- Build initiate the Brick.Main.App. All UI modules/modes, such as UI.Save and
-- UI.Menu, converge here, into drawUI (for the Brick app's Brick.Main.appDraw)
-- which picks which module/mode's drawing function to use and likewise into appEvent
-- (for the Brick app's Brick.Main.appHandleEvent) which picks which module/mode's event
-- handler to use, both are picked based on the current gbsRenderMode set in the Brick
-- application state (GopherBrowserState).
module UI
  ( uiMain
  )
where


import qualified Data.Text                     as T
import           Data.Maybe
import           Control.Monad.IO.Class
import           Control.Monad                  ( void )

import qualified Brick.BChan                   as B
import qualified Brick.Main                    as B
import qualified Brick.Types                   as B
import qualified Graphics.Vty                  as V

import           UI.Menu
import           UI.Progress
import           UI.Save
import           UI.Search
import           UI.Style
import           UI.TextFile
import           UI.Help
import           UI.Representation
import           UI.Goto

-- | The draw handler which will choose a UI based on the browser's mode.
-- | Picks a UI/draw function based on the current gbsRenderMode.
--
-- Used as Brick.Main.appDraw when constructing the Brick app.
drawUI :: GopherBrowserState -> [B.Widget MyName]
drawUI gbs = modeUI $ gbsRenderMode gbs
  where
   modeUI mode = case mode of
     MenuMode        -> menuModeUI gbs
     TextFileMode    -> textFileModeUI gbs
     HelpMode        -> helpModeUI gbs
     FileBrowserMode -> fileBrowserUi gbs
     SearchMode      -> searchInputUI gbs
     ProgressMode    -> drawProgressUI gbs
     GotoMode        -> modeUI (seFormerMode $ fromJust $ gbsStatus gbs)

-- FIXME: shouldn't history be handled top level and not in individual handlers? or are there
-- some cases where we don't want history available
--
-- | The Brick application event handler which chooses which event handler to use based
-- on the current gbsRenderMode.
--
-- Used for Brick.Main.appHandleEvent.
appEvent
  :: GopherBrowserState
  -> B.BrickEvent MyName CustomEvent
  -> B.EventM MyName (B.Next GopherBrowserState)
appEvent gbs (B.VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = B.halt gbs
appEvent gbs (B.VtyEvent (V.EvKey (V.KChar 'g') [V.MCtrl])) =
  B.continue $ initGotoMode gbs
-- TODO: needs to reset viewport
appEvent gbs (B.VtyEvent (V.EvKey (V.KChar '?') [])) =
  liftIO (modifyGbsForHelp gbs) >>= B.continue
-- FIXME: this could be easily fixed just by doing appEvent gbs e instead of vtyevent
-- and leaving it up to eventhandlers
-- What about above FIXME... event types should be deicphered by event handler?
-- FIXME: just do vague event type discerning and don't say B.VtyEvent so it leaves it
-- to the event handlers in case they want custom events
appEvent gbs (B.VtyEvent e)
  | gbsRenderMode gbs == MenuMode        = menuEventHandler gbs e
  | gbsRenderMode gbs == TextFileMode    = textFileEventHandler gbs e
  | gbsRenderMode gbs == HelpMode        = helpEventHandler gbs e
  | gbsRenderMode gbs == FileBrowserMode = saveEventHandler gbs e
  | gbsRenderMode gbs == SearchMode      = searchEventHandler gbs e
  | gbsRenderMode gbs == GotoMode        = gotoEventHandler gbs e
  -- FIXME: two separate ones because of the way we pass events and pattern match
  -- i.e., one for vtyhandler and one for the custom app events, which we should
  -- soon conflate by not matching specifically for VtyEvent (thus passing all events
  -- to the appropriate mode's handler)
  | gbsRenderMode gbs == ProgressMode = progressEventHandler gbs (Right e)
  |
    otherwise                            = error $ "Unrecognized mode in event: " ++ show (gbsRenderMode gbs)
-- Seems hacky FIXME (for customevent)
appEvent gbs e
  | gbsRenderMode gbs == ProgressMode = progressEventHandler gbs (Left e)
  | otherwise                         = B.continue gbs

theApp :: B.App GopherBrowserState CustomEvent MyName
theApp = B.App { B.appDraw         = drawUI
               , B.appChooseCursor = B.showFirstCursor
               , B.appHandleEvent  = appEvent
               , B.appStartEvent   = return
               , B.appAttrMap      = const theMap
               }

-- FIXME: isn't there a way to infer a location's type? Assuming first
-- link is a menu is a horrible hack...
--
-- | Start the Brick app at a specific Gopher menu in Gopherspace.
uiMain :: Maybe (T.Text, Int, T.Text) -> IO ()
uiMain possibleLocation = do
  eventChan <- B.newBChan 10
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  let dummyStateToOverride = GopherBrowserState {
      gbsBuffer = TextFileBuffer $ TextFile
                    { tfContents = ""
                    , tfTitle = ""
                    }
    , gbsLocation = ("", 0, "", TextFileMode)
    , gbsRenderMode = TextFileMode
    , gbsHistory = ([], -1)
    , gbsChan = eventChan
    , gbsPopup = Nothing
    , gbsCache = emptyCache
    , gbsStatus = Nothing
    }

  initialState <- if null possibleLocation then
      -- if we didn't get a location passed to us, then we want to
      -- start with the help page!
      modifyGbsForHelp dummyStateToOverride
    else
      -- ...otherwise let's open the page supplied!
      let (host, port, magicString) = fromJust possibleLocation
          trueLocationType = (host, port, magicString, MenuMode)
          -- FIXME: what a horrible hack to produce a beginning state in order
          -- to use initProgressMode! Especially the buffer part...
          history = ([trueLocationType], 0)
          initialGbs = dummyStateToOverride
            { gbsBuffer = TextFileBuffer $ TextFile
                            { tfContents = ""
                            , tfTitle = ""
                            }
            , gbsLocation = trueLocationType
            , gbsRenderMode = MenuMode
            , gbsHistory = history
            }
      in initProgressMode initialGbs (Just history) trueLocationType

  void $ B.customMain initialVty buildVty (Just eventChan) theApp initialState
