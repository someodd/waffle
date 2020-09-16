{-# LANGUAGE OverloadedStrings #-}

-- | Stitch together the Brick app from the disparate parts of the UI.
--
-- The UI module and its child modules are all about `Brick` stuff.
module BrickApp
  ( uiMain
  )
where


import qualified Data.Text                     as T
import           Data.Maybe
import           Control.Monad                  ( void )

import qualified Brick.BChan                   as B
import qualified Brick.Main                    as B
import qualified Graphics.Vty                  as V
import           Brick.Widgets.Core             ( txt )

import Homepage
import BrickApp.Types
import BrickApp.Types.Names
import BrickApp.Utils
import BrickApp.Draw
import BrickApp.Handle
import BrickApp.ModeAction.Progress
import BrickApp.Utils.Style

theApp :: B.App GopherBrowserState CustomEvent AnyName
theApp = B.App { B.appDraw         = drawUI
               , B.appChooseCursor = B.showFirstCursor
               , B.appHandleEvent  = appEvent
               , B.appStartEvent   = return
               , B.appAttrMap      = const theMap
               }

-- FIXME: interpret homepage from config
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
                    { tfContents = txt ""
                    , tfTitle = ""
                    }
    , gbsLocation = ("", 0, "", TextFileMode, Nothing)
    , gbsRenderMode = TextFileMode
    , gbsHistory = ([], -1)
    , gbsChan = eventChan
    , gbsPopup = Nothing
    , gbsCache = emptyCache
    , gbsStatus = Nothing
    }

  initialState <- if null possibleLocation then do
      -- if we didn't get a location passed to us, then we want to
      -- start with the home page from the config!
      goHome dummyStateToOverride
      -- modifyGbsForHelp dummyStateToOverride
    else
      -- ...otherwise let's open the page supplied!
      let (host, port, magicString) = fromJust possibleLocation
          trueLocationType = (host, port, magicString, selectorToRenderMode magicString, Nothing)
          -- FIXME: what a horrible hack to produce a beginning state in order
          -- to use initProgressMode! Especially the buffer part...
          history = ([trueLocationType], 0)
          initialGbs = dummyStateToOverride
            { gbsBuffer = TextFileBuffer $ TextFile
                            { tfContents = txt ""
                            , tfTitle = ""
                            }
            , gbsLocation = trueLocationType
            , gbsRenderMode = selectorToRenderMode magicString
            , gbsHistory = history
            }
      in initProgressMode initialGbs (Just history) trueLocationType

  theme <- getTheme
  void $ B.customMain initialVty buildVty (Just eventChan) (theApp {B.appAttrMap=const theme}) initialState
