-- | Functions for actually drawing the application state according to the current
-- `RenderMode`, which is used by this Brick application's `Brick.Main.appDraw`.
module BrickApp.Draw where

import Data.Maybe

import Brick.Types as B

import BrickApp.Draw.Open ( openConfigModeUI )
import BrickApp.Types
import BrickApp.Types.Names
import BrickApp.Draw.Menu
import BrickApp.Draw.Help
import BrickApp.Draw.Save
import BrickApp.Draw.Search
import BrickApp.Draw.TextFile
import BrickApp.Draw.Progress

-- | The draw handler which will choose a UI based on the browser's mode.
-- | Picks a UI/draw function based on the current gbsRenderMode.
--
-- Used as Brick.Main.appDraw when constructing the Brick app.
drawUI :: GopherBrowserState -> [B.Widget AnyName]
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
     MenuJumpMode    -> modeUI (seFormerMode $ fromJust $ gbsStatus gbs)
     OpenConfigMode  -> openConfigModeUI gbs
