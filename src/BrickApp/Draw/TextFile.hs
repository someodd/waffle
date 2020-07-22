{-# LANGUAGE OverloadedStrings #-}

module BrickApp.Draw.TextFile where

import qualified Brick.Types                   as T
import           Brick.Widgets.Core             ( txt )

import BrickApp.Types.Names
import BrickApp.Types.Helpers
import BrickApp.Types
import BrickApp.Utils

textFileModeUI :: GopherBrowserState -> [T.Widget AnyName]
textFileModeUI gbs = defaultOptimizedUI gbs titleWidget mainWidget statusWidget
  where
   mainWidget   = tfContents $ getTextFile gbs
   titleWidget  = txt $ tfTitle $ getTextFile gbs
   statusWidget = txt "? for help. Text file mode."
