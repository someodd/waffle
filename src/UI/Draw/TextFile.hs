{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.TextFile where

import qualified Brick.Types                   as T
import           Brick.Widgets.Core             ( txt )

import UI.Types.Names
import UI.Types.Helpers
import UI.Types
import UI.Utils

textFileModeUI :: GopherBrowserState -> [T.Widget AnyName]
textFileModeUI gbs = defaultOptimizedUI gbs titleWidget mainWidget statusWidget
  where
   mainWidget   = tfContents $ getTextFile gbs
   titleWidget  = txt $ tfTitle $ getTextFile gbs
   statusWidget = txt "? for help. Text file mode."
