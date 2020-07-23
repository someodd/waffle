{-# LANGUAGE OverloadedStrings #-}

-- | Draw `HelpMode`.
module BrickApp.Draw.Help where

import qualified Brick.Types                   as T
import           Brick.Widgets.Core             ( viewport
                                                , txt
                                                )

import BrickApp.Types.Names
import BrickApp.Types.Helpers
import BrickApp.Types
import BrickApp.Utils

helpModeUI :: GopherBrowserState -> [T.Widget AnyName]
helpModeUI gbs = defaultBrowserUI gbs (viewport (MyName TextViewport) T.Both) titleWidget mainWidget statusWidget
  where
   mainWidget   = getHelpTextFileContents gbs
   titleWidget  = txt "Waffle Help"
   statusWidget = txt "Help mode. Use arrows or hjkl to scroll."
