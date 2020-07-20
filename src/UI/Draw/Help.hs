{-# LANGUAGE OverloadedStrings #-}

-- | Draw `HelpMode`.
module UI.Draw.Help where

import qualified Brick.Types                   as T
import           Brick.Widgets.Core             ( viewport
                                                , txt
                                                )

import UI.Types.Names
import UI.Types.Helpers
import UI.Types
import UI.Utils

helpModeUI :: GopherBrowserState -> [T.Widget AnyName]
helpModeUI gbs = defaultBrowserUI gbs (viewport (MyName TextViewport) T.Both) titleWidget mainWidget statusWidget
  where
   mainWidget   = getHelpTextFileContents gbs
   titleWidget  = txt "Waffle Help"
   statusWidget = txt "Help mode. Use arrows or hjkl to scroll."
