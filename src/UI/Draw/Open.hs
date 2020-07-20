{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Open where

import Brick.Widgets.Core
  ( vBox
  , str
  , txt
  , viewport
  )
import qualified Brick.Types                   as T

import UI.Types
import UI.Utils
import UI.Types.Names
import UI.ModeAction.Open

openConfigModeUI :: GopherBrowserState -> [T.Widget AnyName]
openConfigModeUI gbs =
  defaultBrowserUI gbs (viewport (MyName MyViewport) T.Vertical) titleWidget mainWidget statusWidget
 where
  {-
  fields =
    let renderEditor = E.renderEditor (str . unlines)
        fieldMaker = \x -> F.withFocusRing (focusRing openConfigState) renderEditor (x openConfigState)
    in  map fieldMaker ...
  -}
  openConfigState =
    let (OpenConfigBuffer ocs) = gbsBuffer gbs
    in  ocs
  titleWidget = txt "Config: Open Associations"
  statusWidget = txt "Set commands for item types. Use tab to cycle fields."

  vBoxHeader =
    [ str "Set the commands associated with opening specific menu item types."
    , str "Leave blank to use xdg-open."
    , str " "
    ]
  vBoxFooter =
    [ str " "
    , str "Press TAB to switch between editors, ESC to quit."
    ]

  mainWidget :: T.Widget AnyName
  mainWidget = vBox (vBoxHeader ++ editWidgets openConfigState ++ vBoxFooter)


