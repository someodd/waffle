{-# LANGUAGE OverloadedStrings #-}

-- | Draw/render `SaveMode`
module BrickApp.Draw.Save where

import qualified Data.Text                     as T
import           Control.Exception              ( displayException )

import qualified Brick.Widgets.FileBrowser     as FB
import qualified Brick.Types                   as T
import           Brick.Widgets.Center           ( center
                                                , hCenter
                                                )
import           Brick.Widgets.Border           ( borderWithLabel )
import           Brick.Widgets.Core             ( vLimitPercent
                                                , hLimitPercent
                                                , (<=>)
                                                , txt
                                                , padTop
                                                , vBox
                                                , emptyWidget
                                                , str
                                                , withDefAttr
                                                )

import           BrickApp.Types.Names
import           BrickApp.Types.Helpers
import           BrickApp.Types
import           BrickApp.Utils.Style

-- FIXME
fileBrowserUi :: GopherBrowserState -> [T.Widget AnyName]
fileBrowserUi gbs =
  [center $ vLimitPercent 100 $ hLimitPercent 100 $ ui <=> help]
 where
  b = fromBuffer $ getSaveBrowser gbs
  fromBuffer = fbFileBrowser
  ui = hCenter $ borderWithLabel (txt "Choose a file") $ FB.renderFileBrowser
    True
    b
  help = padTop (T.Pad 1) $ vBox
    [ case FB.fileBrowserException b of
      Nothing -> emptyWidget
      Just e ->
        hCenter $ withDefAttr errorAttr $ txt $ T.pack $ displayException e
    , hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
    , hCenter $ txt "Esc: quit/cancel save"
    , hCenter $ txt "n: name the output file and then hit enter"
    , hCenter $ str $ fbFileOutPath (getSaveBrowser gbs)
    ]
