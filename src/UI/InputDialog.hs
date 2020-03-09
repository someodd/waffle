-- | A single-line input dialog box.
module UI.InputDialog where

import qualified Brick.Types as T
import Brick.Widgets.Center (center, hCenter, vCenter)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Core (viewport, vBox, str, hLimit, vLimit, padLeftRight, padTop, padTopBottom, withAttr)

import UI.Util
import UI.Style

-- FIXME: implement editor from Brick?
-- https://hackage.haskell.org/package/brick-0.52/docs/Brick-Widgets-Edit.html#v:editorText
inputDialogUI :: String -> String -> String -> [T.Widget MyName]
inputDialogUI inputString label helpString =
  let ui = vCenter $ hCenter $ borderWithLabel (padLeftRight 1 $ str label) $ padTopBottom 1 $ padLeftRight 4 $ hLimit 100 $ vLimit 3 $ viewport MyViewport T.Both $ vBox [withAttr inputFieldAttr $ padLeftRight 1 $ str inputString, padTop (T.Pad 1) $ str helpString]
  in [center $ ui]
