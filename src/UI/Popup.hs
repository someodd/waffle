-- FIXME TODO: rename to DialogPopup?
-- | A single-line input dialog box.
module UI.Popup
  ( popOver
  , popup
  , inputPopupUI
  ) where

import qualified Brick.Widgets.Edit            as E
import qualified Brick.Types                   as T
import           Brick.Widgets.Center           ( center
                                                , hCenter
                                                , vCenter
                                                )
import           Brick.Widgets.Border           ( borderWithLabel )
import           Brick.AttrMap                  ( applyAttrMappings )
import           Brick.Widgets.Core             ( vBox
                                                , str
                                                , hLimit
                                                , vLimit
                                                , withBorderStyle
                                                , padLeftRight
                                                , padTop
                                                , padTopBottom
                                                , withAttr
                                                , updateAttrMap
                                                )

import           UI.Style
import           UI.Representation

-- | A vertically-centered, full-width popup box to be displayed over other widgets by putting it before
-- the other widgets in the list of widgets being rendered.
popup :: String -> [T.Widget MyName] -> String -> [T.Widget MyName]
popup label widgets helpString =
  let
    ui =
      updateAttrMap (applyAttrMappings borderMappingsInputDialog)
        $ withBorderStyle inputDialogBorder
        $ borderWithLabel
            (withAttr inputDialogLabelAttr $ padLeftRight 1 $ str label)
        $ hCenter
        $ padLeftRight 4
        $ padTopBottom 1
        $ padLeftRight 4
        $ hLimit 100
        $ vLimit 3
        $ vBox
            [ vBox widgets
            , padTop (T.Pad 1) $ str helpString
            ]
  in  [ui]

-- | A full-screen popup which displays nothing underneath it. Centered vertically and horizontally.
popOver :: String -> [T.Widget MyName] -> String -> [T.Widget MyName]
popOver label mainWidget helpString =
  let
    ui =
      vCenter
        $ hCenter
        $ padLeftRight 4
        $ (head $ popup label mainWidget helpString)
  in  [center ui]

inputPopupUI :: E.Editor String MyName -> String -> String -> [T.Widget MyName]
inputPopupUI editorState label helpString = popOver label [editorWidget] helpString
  where
   editorWidget = withAttr inputFieldAttr $ E.renderEditor (str . unlines) True editorState
