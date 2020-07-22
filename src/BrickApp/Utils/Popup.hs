-- FIXME TODO: rename to DialogPopup?
-- | A single-line input dialog box.
module BrickApp.Utils.Popup
  ( popOver
  , popup
  , inputPopupUI
  ) where

import qualified Data.Text                     as T
import qualified Brick.Widgets.Edit            as E
import qualified Brick.Types                   as T
import           Brick.Widgets.Center           ( center
                                                , hCenter
                                                , vCenter
                                                )
import           Brick.Widgets.Border           ( borderWithLabel )
import           Brick.AttrMap                  ( applyAttrMappings )
import           Brick.Widgets.Core             ( vBox
                                                , txt
                                                , hLimit
                                                , vLimit
                                                , withBorderStyle
                                                , padLeftRight
                                                , padTop
                                                , padTopBottom
                                                , withAttr
                                                , updateAttrMap
                                                )

import           BrickApp.Utils.Style
import           BrickApp.Types.Names

-- | A vertically-centered, full-width popup box to be displayed over other widgets by putting it before
-- the other widgets in the list of widgets being rendered.
popup :: T.Text -> [T.Widget AnyName] -> T.Text -> [T.Widget AnyName]
popup label widgets helpString =
  let
    ui =
      updateAttrMap (applyAttrMappings borderMappingsInputDialog)
        $ withBorderStyle inputDialogBorder
        $ borderWithLabel
            (withAttr inputDialogLabelAttr $ padLeftRight 1 $ txt label)
        $ hCenter
        $ padLeftRight 4
        $ padTopBottom 1
        $ padLeftRight 4
        $ hLimit 100
        $ vLimit 3
        $ vBox
            [ vBox widgets
            , padTop (T.Pad 1) $ txt helpString
            ]
  in  [ui]

-- | A full-screen popup which displays nothing underneath it. Centered vertically and horizontally.
popOver :: T.Text -> [T.Widget AnyName] -> T.Text -> [T.Widget AnyName]
popOver label mainWidget helpString =
  let
    ui =
      vCenter
        $ hCenter
        $ padLeftRight 4
        (head $ popup label mainWidget helpString)
  in  [center ui]

inputPopupUI :: E.Editor T.Text AnyName -> T.Text -> T.Text -> [T.Widget AnyName]
inputPopupUI editorState label helpString = popOver label [editorWidget] helpString
  where
   editorWidget = withAttr inputFieldAttr $ E.renderEditor (txt . T.unlines) True editorState
