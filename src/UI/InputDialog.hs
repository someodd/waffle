-- | A single-line input dialog box.
module UI.InputDialog where

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

import           UI.Util
import           UI.Style
import           UI.Representation

-- updateAttrMap (applyAttrMappings borderMappings) $ withBorderStyle inputDialogBorderAttr $ borderWithLabel (withAttr inputDialogLabelAttr $ str title)

-- FIXME: implement editor from Brick?
-- https://hackage.haskell.org/package/brick-0.52/docs/Brick-Widgets-Edit.html#v:editorText
inputDialogUI :: E.Editor String MyName -> String -> String -> [T.Widget MyName]
inputDialogUI editorState label helpString =
  let
    ui =
      vCenter
        $ hCenter
        $ padLeftRight 4
        $ updateAttrMap (applyAttrMappings borderMappingsInputDialog)
        $ withBorderStyle inputDialogBorder
        $ borderWithLabel
            (withAttr inputDialogLabelAttr $ padLeftRight 1 $ str label)
        $ withAttr inputDialogAttr
        $ padTopBottom 1
        $ padLeftRight 4
        $ hLimit 100
        $ vLimit 3
        $ vBox
            [ withAttr inputFieldAttr editorField
            , padTop (T.Pad 1) $ str helpString
            ]
  in  [center ui]
  where editorField = E.renderEditor (str . unlines) True editorState
