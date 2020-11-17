-- FIXME: maybe a lot of this could go into modeaction?
-- FIXME TODO: rename to DialogPopup?
-- | A single-line input dialog box. Being phased out with a Dialog version
-- from `brick`.
module BrickApp.Utils.Popup
  ( popOver
  , popup
  , inputPopupUI
  , popupDialogEventHandler
  ) where

import           Control.Monad.IO.Class
import qualified Data.Map                      as Map
import qualified Data.Text                     as T

import qualified Brick.Main                    as B
import qualified Brick.Types                   as B
import qualified Graphics.Vty                  as V
import qualified Brick.Widgets.Dialog          as B
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
import           BrickApp.Types
import           BrickApp.Types.Helpers

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

-- | The Brick event handler used when a popup dialog is present.
popupDialogEventHandler
  :: GopherBrowserState
  -> Popup
  -> B.BrickEvent AnyName CustomEvent
  -> T.EventM AnyName (T.Next GopherBrowserState)
popupDialogEventHandler gbs n e =
  case e of
    B.VtyEvent (V.EvKey V.KEsc []) -> B.continue $ closePopup gbs
    B.VtyEvent (V.EvKey V.KEnter []) -> B.continue =<< liftIO (doChoice (B.dialogSelection (pDialogWidget n)) gbs)
    B.VtyEvent vtyev@(_) -> B.continue =<< (updatePopupWidget gbs <$> B.handleDialogEvent vtyev (pDialogWidget n))
    _ -> B.continue gbs
 where
  updatePopupWidget gbs' dialog = gbs' { gbsPopup = Just (n { pDialogWidget = dialog } ) }
  doChoice Nothing gbs' = pure gbs'
  doChoice (Just choice) gbs' =
    let (Just popup') = gbsPopup gbs
        (Just choiceFunc) = Map.lookup (show choice) (pDialogMap popup')
    in  choiceFunc gbs'
