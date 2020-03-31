{-# LANGUAGE OverloadedStrings #-}
module UI.Style where

import qualified Brick.Widgets.List            as L
import qualified Graphics.Vty                  as V
import qualified Brick.Widgets.FileBrowser     as FB
import qualified Brick.Widgets.Edit            as E
import qualified Brick.AttrMap                 as A
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Border          as B
import           Brick.Util                     ( fg
                                                , on
                                                )

-- TODO: this all feels very messy
customAttr :: A.AttrName
customAttr = "custom"

custom2Attr :: A.AttrName
custom2Attr = "custom2"

directoryAttr :: A.AttrName
directoryAttr = "directoryAttr"

fileAttr :: A.AttrName
fileAttr = "fileAttr"

indexSearchServerAttr :: A.AttrName
indexSearchServerAttr = "indexSearchServerAttr"

genericTypeAttr :: A.AttrName
genericTypeAttr = "genericTypeAttr"

numberPrefixAttr :: A.AttrName
numberPrefixAttr = "numberPrefixAttr"

linkAttr :: A.AttrName
linkAttr = "linkAttr"

textAttr :: A.AttrName
textAttr = "textAttr"

-- TODO: bad name now...
asteriskAttr :: A.AttrName
asteriskAttr = "asteriskAttr"

titleAttr :: A.AttrName
titleAttr = "titleAttr"

errorAttr :: A.AttrName
errorAttr = "error"

inputFieldAttr :: A.AttrName
inputFieldAttr = "inputField"

inputDialogLabelAttr :: A.AttrName
inputDialogLabelAttr = "inputDialogLabelAttr"

inputDialogAttr :: A.AttrName
inputDialogAttr = "inputDialogAttr"

theMap :: A.AttrMap
theMap = A.attrMap
  V.defAttr
  [ (L.listAttr, V.yellow `on` V.rgbColor (0 :: Int) (0 :: Int) (0 :: Int))
  , ( L.listSelectedAttr
    , (V.defAttr `V.withStyle` V.bold) `V.withForeColor` V.white
    )
  , ( inputDialogAttr
    , V.rgbColor (255 :: Int) (255 :: Int) (0 :: Int)
      `on` V.rgbColor (0 :: Int) (0 :: Int) (0 :: Int)
    )
  , (directoryAttr        , fg V.red)
  , (fileAttr             , fg V.cyan)
  , (indexSearchServerAttr, fg V.magenta)
  , (linkAttr, fg (V.rgbColor (28 :: Int) (152 :: Int) (255 :: Int)))
  , (textAttr, fg (V.rgbColor (255 :: Int) (255 :: Int) (0 :: Int)))
  , (genericTypeAttr      , fg V.green)
  , (numberPrefixAttr, fg (V.rgbColor (252 :: Int) (40 :: Int) (254 :: Int)))
  , (customAttr, (V.defAttr `V.withStyle` V.bold) `V.withForeColor` V.white)
  , (custom2Attr          , fg V.yellow)
  , ( titleAttr
    , (V.defAttr `V.withStyle` V.reverseVideo)
    `V.withStyle`     V.bold
    `V.withForeColor` V.white
    )
  , ( inputDialogLabelAttr
    , (V.defAttr `V.withStyle` V.reverseVideo)
    `V.withStyle`     V.bold
    `V.withForeColor` V.yellow
    )
  , (asteriskAttr                      , fg V.white)
  , (E.editAttr                        , V.white `on` V.brightBlack)
  , (E.editFocusedAttr                 , V.white `on` V.brightBlack)
  , (FB.fileBrowserCurrentDirectoryAttr, V.white `on` V.blue)
  , (FB.fileBrowserSelectionInfoAttr   , V.white `on` V.blue)
  , (FB.fileBrowserDirectoryAttr       , fg V.blue)
  , (FB.fileBrowserBlockDeviceAttr     , fg V.magenta)
  , (FB.fileBrowserCharacterDeviceAttr , fg V.green)
  , (FB.fileBrowserNamedPipeAttr       , fg V.yellow)
  , (FB.fileBrowserSymbolicLinkAttr    , fg V.cyan)
  , (FB.fileBrowserUnixSocketAttr      , fg V.red)
  , (FB.fileBrowserSelectedAttr        , V.white `on` V.magenta)
  , (errorAttr                         , fg V.red)
  ]

customBorder :: BS.BorderStyle
customBorder = BS.BorderStyle { BS.bsCornerTL      = '▚'
                              , BS.bsCornerTR      = '▚'
                              , BS.bsCornerBR      = '▚'
                              , BS.bsCornerBL      = '▚'
                              , BS.bsIntersectFull = ' '
                              , BS.bsIntersectL    = ' '
                              , BS.bsIntersectR    = ' '
                              , BS.bsIntersectT    = ' '
                              , BS.bsIntersectB    = ' '
                              , BS.bsHorizontal    = '▚'
                              , BS.bsVertical      = ' '
                              }

-- Round oval
inputDialogBorder :: BS.BorderStyle
inputDialogBorder = BS.BorderStyle { BS.bsCornerTL      = '░'
                                   , BS.bsCornerTR      = '░'
                                   , BS.bsCornerBR      = '░'
                                   , BS.bsCornerBL      = '░'
                                   , BS.bsIntersectFull = ' '
                                   , BS.bsIntersectL    = ' '
                                   , BS.bsIntersectR    = ' '
                                   , BS.bsIntersectT    = ' '
                                   , BS.bsIntersectB    = ' '
                                   , BS.bsHorizontal    = '▓'
                                   , BS.bsVertical      = '▒'
                                   }

-- emoji test
emojiBorder :: BS.BorderStyle
emojiBorder = BS.BorderStyle { BS.bsCornerTL      = '╋'
                             , BS.bsCornerTR      = '╋'
                             , BS.bsCornerBR      = '╋'
                             , BS.bsCornerBL      = '╋'
                             , BS.bsIntersectFull = ' '
                             , BS.bsIntersectL    = ' '
                             , BS.bsIntersectR    = ' '
                             , BS.bsIntersectT    = ' '
                             , BS.bsIntersectB    = ' '
                             , BS.bsHorizontal    = '┅'
                             , BS.bsVertical      = '┇'
                             }

inputDialogBorderAttr :: A.AttrName
inputDialogBorderAttr = "inputDialogBorderAttr"

borderMappingsInputDialog :: [(A.AttrName, V.Attr)]
borderMappingsInputDialog =
--  [ (inputDialogBorderAttr, V.red `on` V.rgbColor (55 :: Int) (175 :: Int) (200 :: Int)) ]
  [ ( B.borderAttr
    , V.yellow `on` V.rgbColor (55 :: Int) (175 :: Int) (200 :: Int)
    )
  ]

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
  [(B.borderAttr, V.cyan `on` V.rgbColor (0 :: Int) (0 :: Int) (0 :: Int))]
