{-# LANGUAGE OverloadedStrings #-}
module BrickApp.Utils.Style
  ( customBorder
  , custom2Attr
  , theMap
  , inputFieldAttr
  , inputDialogBorder
  , inputDialogBorderAttr
  , borderMappingsInputDialog
  , getTheme
  , borderMappings
  , errorAttr
  , inputDialogAttr
  , inputDialogLabelAttr
  , titleAttr
  , customAttr
  , asteriskAttr
  , numberPrefixAttr
  , fileAttr
  , indexSearchServerAttr
  , linkAttr
  , textAttr
  , directoryAttr
  , genericTypeAttr
  , buttonSelectedAttr
  ) where

import           Brick.Themes ( Theme
                              , newTheme
                              , themeToAttrMap
                              , loadCustomizations
                              )
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

import           Config.Theme ( getUserThemePath )

-- | In order to use ini-defined themes we need to have a default `Theme`,
-- as that's the way `Brick` works.
defaultTheme :: Theme
defaultTheme = newTheme (V.white `on` V.blue) listMapThingy

-- | Load the main theme..
getTheme :: IO A.AttrMap
getTheme = do
  userThemePath <- getUserThemePath
  perhapsTheme <- loadCustomizations userThemePath defaultTheme
  case perhapsTheme of
    Left errorMessage -> error errorMessage
    Right theme -> pure $ themeToAttrMap theme

menuLineAttr :: A.AttrName
menuLineAttr = menuAttr <> "line"

-- TODO: this all feels very messy
customAttr :: A.AttrName
customAttr = menuLineAttr <> "selected"

custom2Attr :: A.AttrName
custom2Attr = textAttr <> "selected"

-- | This style attribute is used for the menus in gopherspace.
-- The menus are just a specific kind of list.
menuAttr :: A.AttrName
menuAttr = "menu"

-- | This is the bit that describes the line's item type
itemDescAttr :: A.AttrName
itemDescAttr = menuLineAttr <> "itemDesc"

-- | This is used for the item type indicator in menus...
directoryAttr :: A.AttrName
directoryAttr = itemDescAttr <> "directory"

fileAttr :: A.AttrName
fileAttr = itemDescAttr <> "file"

indexSearchServerAttr :: A.AttrName
indexSearchServerAttr = itemDescAttr <> "indexSearchServer"

-- | A catchall, in other words "other."
genericTypeAttr :: A.AttrName
genericTypeAttr = itemDescAttr <> "generic"

numberPrefixAttr :: A.AttrName
numberPrefixAttr = menuAttr <> "numberPrefix"

linkAttr :: A.AttrName
linkAttr = menuLineAttr <> "linkString"

textAttr :: A.AttrName
textAttr = menuLineAttr <> "info"

-- TODO: bad name now...
asteriskAttr :: A.AttrName
asteriskAttr = menuAttr <> "asterisk"

titleAttr :: A.AttrName
titleAttr = "titleAttr"

errorAttr :: A.AttrName
errorAttr = "error"

inputFieldAttr :: A.AttrName
inputFieldAttr = "inputField"

popupAttr :: A.AttrName
popupAttr = "popup"

inputDialogLabelAttr :: A.AttrName
inputDialogLabelAttr = popupAttr <> "label"

-- I don't think this is being used FIXME
inputDialogAttr :: A.AttrName
inputDialogAttr = "inputDialogAttr"

buttonAttr :: A.AttrName
buttonAttr = "button"

buttonSelectedAttr :: A.AttrName
buttonSelectedAttr = buttonAttr <> "selected"

listMapThingy :: [(A.AttrName, V.Attr)]
listMapThingy =
  [ (L.listAttr, V.yellow `on` V.rgbColor (0 :: Int) (0 :: Int) (0 :: Int))
  -- The forecolor below effectively does *nothing*
  , ( L.listSelectedAttr
    , (V.defAttr `V.withStyle` V.bold) `V.withForeColor` V.white
    )
  , ( inputDialogAttr-- FIXME: unused?
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
  , (buttonAttr                        , (V.black `on`  V.yellow))
  , (buttonSelectedAttr                , (V.defAttr `V.withStyle` V.bold) `V.withForeColor` V.rgbColor (0 :: Int) (0 :: Int) (0 :: Int) `V.withBackColor` V.rgbColor (255 :: Int) (255 :: Int) (0 :: Int))
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

theMap :: A.AttrMap
theMap = A.attrMap
  V.defAttr listMapThingy

-- FIXME: better name and make configurable through INI
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

-- FIXME: make configurable through INI
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
