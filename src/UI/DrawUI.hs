{-# LANGUAGE OverloadedStrings #-}
module UI.DrawUI where

import UI.StyleUI
import UI.StateUI
import GopherClient

import qualified Data.Vector as Vec
import Data.Maybe
import Data.List as Lis
import qualified Data.Text as Text
import qualified Control.Exception as E

import Lens.Micro ((^.))
import Brick.Widgets.Core (withDefAttr, emptyWidget, viewport, hLimitPercent, vLimitPercent, updateAttrMap, withBorderStyle, str, vBox, vLimit, withAttr, (<+>), (<=>), txt, padTop)
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import qualified Brick.Widgets.List as L

-- FIXME: this is messy! unoptimized!
listDrawElement :: GopherBrowserState -> Int -> Bool -> String -> Widget MyName
listDrawElement gbs indx sel a =
  cursorRegion <+> possibleNumber <+> withAttr lineColor (lineDescriptorWidget (menuLine (gmenu) indx) <+> selStr a)
  where
    selStr s
      | sel && isInfoMsg (selectedMenuLine gbs) = withAttr custom2Attr (str s)
      | sel = withAttr customAttr $ str s
      | otherwise = str s

    (MenuBuffer (gmenu, mlist, focusLines)) = gbsBuffer gbs

    cursorRegion = if sel then withAttr asteriskAttr $ str " ➤ " else str "   "
    isLink = indx `elem` focusLines
    lineColor = if isLink then linkAttr else textAttr
    biggestIndexDigits = length $ show (Vec.length $ mlist^.L.listElementsL)
    curIndexDigits = length $ show $ fromJust $ indx `elemIndex` focusLines

    possibleNumber = if isLink then withAttr numberPrefixAttr $ str $ numberPad $ show (fromJust $ indx `elemIndex` focusLines) ++ ". " else str "" 
      where
      numberPad = (replicate (biggestIndexDigits - curIndexDigits) ' ' ++)

    lineDescriptorWidget :: Either GopherLine MalformedGopherLine -> Widget n
    lineDescriptorWidget line = case line of
      -- it's a gopherline
      (Left gl) -> case glType gl of
        -- Cannonical type
        (Left ct) -> case ct of
          Directory -> withAttr directoryAttr $ str "📂 [Directory] "
          File -> withAttr fileAttr $ str "📄 [File] "
          IndexSearchServer -> withAttr indexSearchServerAttr $ str "🔎 [IndexSearchServer] "
          _ -> withAttr genericTypeAttr $ str $ "[" ++ show ct ++ "] "
        -- Noncannonical type
        (Right nct) -> case nct of
          InformationalMessage -> str $ replicate (biggestIndexDigits+2) ' '
          HtmlFile -> withAttr directoryAttr $ str "🌐 [HTMLFile] "
          _ -> withAttr genericTypeAttr $ str $ "[" ++ show nct ++ "] "
      -- it's a malformed line
      (Right _) -> str ""



-- | The UI for rendering and viewing a text file.
textFileModeUI :: GopherBrowserState -> [Widget MyName]
textFileModeUI gbs =
  let (TextFileBuffer tfb) = gbsBuffer gbs
      ui = viewport MyViewport T.Both $ vBox [str $ clean tfb]
  in [C.center $ B.border $ hLimitPercent 100 $ vLimitPercent 100 ui]

-- | The UI for searching
searchModeUI :: GopherBrowserState -> [Widget MyName]
searchModeUI gbs =
  let sb = gbsBuffer gbs
      ui = viewport MyViewport T.Both $ vBox [str $ sbQuery sb]
  in [C.center $ B.border $ hLimitPercent 100 $ vLimitPercent 100 ui]

-- | The UI for rendering and viewing a menu.
menuModeUI :: GopherBrowserState -> [Widget MyName]
menuModeUI gbs = [C.hCenter $ C.vCenter view]
  where
    (MenuBuffer (_, l, _)) = gbsBuffer gbs
    label = str " Item " <+> cur <+> str " of " <+> total -- TODO: should be renamed
    (host, port, resource, _) = gbsLocation gbs
    title = " " ++ host ++ ":" ++ show port ++ if not $ Lis.null resource then " (" ++ resource ++ ") " else " "
    cur = case l^.L.listSelectedL of
      Nothing -> str "-"
      Just i  -> str (show (i + 1))
    total = str $ show $ Vec.length $ l^.L.listElementsL
    box = updateAttrMap (A.applyAttrMappings borderMappings) $ withBorderStyle customBorder $ B.borderWithLabel (withAttr titleAttr $ str title) $ L.renderListWithIndex (listDrawElement gbs) True l
    view = vBox [ box, vLimit 1 $ str "Esc to exit. Vi keys to browse. Enter to open." <+> label]

-- FIXME
fileBrowserUi :: GopherBrowserState -> [Widget MyName]
fileBrowserUi gbs = [C.center $ vLimitPercent 100 $ hLimitPercent 100 $ ui <=> help]
    where
        b = fromBuffer $ gbsBuffer gbs
        fromBuffer x = fbFileBrowser x
        ui = C.hCenter $
             B.borderWithLabel (txt "Choose a file") $
             FB.renderFileBrowser True b
        help = padTop (T.Pad 1) $
               vBox [ case FB.fileBrowserException b of
                          Nothing -> emptyWidget
                          Just e -> C.hCenter $ withDefAttr errorAttr $
                                    txt $ Text.pack $ E.displayException e
                    , C.hCenter $ txt "Up/Down: select"
                    , C.hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
                    , C.hCenter $ txt "Enter: change directory or select file"
                    , C.hCenter $ txt "Esc: quit"
                    , C.hCenter $ str $ fbFileOutPath (gbsBuffer gbs)
                    ]

-- | The draw handler which will choose a UI based on the browser's mode.
drawUI :: GopherBrowserState -> [Widget MyName]
drawUI gbs
  | renderMode == MenuMode = menuModeUI gbs
  | renderMode == TextFileMode = textFileModeUI gbs
  | renderMode == FileBrowserMode = fileBrowserUi gbs
  | renderMode == SearchMode = searchModeUI gbs
  | otherwise = error "Cannot draw the UI for this unknown mode!"
  where renderMode = gbsRenderMode gbs


