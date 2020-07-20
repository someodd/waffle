{-# LANGUAGE OverloadedStrings #-}

-- | Drawing `MenuMode`
module UI.Draw.Menu where

import qualified Data.Text                     as T
import           Data.List                     as List
import qualified Data.Vector                   as Vector
import           Data.Maybe

import           Lens.Micro                     ( (^.) )
import qualified Brick.Widgets.List            as BrickList
import qualified Brick.Types                   as T
import           Brick.Widgets.Core             ( viewport
                                                , txt
                                                , withAttr
                                                , (<+>)
                                                )

import Gopher
import UI.Types
import UI.Types.Names
import UI.Types.Helpers
import UI.Utils.Style
import UI.Utils
import UI.ModeAction.Menu.State

menuModeUI :: GopherBrowserState -> [T.Widget AnyName]
menuModeUI gbs = defaultBrowserUI gbs
                                  (viewport (MyName MenuViewport) T.Horizontal)
                                  titleWidget
                                  mainWidget
                                  statusWidget
 where
  (Menu (_, l, _)) = getMenu gbs
  titleWidget =
    let (host, port, resource, _) = gbsLocation gbs
    in  txt
          $  " "
          <> host
          <> ":"
          <> T.pack (show port)
          <> if not $ T.null resource then " (" <> resource <> ") " else " "
  statusWidget =
    let cur = case l ^. BrickList.listSelectedL of
          Nothing -> txt "-"
          Just i  -> txt (T.pack $ show (i + 1))
        total =
            txt $ T.pack . show $ Vector.length $ l ^. BrickList.listElementsL
    in  txt "? for help. Menu mode. "
          <+> txt "Item "
          <+> cur
          <+> txt " of "
          <+> total

  mainWidget :: T.Widget AnyName
  mainWidget = BrickList.renderListWithIndex (listDrawElement $ getMenu gbs) True l

-- FIXME: this is messy! unoptimized!
listDrawElement
  :: Menu -> Int -> Bool -> T.Text -> T.Widget AnyName
listDrawElement menu indx sel a = cursorRegion <+> possibleNumber <+> withAttr
  lineColor
  (selStr a <+> lineDescriptorWidget (menuLine gmenu indx))
 where
  (Menu (gmenu, mlist, focusLines)) = menu
  maybeSelectedLine                 = selectedMenuLine menu

  -- FIXME: I should document what this does...
  selStr s
    | sel && isJust maybeSelectedLine && isInfoMsg
      (fromJust $ selectedMenuLine menu)
    = withAttr custom2Attr (txt s)
    | sel
    = withAttr customAttr $ txt s
    | otherwise
    = txt s

  cursorRegion = if sel then withAttr asteriskAttr $ txt " ➤ " else txt "   "
  isLink       = indx `elem` focusLines
  lineColor    = if isLink then linkAttr else textAttr
  biggestIndexDigits =
    length $ show (Vector.length $ mlist ^. BrickList.listElementsL)
  curIndexDigits = length $ show $ fromJust $ indx `elemIndex` focusLines

  possibleNumber = if isLink
    then
      withAttr numberPrefixAttr
      $  txt
      $  numberPad
      $  T.pack (show (fromJust $ indx `elemIndex` focusLines))
      <> ". "
    else txt $ T.replicate (biggestIndexDigits + 2) " "
   where
    numberPad :: T.Text -> T.Text
    numberPad = (T.replicate (biggestIndexDigits - curIndexDigits) " " <>)

  lineDescriptorWidget :: MenuLine -> T.Widget n
  lineDescriptorWidget line = case line of
    -- it's a parsed line
    (Parsed gl) -> case glType gl of
      -- Cannonical type
      (Canonical ct) -> case ct of
        Directory -> withAttr directoryAttr $ txt " 📂 [Directory]"
        File      -> withAttr fileAttr $ txt " 📄 [File]"
        IndexSearchServer ->
          withAttr indexSearchServerAttr $ txt " 🔎 [IndexSearchServer]"
        _ -> withAttr genericTypeAttr $ txt $ " [" <> T.pack (show ct) <> "]"
      -- Noncannonical type
      (NonCanonical nct) -> case nct of
        InformationalMessage -> txt $ T.replicate (biggestIndexDigits + 2) " "
        HtmlFile -> withAttr directoryAttr $ txt " 🌐 [HTMLFile] "
        _ -> withAttr genericTypeAttr $ txt $ " [" <> T.pack (show nct) <> "]"
    -- it's a malformed/unrecognized line
    (Unparseable _) -> txt ""
