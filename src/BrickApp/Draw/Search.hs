{-# LANGUAGE OverloadedStrings #-}

module BrickApp.Draw.Search where

import qualified Brick.Types                   as T

import BrickApp.Types
import BrickApp.Types.Names
import BrickApp.Types.Helpers
import BrickApp.Utils.Popup

-- | Draw the search prompt. Used by UI.drawUI if the gbsRenderMode
-- is SearchMode.
searchInputUI :: GopherBrowserState -> [T.Widget AnyName]
searchInputUI gbs = inputPopupUI editorBuffer labelText helpText
 where
  searchBuffer = getSearch gbs
  editorBuffer = sbEditorState (getSearch gbs)
  labelText    = "Search: " <> sbHost searchBuffer
  helpText     = "Press ENTER to search"
