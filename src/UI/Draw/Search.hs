{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Search where

import qualified Brick.Types                   as T

import UI.Types
import UI.Types.Names
import UI.Types.Helpers
import UI.Utils.Popup

-- | Draw the search prompt. Used by UI.drawUI if the gbsRenderMode
-- is SearchMode.
searchInputUI :: GopherBrowserState -> [T.Widget AnyName]
searchInputUI gbs = inputPopupUI editorBuffer labelText helpText
 where
  searchBuffer = getSearch gbs
  editorBuffer = sbEditorState (getSearch gbs)
  labelText    = "Search: " <> sbHost searchBuffer
  helpText     = "Press ENTER to search"
