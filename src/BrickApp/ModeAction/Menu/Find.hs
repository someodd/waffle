{-# LANGUAGE OverloadedStrings #-}

-- TODO: this doesn't even make sense here atm because there's no mode its just menu mode etc
-- | UI for editing the open config for setting commands associated with
-- opening a menu item of specific types.
module BrickApp.ModeAction.Menu.Find where

import qualified Data.Text                     as T

import           Brick.Widgets.List

import           BrickApp.Types
import           BrickApp.Utils                       ( getSearchEditorContents )
import           BrickApp.Types.Helpers

selectFirstFound :: GopherBrowserState -> GopherBrowserState
selectFirstFound gbs =
  if T.null $ inputValue
    then gbs
    else
      let Menu (gopherMenu, brickList, focusLines) = getMenu gbs
          newList = listFindBy (T.isInfixOf inputValue) brickList
      in  gbs { gbsBuffer = MenuBuffer $ Menu (gopherMenu, newList, focusLines) }
 where
  inputValue = getSearchEditorContents gbs
