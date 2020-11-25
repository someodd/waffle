{-# LANGUAGE OverloadedStrings #-}

module BrickApp.ModeAction.Menu where

import qualified Data.Map as Map
import           Data.List                     as List
import           Data.Maybe

import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.List            as BrickList
import           Brick.Widgets.Core             ( txt )

import BrickApp.ModeAction.Menu.State
import BrickApp.Types
import BrickApp.Types.Helpers
import BrickApp.ModeAction.Progress
import Gopher

-- | Used by `jumpNextLink` and `jumpPrevLink` for creating a new
-- menu that uses the updated list position.
updateMenuPosition :: Menu -> Int -> Menu
updateMenuPosition menu next =
  let (Menu (gm, l, fl)) = menu in Menu (gm, BrickList.listMoveTo next l, fl)

-- | Jump to the next line (wraps around).
nextLine :: Menu -> Menu
nextLine menu = updateMenuPosition menu next
 where
  (Menu (_, l, _)) = menu

  next = case BrickList.listSelected l of
    Just currentIndex ->
      if currentIndex == length l - 1
        then 0
        else currentIndex + 1
    Nothing           -> 0

-- | Jump to the previous line (wraps around).
previousLine :: Menu -> Menu
previousLine menu = updateMenuPosition menu next
 where
  (Menu (_, l, _)) = menu

  next = case BrickList.listSelected l of
    Just currentIndex ->
      if currentIndex == 0
        then length l - 1
        else currentIndex - 1
    Nothing           -> 0

-- FIXME: move away form getMenu gbs and using gbs
-- | Jump to the next link (wraps around). Basically, skips info items.
jumpNextLink :: Menu -> Menu
jumpNextLink menu = updateMenuPosition menu next
 where
  (Menu (_, l, focusLines)) = menu

  headOr a []      = a
  headOr _ (x : _) = x

  next = case BrickList.listSelected l of
    -- NOTE: using "find" for this feels inefficient... oh well!
    Just currentIndex ->
      -- Try to find a line # bigger than the currently selected line in
      -- the focusLines to give us the new/next line to jump to.
      --
      -- If we cannot find a line # bigger than the currently selected line
      -- we wrap to the first link. However, if there is no "first link,"
      -- something that would happen if there's no elements in focusLines,
      -- we just return the active line.
                         fromMaybe (headOr currentIndex focusLines)
                                   (find (> currentIndex) focusLines)
    -- If there's no currently selected line let's select line 0!
    Nothing -> headOr 0 focusLines

-- | Jump to previous link (wraps around). Basically, skips info items.
-- Be sure to see `jumpNextLink` (most of my code comments are in there).
jumpPrevLink :: Menu -> Menu
jumpPrevLink menu = updateMenuPosition menu next
 where
  (Menu (_, l, focusLines)) = menu

  lastOr a [] = a
  lastOr _ xs = last xs

  next = case BrickList.listSelected l of
    Just currentIndex -> fromMaybe
      (lastOr currentIndex focusLines)
      (find (< currentIndex) $ reverse focusLines)
    Nothing -> lastOr 0 focusLines

-- | Make a request based on the currently selected Gopher menu item and open
-- the file!
newStateFromOpenItem :: GopherBrowserState -> IO GopherBrowserState
newStateFromOpenItem gbs =
  initOpenMode gbs (host, port, resource, FileBrowserMode, Nothing) lineType -- render mode not needed
 where
  menu                             = getMenu gbs
  (host, port, resource, lineType) = case selectedMenuLine menu of
    -- ParsedLine
    Just (Parsed      gl) -> (glHost gl, glPort gl, glSelector gl, glType gl)
    -- FIXME: why even error here?
    -- Unrecognized/unparseable line
    Just (Unparseable _ ) -> error "Can't do anything with unrecognized line."
    Nothing               -> error "Nothing is selected!"

-- | Describe the currently selected line in the menu/map.
lineInfoPopup :: GopherBrowserState -> GopherBrowserState
lineInfoPopup gbs =
  let menu            = getMenu gbs
      currentLineInfo = case selectedMenuLine menu of
        Just gopherLine -> explainLine gopherLine
        Nothing         -> "Nothing is selected!"
  in  gbs
        { gbsPopup =
          Just $ Popup
            { pDialogWidget = D.dialog (Just "Currently Selected Line Info") (Just (0, [ ("Ok", Ok) ])) 50--wtf what about max width for bug
            , pDialogMap = Map.fromList [("Ok", pure . closePopup)]
            , pDialogBody = txt currentLineInfo
            }
        }
