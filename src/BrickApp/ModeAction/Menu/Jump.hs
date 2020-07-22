-- | Does the heavy-lifting, the actual stuff for `MenuJumpMode`.
module BrickApp.ModeAction.Menu.Jump where

import qualified Data.Text                     as T -- FIXME

import           Brick.Widgets.List             ( listMoveTo )
import           Brick.Widgets.Edit            as E

import BrickApp.Types

-- REDUNDANT CODE/DUPLICATE
-- | Used by `jumpNextLink` and `jumpPrevLink` for creating a new
-- menu that uses the updated list position.
updateMenuPosition :: Menu -> Int -> Menu
updateMenuPosition menu next =
  let (Menu (gm, l, fl)) = menu in Menu (gm, listMoveTo next l, fl)

jumpToLink :: Menu -> Int -> Menu
jumpToLink menu linkNumber = updateMenuPosition menu jumpToIndex
 where
  jumpToIndex =
    let (Menu (_, _, focusLines)) = menu
        ind                       = min linkNumber $ length focusLines - 1
    in  focusLines !! ind

-- TODO: getStatusEditorText
-- FIXME: fromJust is very bad!
-- this should be a generic util function or something in repr just for status editor

getEditText :: Editor T.Text n -> T.Text
getEditText x = T.filter (/= '\n') $ T.unlines (E.getEditContents x)
