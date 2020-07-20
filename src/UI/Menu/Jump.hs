{-# LANGUAGE OverloadedStrings #-}

-- FIXME: belongs to src/UI/Menu/Jump.hs
-- Menu jump mode.
-- | Jump to link # in a menu.

module UI.Menu.Jump
  ( jumpEventHandler
  , initJumpMode
  ) where

import           Data.Char                      ( isDigit)
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text                     as T

import qualified Brick.Widgets.List            as BrickList
import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Graphics.Vty.Input.Events      ( Event )
import qualified Graphics.Vty                  as V
import           Brick.Widgets.Edit            as E

import UI.Menu.State
import UI.Types
import UI.Types.Names
import UI.Types.Helpers

-- REDUNDANT CODE/DUPLICATE
-- | Used by `jumpNextLink` and `jumpPrevLink` for creating a new
-- menu that uses the updated list position.
updateMenuPosition :: Menu -> Int -> Menu
updateMenuPosition menu next =
  let (Menu (gm, l, fl)) = menu in Menu (gm, BrickList.listMoveTo next l, fl)

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

-- WHAT WHY
statusEditorUpdateHandler :: GopherBrowserState -> Event -> T.EventM AnyName GopherBrowserState
statusEditorUpdateHandler gbs ev =
  -- Maybe functor is so handy! defaults to Nothing if Nothing!
  let updateEditorInStatus x
        | T.length (getEditText x) > 0 && T.all isDigit (getEditText x) = newMenuBuffer (gbs { gbsStatus = (\f -> f { seEditorState = x }) <$> gbsStatus gbs }) $ jumpToLink (getMenu gbs) (read (T.unpack $ getEditText x) :: Int)
        | otherwise      = formerMode gbs
  in  updateEditorInStatus
        <$> E.handleEditorEvent ev (seEditorState $ fromJust $ gbsStatus gbs)

-- Think really hard about why this is TODO
initJumpMode :: GopherBrowserState -> Int -> T.EventM AnyName (T.Next GopherBrowserState)
initJumpMode gbs i =
  let newGbs = gbs {
      gbsRenderMode = MenuJumpMode
    , gbsStatus = Just $ StatusEditor { seLabel = "Jump to link #: "
                                      , seEditorState = E.editor (MyName EditorViewport) Nothing (T.pack $ show i)
                                      , seFormerMode = gbsRenderMode gbs
                                      }
    }
  -- WHY?
  in M.continue =<< statusEditorUpdateHandler newGbs (V.EvKey V.KRight [])

-- This should be more generic for status... could even have UI/Representations/Status.hs
formerMode :: GopherBrowserState -> GopherBrowserState
formerMode g = g { gbsRenderMode = seFormerMode $ fromJust $ gbsStatus g, gbsStatus = Nothing }

jumpEventHandler
  :: GopherBrowserState -> Event -> T.EventM AnyName (T.Next GopherBrowserState)
jumpEventHandler gbs e = case e of
    -- FIXME: esc quits! Change key...
  V.EvKey V.KEsc   [] -> M.continue $ formerMode gbs
  -- If enter key then follow...
  V.EvKey V.KEnter [] -> liftIO (newStateFromSelectedMenuItem $ formerMode gbs) >>= M.continue
  -- V.EvKey V.KEnter [] -> liftIO (mkGotoResponseState gbs) >>= M.continue
  _                   -> M.continue =<< statusEditorUpdateHandler gbs e
