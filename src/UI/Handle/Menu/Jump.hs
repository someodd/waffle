{-# LANGUAGE OverloadedStrings #-}

-- | Handle events for `MenuMode`'s jump feature
module UI.Handle.Menu.Jump where

import           Data.Char                      ( isDigit)
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text                     as T

import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Graphics.Vty.Input.Events      ( Event )
import qualified Graphics.Vty                  as V
import           Brick.Widgets.Edit            as E

import UI.Types
import UI.Types.Names
import UI.Types.Helpers
import UI.ModeAction.Menu.Jump
import UI.ModeAction.Menu.State

-- WHAT WHY
statusEditorUpdateHandler :: GopherBrowserState -> Event -> T.EventM AnyName GopherBrowserState
statusEditorUpdateHandler gbs ev =
  -- Maybe functor is so handy! defaults to Nothing if Nothing!
  let updateEditorInStatus x
        | T.length (getEditText x) > 0 && T.all isDigit (getEditText x) = newMenuBuffer (gbs { gbsStatus = (\f -> f { seEditorState = x }) <$> gbsStatus gbs }) $ jumpToLink (getMenu gbs) (read (T.unpack $ getEditText x) :: Int)
        | otherwise      = formerMode gbs
  in  updateEditorInStatus
        <$> E.handleEditorEvent ev (seEditorState $ fromJust $ gbsStatus gbs)

jumpEventHandler
  :: GopherBrowserState -> Event -> T.EventM AnyName (T.Next GopherBrowserState)
jumpEventHandler gbs e = case e of
    -- FIXME: esc quits! Change key...
  V.EvKey V.KEsc   [] -> M.continue $ formerMode gbs
  -- If enter key then follow...
  V.EvKey V.KEnter [] -> liftIO (newStateFromSelectedMenuItem $ formerMode gbs) >>= M.continue
  -- V.EvKey V.KEnter [] -> liftIO (mkGotoResponseState gbs) >>= M.continue
  _                   -> M.continue =<< statusEditorUpdateHandler gbs e


-- This should be more generic for status... could even have UI/Representations/Status.hs
formerMode :: GopherBrowserState -> GopherBrowserState
formerMode g = g { gbsRenderMode = seFormerMode $ fromJust $ gbsStatus g, gbsStatus = Nothing }

-- TODO: should this go to UI.Handle?
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
