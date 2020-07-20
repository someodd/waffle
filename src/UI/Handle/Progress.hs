-- | Handle events for `ProgressMode`
module UI.Handle.Progress where

import qualified Graphics.Vty                  as V
import qualified Brick.Main                    as M
import qualified Brick.Types                   as T

import UI.Types
import UI.Types.Names
import UI.ModeAction.Progress

-- FIXME: maybe this needs to just have generic B.BrickEvent MyName CustomEvent
-- and match from there
-- TODO: handleProgressEvents
-- FIXME: no need for this left/right nonsense because they're both
-- B.BrickEvent MyName CustomEvent and you can decipher from there like in UI...
-- should do this soon...
progressEventHandler
  :: GopherBrowserState
  -> Either (T.BrickEvent AnyName CustomEvent) V.Event
  -> T.EventM AnyName (T.Next GopherBrowserState)
progressEventHandler gbs (Left e)  = case e of
  -- This is extremely hacky!
  T.AppEvent (NewStateEvent gbs')       -> M.continue gbs'
  T.AppEvent (FinalNewStateEvent gbs')  -> modeTransition >> M.continue gbs'
  _                                     -> M.continue gbs
progressEventHandler gbs (Right _) = M.continue gbs
