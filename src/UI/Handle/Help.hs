-- | Event handler for the help screen/`HelpMode`.
module UI.Handle.Help where

import qualified Brick.Types                   as T
import qualified Graphics.Vty                  as V
import qualified Brick.Main                    as M

import UI.Types
import UI.Types.Names
import UI.Types.Helpers
import UI.Handle.TextFile

-- | Basic text file controls, modularized so that the Help screen can use
-- too, without including the history stuff. See the Help module.
helpEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM AnyName (T.Next GopherBrowserState)
helpEventHandler gbs e = case e of
  -- What about left and right?!
  V.EvKey V.KEsc        [] -> M.continue $ hFormerGbs $ getHelp gbs
  _                        -> basicTextFileEventHandler gbs e
