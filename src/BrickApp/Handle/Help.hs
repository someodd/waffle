-- | Event handler for the help screen/`HelpMode`.
module BrickApp.Handle.Help where

import qualified Brick.Types                   as T
import qualified Graphics.Vty                  as V
import qualified Brick.Main                    as M

import BrickApp.Types
import BrickApp.Types.Names
import BrickApp.Types.Helpers
import BrickApp.Handle.TextFile

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
