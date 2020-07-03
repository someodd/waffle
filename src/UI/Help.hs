{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The help screen, which is also the homepage. It's just a TextFile, basically.
module UI.Help
  ( helpModeUI
  , helpEventHandler
  , modifyGbsForHelp
  ) where

-- NOTE: should look to how search is handled as non-location i think

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

import           Data.FileEmbed
import qualified Graphics.Vty                  as V
import qualified Brick.Types                   as T
import           Brick.Widgets.Core             ( viewport
                                                , txt
                                                )
import qualified Brick.Main                    as M

import           UI.Util
import           UI.TextFile
import           UI.Representation

helpModeUI :: GopherBrowserState -> [T.Widget AnyName]
helpModeUI gbs = defaultBrowserUI gbs (viewport (MyName TextViewport) T.Both) titleWidget mainWidget statusWidget
  where
   mainWidget   = txt $ getHelpTextFileContents gbs
   titleWidget  = txt "Waffle Help"
   statusWidget = txt "Help mode. Use arrows or hjkl to scroll."

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

helpText :: T.Text
helpText = TE.decodeUtf8 $(embedFile "data/help.txt")

-- | Initialize help mode.
modifyGbsForHelp :: GopherBrowserState -> IO GopherBrowserState
modifyGbsForHelp gbs = do
  pure gbs { gbsBuffer = HelpBuffer $ Help { hText = TextFile { tfContents = helpText, tfTitle = "Help: Using Waffle" }, hFormerGbs = gbs }, gbsRenderMode = HelpMode }
