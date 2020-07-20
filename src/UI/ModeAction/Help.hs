{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The actual doing-stuff for `HelpMode`.
module UI.ModeAction.Help where

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

import           Data.FileEmbed
import           Brick.Widgets.Core             ( txt )

import UI.Types

helpText :: T.Text
helpText = TE.decodeUtf8 $(embedFile "data/help.txt")

-- Would this be better as a helper function in UI.Types.Helpers?
-- | Initialize help mode.
modifyGbsForHelp :: GopherBrowserState -> IO GopherBrowserState
modifyGbsForHelp gbs = do
  pure gbs { gbsBuffer = HelpBuffer $ Help { hText = TextFile { tfContents = txt helpText, tfTitle = "Help: Using Waffle" }, hFormerGbs = gbs }, gbsRenderMode = HelpMode }
