{-# LANGUAGE OverloadedStrings #-}

-- | Draw `ProgressMode`
module BrickApp.Draw.Progress where

import qualified Data.Text                     as T

import           Brick.Widgets.Core             ( txt )
import qualified Brick.Types                   as T

import           BrickApp.Types.Names
import           BrickApp.Types.Helpers
import           BrickApp.Types

-- TODO: progressUI...
drawProgressUI :: GopherBrowserState -> [T.Widget AnyName]
drawProgressUI gbs = [a]
 where
  -- FIXME: "downloaded" isn't necessarily correct. You can request more bytes than is left...
  bytesDownloaded = T.pack $ show (pbBytesDownloaded (getProgress gbs))
  bytesMessage = "Downloaded bytes: " <> bytesDownloaded
  downloadingWhat = pbMessage (getProgress gbs)

  connectMessage :: T.Text
  connectMessage
    | pbIsFromCache (getProgress gbs) = "⏳ Loading from cache..."
    | pbConnected (getProgress gbs)   = bytesMessage
    | otherwise                       = "⏳ Connecting..."

  a = txt $ downloadingWhat <> "\n" <> connectMessage


