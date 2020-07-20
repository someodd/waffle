{-# LANGUAGE OverloadedStrings #-}

-- | Draw `ProgressMode`
module UI.Draw.Progress where

import qualified Data.Text                     as T

import           Brick.Widgets.Core             ( txt )
import qualified Brick.Types                   as T

import           UI.Types.Names
import           UI.Types.Helpers
import           UI.Types

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


