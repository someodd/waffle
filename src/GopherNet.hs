-- TODO: use FileSig. maybe chain telling from selector and then file ext and as last resort magic string?
module GopherNet
  ( writeAllBytes
  ) where

import           Data.Maybe
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.UTF8          as U8

import           Network.Simple.TCP

-- MAYBE TELL TYPE TOO!
-- use mimetype function USING this
-- TODO: add docstring. needs to be both Just or both Nothing! will error otherwise!
writeAllBytes
  :: Maybe (a -> Maybe U8.ByteString -> IO a)
  -> Maybe a
  -> Socket
  -> String
  -> IO ()
writeAllBytes stateMutator someState connectionSocket tempFilePath = do
  gosh <- recv connectionSocket recvChunkSize
  newState <- case stateMutator of
    (Just sm) -> Just <$> sm (fromJust someState) gosh-- FIXME: bad to use fromJust
    Nothing   -> pure Nothing
  case gosh of
    Nothing -> pure ()
    -- Doesn't set to started in status TODO FIXME
    Just chnk ->
      ByteString.appendFile tempFilePath chnk
        >> writeAllBytes stateMutator newState connectionSocket tempFilePath
  where
   recvChunkSize = 1024
