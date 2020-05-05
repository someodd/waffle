module GopherNet
  ( writeAllBytes'
  ) where

import           Data.Maybe
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.UTF8          as U8

import           Network.Simple.TCP

import           UI.Representation

-- MAYBE TELL TYPE TOO!
-- use mimetype function USING this
-- TODO: add docstring. needs to be both Just or both Nothing! will error otherwise!
writeAllBytes'
  :: Maybe (a -> Maybe U8.ByteString -> IO a)
  -> Maybe a
  -> Socket
  -> String
  -> IO ()
writeAllBytes' stateMutator someState connectionSocket tempFilePath = do
  gosh <- recv connectionSocket recvChunkSize
  newState <- case stateMutator of
    (Just sm) -> Just <$> sm (fromJust someState) gosh-- FIXME: bad to use fromJust
    Nothing   -> pure Nothing
  case gosh of
    Nothing -> pure ()
    -- Doesn't set to started in status TODO FIXME
    Just chnk ->
      ByteString.appendFile tempFilePath chnk
        >> writeAllBytes' stateMutator newState connectionSocket tempFilePath
  where
   recvChunkSize = 1024

-- Can first guess by selector if it begins with /1/ or whatever and that'll be item type otherwise guess from mime on file system or file extension?
-- Tell type for GOTO via fileextension OR reading file
-- 1. selector prefix like /1/ for gophermap. file extension maybe. otherwise guess by reading file.
-- Useful for Goto and other
{-
writeTellItemType
  :: Selector
  -> Maybe (a -> Maybe U8.ByteString -> IO a)
  -> Maybe a
  -> Socket
  -> String
  -> (Either GopherNonCanonicalItemType GopherCanonicalItemType, U8.ByteString)
writeTellItemType selector stateMutator someState connectionSocket tempFilePath = do
  writeallBytes' stateMutator someState connectionSocket tempFilePath
-}
