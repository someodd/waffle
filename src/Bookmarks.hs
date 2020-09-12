{-# LANGUAGE OverloadedStrings #-}

module Bookmarks ( bookmarksMenuText ) where

import qualified Data.Text                     as T
import qualified Data.Map                      as Map
import           Data.List                      ( intercalate )

import qualified Data.ConfigFile               as CF
import qualified Data.ConfigFile.Types         as CFT

import           Config.Bookmarks

-- | Converts the user's bookmarks.ini into a Gophermenu `Text` which can be parsed
-- into an actual `Menu`. This is to make it so the user's bookmarks can be represented
-- as a Gopher menu.
bookmarksMenuText :: IO T.Text
bookmarksMenuText = do
  cp <- getUserBookmarks
  -- It is noted in Data.ConfigFile to not do this
  let sectionOptionsList = filter (\x -> fst x /= "DEFAULT") $ Map.toList $ CF.content cp -- this skips DEFAULT
      menuLines          = map entryToMenuItemText sectionOptionsList
  pure $ T.intercalate "\n" menuLines
 where
  -- FIXME: do left/right error thing
  entryToMenuItemText :: (CF.SectionSpec, CFT.CPOptions) -> T.Text
  entryToMenuItemText (sectionString, options) =
    let label      = sectionString
        host       =
          case Map.lookup "host" options of
            Just h -> h
            Nothing -> error "Parse bookmark error: no host"
        resource   =
          case Map.lookup "resource" options of
            Just r -> r
            Nothing -> error "Parse bookmark error: no resource"
        gophertype =
          case Map.lookup "type" options of
            Just gt -> gt
            Nothing -> error "Parse bookmark error: no gophertype"
        port       =
          case Map.lookup "port" options of
            Just p -> p
            Nothing -> error "Parse bookmark error: no port!"
    in T.pack $ intercalate "\t" [gophertype ++ label, resource, host, port]
