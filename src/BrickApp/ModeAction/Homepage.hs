{-# LANGUAGE OverloadedStrings #-}
-- | The controller stuff for the homepage feature.
module BrickApp.ModeAction.Homepage where

import qualified Data.Text                     as T
import qualified Data.Map                      as Map

import Brick.Widgets.Core (txt)
import qualified Brick.Widgets.Dialog          as D

import BrickApp.Utils
import BrickApp.Types
import BrickApp.Types.Helpers
import           Config
import Config.Homepage
import           BrickApp.Utils.WaffleAddresses


-- | Function for setting current location as the homepage
setHomeDialog :: GopherBrowserState -> IO GopherBrowserState
setHomeDialog gbs =
  let (domain, port, resource, renderMode, displayString) = gbsLocation gbs
      itemType = T.pack [renderModeToItemChar renderMode]
      uri = T.unpack $ "gopher://" <> domain <> ":" <> (T.pack $ show port) <> "/" <> itemType <> resource
      -- FIXME: maybe should have a helper function since this gets repeated so dang much!
      -- this new gbs below will replace the current home dialog with a success dialog
      choices = [ ("Ok", Ok) ]
      pop = Popup
              { pDialogWidget = D.dialog (Just "Homepage Set!") (Just (0, choices)) 50--wtf what about max width for bug
              , pDialogMap = Map.fromList [("Ok", pure . closePopup)]
              , pDialogBody = txt "Success: Current page set as homepage!"
              }
      newGbs = gbs { gbsPopup = Just pop }
  in  setHomepage uri (fmap T.unpack displayString) >> pure newGbs

-- | The dialog for OK/cancel setting homepage to current
createHomeDialog :: GopherBrowserState -> GopherBrowserState
createHomeDialog gbs =
  let choices = [ ("Ok", Ok), ("Cancel", Cancel) ]
      pop = Popup
              { pDialogWidget = D.dialog (Just "Set Homepage?") (Just (0, choices)) 50--wtf what about max width for bug
              , pDialogMap = Map.fromList [("Ok", setHomeDialog), ("Cancel", pure . closePopup)]
              , pDialogBody = txt "Set current page as homepage?"
              }
  in gbs { gbsPopup = Just pop }

goHome :: GopherBrowserState -> IO GopherBrowserState
goHome gbs = do
  configParser <- getUserHomepageConfig
  unparsedURI <- readConfigParserValue configParser "homepage" "uri"
  displayString <- readConfigParserValue configParser "homepage" "display" -- What happens if this is blank?
  loadAddress gbs (T.pack $ unparsedURI) (Just displayString)
