{-# LANGUAGE OverloadedStrings #-}

-- Support waffle:// addresses which simply point to various config and
-- information screens.
module BrickApp.Utils.WaffleAddresses where

import           Data.Text as T
import qualified Data.Map as Map

import           Brick.Widgets.Core             ( txt )
import           Network.URI

import           BrickApp.ModeAction.Help
import           BrickApp.ModeAction.Open
import           BrickApp.ModeAction.Bookmarks
import           BrickApp.Types
import           BrickApp.Utils
import           BrickApp.ModeAction.Progress
import           Gopher


-- Geets the proper mode initializer (this should be a hierarchical thign in my code) for the address
-- FIXME: pass gbs, e
-- NOTE: all of these will check doEventIfModes for making sure it can't open itself while its active.
-- The waffle address after the waffle:// scheme mapped to a certain init mode...?
waffleAddresses
  :: Map.Map T.Text (GopherBrowserState -> IO GopherBrowserState)
waffleAddresses =
  Map.fromList
    [ ("bookmarks", \gbs -> initBookmarksMode gbs)
    , ("help",      \gbs -> modifyGbsForHelp gbs)
    , ("assocs",    \gbs -> initConfigOpenMode gbs)
    --, ("homeconf",  setHomeConfig)
    ]

-- | Drops waffle:// from supplied address, but give back Nothing
-- if (a) the result of doing so results in a blank `Text` or
-- (b) the `Text` does not start with `waffle://`.
dropWaffleScheme :: T.Text -> Maybe T.Text
dropWaffleScheme address =
  if "waffle://" `T.isPrefixOf` address
    then case T.drop (T.length "waffle://") address of
           ""                  -> Nothing
           withoutWaffleScheme -> Just withoutWaffleScheme
    else Nothing

-- | Will produce an appropriate event for the Waffle address supplied, or Nothing
-- if it is not an actual waffle address.
waffleAddressEvent
  :: GopherBrowserState
  -> T.Text
  -> Maybe (IO GopherBrowserState)
waffleAddressEvent gbs address =
  let withoutWaffleScheme = dropWaffleScheme address
  in  case withoutWaffleScheme of
        Nothing      -> Nothing
        Just without -> case Map.lookup without waffleAddresses of
                          Just eventFunction -> Just $ eventFunction gbs
                          Nothing            -> Nothing

-- | Either load the appropriate waffle:// mode from address, or load an actual
-- URI/destination in gopherspace as specified by supplied address.
loadAddress :: GopherBrowserState -> T.Text -> IO GopherBrowserState
loadAddress gbs unparsedURI =
  case waffleAddressEvent gbs unparsedURI of
    -- It was a valid waffle:// address
    Just event -> event
    -- It was not a valid waffle:// address and is (hopefully) some valid gopher:// address
    Nothing    -> either (errorPopup gbs unparsedURI) (initProgressMode gbs Nothing) (tryLocationOrFail unparsedURI)
 where
  errorPopup :: GopherBrowserState -> T.Text -> T.Text -> IO GopherBrowserState
  errorPopup gbs' someBadURI message =
    let pop = Popup
                { pLabel   = "Goto input error!"
                , pWidgets = [txt message]
                , pHelp    = "Invalid:" <> someBadURI
                }
    in  pure $ gbs' { gbsPopup = Just pop }

  -- | Try to parse a `Location` from `Text` (which is hopefully
  -- some kind of valid URI), or give back an error message.
  tryLocationOrFail :: T.Text -> Either T.Text (T.Text, Int, T.Text, RenderMode, Maybe T.Text)
  tryLocationOrFail potentialURI = do
    parsedURI <- case (parseURI . T.unpack $ prefixSchemeIfMissing potentialURI) of
      Just uri -> Right uri
      Nothing  -> Left "Cannot even begin to parse supplied URI!"
    authority' <- case uriAuthority parsedURI of
      Just auth -> Right auth
      Nothing   -> Left $ "Invalid URI (no authority)."
    regName    <- case uriRegName authority' of
      ""      -> Left "Invalid URI (no regname/host)."
      rn      -> Right rn
    port <- case uriPort authority' of
      ""     -> Right 70
      ':':p  -> Right (read p :: Int)
      _      -> Left $ "Invalid URI (bad port)." -- I don' think this ever can occur with Network.URI...
    let resource = uriPath parsedURI
    Right (T.pack regName, port, removeGopherType $ T.pack resource, selectorToRenderMode $ T.pack resource, Nothing)

  prefixSchemeIfMissing :: T.Text -> T.Text
  prefixSchemeIfMissing potentialURI
    | "gopher://" `T.isPrefixOf` potentialURI = potentialURI
    | otherwise                               = "gopher://" <> potentialURI

