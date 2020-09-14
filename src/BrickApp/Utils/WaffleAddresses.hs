{-# LANGUAGE OverloadedStrings #-}

-- Support waffle:// addresses which simply point to various config and
-- information screens.
module BrickApp.Utils.WaffleAddresses where

import           Control.Monad.IO.Class
import           Data.Text as T
import qualified Data.Map as Map

import           BrickApp.ModeAction.Help
import           BrickApp.ModeAction.Open
import           BrickApp.ModeAction.Bookmarks
import qualified Brick.Main                    as B
import qualified Brick.Types                   as B

import           BrickApp.Types.Names
import           BrickApp.Types

-- Geets the proper mode initializer (this should be a hierarchical thign in my code) for the address
-- FIXME: pass gbs, e
-- NOTE: all of these will check doEventIfModes for making sure it can't open itself while its active.
-- The waffle address after the waffle:// scheme mapped to a certain init mode...?
waffleAddresses
  :: Map.Map T.Text (GopherBrowserState -> B.EventM AnyName (B.Next GopherBrowserState))
waffleAddresses =
  Map.fromList
    [ ("bookmarks", \gbs -> liftIO (initBookmarksMode gbs) >>= B.continue)
    , ("help",      \gbs -> liftIO (modifyGbsForHelp gbs) >>= B.continue)
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
  -> Maybe (B.EventM AnyName (B.Next GopherBrowserState))
waffleAddressEvent gbs address =
  let withoutWaffleScheme = dropWaffleScheme address
  in  case withoutWaffleScheme of
        Nothing      -> Nothing
        Just without -> case Map.lookup without waffleAddresses of
                          Just eventFunction -> Just $ eventFunction gbs
                          Nothing            -> Nothing
