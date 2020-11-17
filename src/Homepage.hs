-- FIXME: this is all wrong because it references GopherBrowserState so it shoudl be in utils!
-- IIt also references loadAddress!
module Homepage where

import           Data.Text            as T

import           BrickApp.Types
import           Config
import           BrickApp.Utils.WaffleAddresses
import           Config.Homepage

-- FIXME: this should be in src/Brickapp/ModeAction/Homepage.hs
goHome :: GopherBrowserState -> IO GopherBrowserState
goHome gbs = do
  configParser <- getUserHomepageConfig
  unparsedURI <- readConfigParserValue configParser "homepage" "uri"
  loadAddress gbs (T.pack $ unparsedURI)
