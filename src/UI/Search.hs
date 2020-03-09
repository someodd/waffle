module UI.Search where

import qualified Brick.Types as T

import UI.Util
import GopherClient (searchGet, makeGopherMenu)
import UI.History
import UI.InputDialog

searchInputUI :: GopherBrowserState -> [T.Widget MyName]
searchInputUI gbs = inputDialogUI inputText labelText helpText
  where
    searchBuffer = gbsBuffer gbs
    inputText = sbQuery searchBuffer
    labelText = "Search: " ++ (sbHost searchBuffer)
    helpText = "Press ENTER to search"

-- XXX: how will location be done? FIXME this is broke currently...
mkSearchResponseState :: GopherBrowserState -> IO GopherBrowserState
mkSearchResponseState gbs = do
  let host = (sbHost $ gbsBuffer gbs)
      port = (sbPort $ gbsBuffer gbs)
      resource = (sbSelector $ gbsBuffer gbs)
      query = (sbQuery $ gbsBuffer gbs)
  (o, selector) <- searchGet host (show port) resource query
  let newMenu = makeGopherMenu o
      location = (host, port, selector, MenuMode)
  pure $ newStateForMenu newMenu location (newChangeHistory gbs location)
  -- XXX finish
