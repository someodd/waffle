-- | History and general navigation.
module UI.History where

import           Data.Maybe

import           UI.Util
import           UI.Representation
import           GopherClient

-- FIXME: can get an index error! should resolve with a dialog box.
-- Shares similarities with menu item selection
goHistory :: GopherBrowserState -> Int -> IO GopherBrowserState
goHistory gbs when = do
  let
    (history, historyMarker) = gbsHistory gbs
    unboundIndex             = historyMarker + when
    historyLastIndex         = length history - 1
    newHistoryMarker
      | unboundIndex > historyLastIndex = historyLastIndex
      | unboundIndex < 0 = 0
      | otherwise = unboundIndex
    location@(host, port, magicString, renderMode) =
      history !! newHistoryMarker
    newHistory = (history, newHistoryMarker)
  o <- gopherGet host (show port) magicString-- FIXME
  case renderMode of
    MenuMode ->
      let newMenu = makeGopherMenu o
      in  pure $ newStateForMenu (gbsChan gbs) newMenu location newHistory
    TextFileMode -> pure $ gbs
      { gbsBuffer     = TextFileBuffer $ TextFile $ clean o
      , gbsHistory    = newHistory
      , gbsRenderMode = TextFileMode
      }
    m ->
      error
        $  "Should not be able to have a history item in the mode: "
        ++ show m

-- | Create a new history after visiting a new page.
--
-- The only way to change the list of locations in history. Everything after
-- the current location is dropped, then the new location is appended, and
-- the history index increased. Thus, the new location is as far "forward"
-- as the user can now go.
--
-- See also: GopherBrowserState.
newChangeHistory :: GopherBrowserState -> Location -> History
newChangeHistory gbs newLoc =
  let (history, historyMarker) = gbsHistory gbs
      newHistory               = take (historyMarker + 1) history ++ [newLoc]
      newHistoryMarker         = historyMarker + 1
  in  (newHistory, newHistoryMarker)

-- | Change the state to the parent menu by network request.
goParentDirectory :: GopherBrowserState -> IO GopherBrowserState
goParentDirectory gbs = do
  let (host, port, magicString, _) = gbsLocation gbs
      parentMagicString            = fromMaybe "/" (parentDirectory magicString)
  o <- gopherGet host (show port) parentMagicString
  let newMenu     = makeGopherMenu o
      newLocation = (host, port, parentMagicString, MenuMode)
  pure $ newStateForMenu (gbsChan gbs)
                         newMenu
                         newLocation
                         (newChangeHistory gbs newLocation)
