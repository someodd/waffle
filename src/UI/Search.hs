module UI.Search where

import qualified Brick.Types as T
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core (viewport, vBox, str, hLimitPercent, vLimitPercent)

import UI.Util
import GopherClient (searchGet, makeGopherMenu)
import UI.History

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

-- | The UI for searching
searchModeUI :: GopherBrowserState -> [T.Widget MyName]
searchModeUI gbs =
  let sb = gbsBuffer gbs
      ui = viewport MyViewport T.Both $ vBox [str $ sbQuery sb]
  in [center $ border $ hLimitPercent 100 $ vLimitPercent 100 ui]
