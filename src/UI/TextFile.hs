-- | Everything related to the UI for viewing text files.
module UI.TextFile where

import qualified Brick.Types as T
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core (viewport, vBox, str, hLimitPercent, vLimitPercent)

import UI.Util
  ( GopherBrowserState, MyName(..)
  , Buffer(TextFileBuffer)
  , GopherBrowserState(..)
  , clean
  )

-- | The UI for rendering and viewing a text file.
textFileModeUI :: GopherBrowserState -> [T.Widget MyName]
textFileModeUI gbs =
  let (TextFileBuffer tfb) = gbsBuffer gbs
      ui = viewport MyViewport T.Both $ vBox [str $ clean tfb]
  in [center $ border $ hLimitPercent 100 $ vLimitPercent 100 ui]
