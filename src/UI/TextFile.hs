-- | Everything related to the UI for viewing text files.
module UI.TextFile where

import           Control.Monad.IO.Class

import qualified Graphics.Vty                  as V
import qualified Brick.Types                   as T
import           Brick.Widgets.Center           ( center )
import           Brick.Widgets.Border           ( border )
import           Brick.Widgets.Core             ( viewport
                                                , vBox
                                                , str
                                                , hLimitPercent
                                                , vLimitPercent
                                                )
import qualified Brick.Main                    as M

import           UI.Util
import           UI.History
import           UI.Representation

-- | The UI for rendering and viewing a text file.
textFileModeUI :: GopherBrowserState -> [T.Widget MyName]
textFileModeUI gbs =
  let (TextFile textFileContents) = getTextFile gbs
      ui = viewport MyViewport T.Both $ vBox [str $ clean textFileContents]
  in  [center $ border $ hLimitPercent 100 $ vLimitPercent 100 ui]

textFileEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM MyName (T.Next GopherBrowserState)
textFileEventHandler gbs e = case e of
  V.EvKey (V.KChar 'j') [] -> M.vScrollBy myNameScroll 1 >> M.continue gbs
  V.EvKey (V.KChar 'k') [] -> M.vScrollBy myNameScroll (-1) >> M.continue gbs
  V.EvKey (V.KChar 'u') [] -> liftIO (goParentDirectory gbs) >>= M.continue
  V.EvKey (V.KChar 'f') [] -> liftIO (goHistory gbs 1) >>= M.continue
  V.EvKey V.KEsc        [] -> M.halt gbs
  V.EvKey (V.KChar 'b') [] -> liftIO (goHistory gbs (-1)) >>= M.continue
  _                        -> M.continue gbs
