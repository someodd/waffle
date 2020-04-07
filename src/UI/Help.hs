-- | The help screen, which is also the homepage. It's just a TextFile, basically.
module UI.Help where

-- NOTE: should look to how search is handled as non-location i think

import Paths_waffle

import           Control.Exception

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
import           UI.TextFile
import           UI.Representation

-- | The UI for rendering and viewing a text file.
-- This is also used in the help screen/used by Help module.
helpModeUI :: GopherBrowserState -> [T.Widget MyName]
helpModeUI gbs =
  let textFileContents = getHelpTextFileContents gbs
      ui = viewport MyViewport T.Both $ vBox [str $ clean textFileContents]
  in  [center $ border $ hLimitPercent 100 $ vLimitPercent 100 ui]

-- | Basic text file controls, modularized so that the Help screen can use
-- too, without including the history stuff. See the Help module.
helpEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM MyName (T.Next GopherBrowserState)
helpEventHandler gbs e = case e of
  -- What about left and right?!
  V.EvKey V.KEsc        [] -> M.continue $ hFormerGbs $ getHelp gbs
  _                        -> basicTextFileEventHandler gbs e

getHelpContents :: IO String
getHelpContents = do
  pathToHelpFile <- getDataFileName "data/help.txt"
  catch (readFile pathToHelpFile)
        (\e -> let err = show (e :: IOException)
               in  pure $ "Warning: Couldn't open " ++ pathToHelpFile ++ ": " ++ err)

modifyGbsForHelp :: GopherBrowserState -> IO GopherBrowserState
modifyGbsForHelp gbs = do
  helpContents <- getHelpContents
  pure gbs { gbsBuffer = HelpBuffer $ Help { hText = TextFile helpContents, hFormerGbs = gbs }, gbsRenderMode = HelpMode }
