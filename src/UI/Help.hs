-- | The help screen, which is also the homepage. It's just a TextFile, basically.
module UI.Help where

-- NOTE: should look to how search is handled as non-location i think

import Paths_waffle

import           Control.Exception

import qualified Graphics.Vty                  as V
import qualified Brick.Types                   as T
import           Brick.Widgets.Core             ( viewport
                                                , str
                                                )
import qualified Brick.Main                    as M

import           UI.Util
import           UI.TextFile
import           UI.Representation

helpModeUI :: GopherBrowserState -> [T.Widget MyName]
helpModeUI gbs = defaultBrowserUI gbs (viewport TextViewport T.Both) titleWidget mainWidget statusWidget
  where
   mainWidget   = str . clean $ getHelpTextFileContents gbs
   titleWidget  = str "Waffle Help"
   statusWidget = str "Help mode. Use arrows or hjkl to scroll."

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
  pure gbs { gbsBuffer = HelpBuffer $ Help { hText = TextFile { tfContents = helpContents, tfTitle = "Help: Using Waffle" }, hFormerGbs = gbs }, gbsRenderMode = HelpMode }
