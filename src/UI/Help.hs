{-# LANGUAGE OverloadedStrings #-}

-- | The help screen, which is also the homepage. It's just a TextFile, basically.
module UI.Help
  ( helpModeUI
  , helpEventHandler
  , modifyGbsForHelp
  ) where

-- NOTE: should look to how search is handled as non-location i think

import qualified Data.Text.IO                  as TIO
import qualified Data.Text                     as T
import           Control.Exception

import qualified Graphics.Vty                  as V
import qualified Brick.Types                   as T
import           Brick.Widgets.Core             ( viewport
                                                , txt
                                                )
import qualified Brick.Main                    as M

import           Paths_waffle
import           UI.Util
import           UI.TextFile
import           UI.Representation
import           Gopher

helpModeUI :: GopherBrowserState -> [T.Widget AnyName]
helpModeUI gbs = defaultBrowserUI gbs (viewport (MyName TextViewport) T.Both) titleWidget mainWidget statusWidget
  where
   mainWidget   = txt $ getHelpTextFileContents gbs
   titleWidget  = txt "Waffle Help"
   statusWidget = txt "Help mode. Use arrows or hjkl to scroll."

-- | Basic text file controls, modularized so that the Help screen can use
-- too, without including the history stuff. See the Help module.
helpEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM AnyName (T.Next GopherBrowserState)
helpEventHandler gbs e = case e of
  -- What about left and right?!
  V.EvKey V.KEsc        [] -> M.continue $ hFormerGbs $ getHelp gbs
  _                        -> basicTextFileEventHandler gbs e

getHelpContents :: IO T.Text
getHelpContents = do
  pathToHelpFile <- getDataFileName "data/help.txt"
  catch (cleanAll <$> TIO.readFile pathToHelpFile)
        (\e -> let err = show (e :: IOException)
               in  pure $ T.pack $ "Warning: Couldn't open " ++ pathToHelpFile ++ ": " ++ err)

-- | Initialize help mode.
modifyGbsForHelp :: GopherBrowserState -> IO GopherBrowserState
modifyGbsForHelp gbs = do
  helpContents <- getHelpContents
  pure gbs { gbsBuffer = HelpBuffer $ Help { hText = TextFile { tfContents = helpContents, tfTitle = "Help: Using Waffle" }, hFormerGbs = gbs }, gbsRenderMode = HelpMode }
