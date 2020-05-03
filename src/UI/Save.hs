-- TODO: rename stuff to *save* and not file browser etc
{-# LANGUAGE OverloadedStrings #-}
module UI.Save
  ( saveEventHandler
  , fileBrowserUi
  ) where

import           Control.Monad.IO.Class
import qualified Data.Text                     as T
import           Control.Exception              ( displayException )

import qualified Brick.Main                    as M
import qualified Brick.Widgets.FileBrowser     as FB
import qualified Graphics.Vty                  as V
import qualified Brick.Types                   as T
import           Brick.Widgets.Center           ( center
                                                , hCenter
                                                )
import           Brick.Widgets.Border           ( borderWithLabel )
import           Brick.Widgets.Core             ( vLimitPercent
                                                , hLimitPercent
                                                , (<=>)
                                                , txt
                                                , padTop
                                                , vBox
                                                , emptyWidget
                                                , str
                                                , withDefAttr
                                                )

import           UI.Style
import           UI.Representation

-- FIXME, TODO: document the features in handleFileBrowserEvent!
-- FIXME: only need to return GopherBrowserState actually
-- TODO: could just use built-in editor?
-- | Overrides handling file browse revents because we have a special text entry mode!
--- See also: handleFileBrowserEvent
handleFileBrowserEvent'
  :: (Ord n)
  => GopherBrowserState
  -> V.Event
  -> FB.FileBrowser n
  -> (GopherBrowserState, T.EventM n (FB.FileBrowser n))
handleFileBrowserEvent' gbs e b
  |
    -- FIXME: okay this is very wrong/messed up. take another look at regular handleFIleBrowserEvent'
    not isNamingFile && e == V.EvKey (V.KChar 'n') []
  = (initiateNamingState, pure b)
  -- If we are naming a file, then interpret events as we are in the file name input...
  | isNamingFile
  = case e of
    -- Enter key means we're done naming the file.
    V.EvKey V.KEnter [] ->
      ( finalOutFilePath $ FB.getWorkingDirectory b <> "/" <> curOutFilePath
      , pure b
      )
    -- Delete a character.
    V.EvKey V.KBS [] ->
      ( updateOutFilePath $ take (length curOutFilePath - 1) curOutFilePath
      , pure b
      )
    -- Entering in a character/appending a letter to name a file!
    V.EvKey (V.KChar c) [] ->
      (updateOutFilePath $ curOutFilePath ++ [c], pure b)
    _ -> (gbs, FB.handleFileBrowserEvent e b)
  -- Otherwise send things off to the default brick event handler!
  | otherwise
  = (gbs, FB.handleFileBrowserEvent e b)
 where
  initiateNamingState :: GopherBrowserState
  initiateNamingState =
    let cb x = x { fbIsNamingFile = True
                 , fbFileOutPath  = fbOriginalFileName (getSaveBrowser gbs)
                 }
    in  updateFileBrowserBuffer gbs cb

  finalOutFilePath :: FilePath -> GopherBrowserState
  finalOutFilePath p =
    let cb x = x { fbFileOutPath = p, fbIsNamingFile = False }
    in  updateFileBrowserBuffer gbs cb

  isNamingFile :: Bool
  isNamingFile = fbIsNamingFile (getSaveBrowser gbs)

  updateOutFilePath :: String -> GopherBrowserState
  updateOutFilePath p =
    let cb x = x { fbFileOutPath = p } in updateFileBrowserBuffer gbs cb

  curOutFilePath :: String
  curOutFilePath = fbFileOutPath (getSaveBrowser gbs)

-- FIXME
fileBrowserUi :: GopherBrowserState -> [T.Widget MyName]
fileBrowserUi gbs =
  [center $ vLimitPercent 100 $ hLimitPercent 100 $ ui <=> help]
 where
  b = fromBuffer $ getSaveBrowser gbs
  fromBuffer = fbFileBrowser
  ui = hCenter $ borderWithLabel (txt "Choose a file") $ FB.renderFileBrowser
    True
    b
  help = padTop (T.Pad 1) $ vBox
    [ case FB.fileBrowserException b of
      Nothing -> emptyWidget
      Just e ->
        hCenter $ withDefAttr errorAttr $ txt $ T.pack $ displayException e
    , hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
    , hCenter $ txt "Esc: quit/cancel save"
    , hCenter $ txt "n: name the output file and then hit enter"
    , hCenter $ str $ fbFileOutPath (getSaveBrowser gbs)
    ]

saveEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM MyName (T.Next GopherBrowserState)
saveEventHandler gbs e = case e of
  -- instances of 'b' need to tap into gbsbuffer
  -- TODO: document why this does so many checks to allow ESC to former state
  V.EvKey V.KEsc []
    | not
      (FB.fileBrowserIsSearching $ fromFileBrowserBuffer (getSaveBrowser gbs))
    -> M.continue $ returnFormerState gbs
  -- Handle a file browser event with Brick's file brower event handler, but
  -- with some added magic!
  _ -> do
    let (gbs', bUnOpen') = handleFileBrowserEvent'
          gbs
          e
          (fromFileBrowserBuffer $ getSaveBrowser gbs)
    b' <- bUnOpen'
    -- If the browser has a selected file after handling the
    -- event (because the user pressed Enter), shut down.
    let fileOutPath = fbFileOutPath (getSaveBrowser gbs')
    if isNamingFile gbs'
      then M.continue (upFileBrowserBuffer gbs' b')
    -- this errors now
      else if not (null $ getOutFilePath gbs')
        then liftIO (doCallBack fileOutPath) >>= M.continue
        else M.continue (upFileBrowserBuffer gbs' b')
 where
  fromFileBrowserBuffer = fbFileBrowser
  returnFormerState g = g { gbsBuffer = fbFormerBufferState $ getSaveBrowser g
                          , gbsRenderMode = MenuMode
                          }
  isNamingFile g = fbIsNamingFile (getSaveBrowser g)

  -- FIXME: redundant?
  upFileBrowserBuffer g bu =
    let cb x = x { fbFileBrowser = bu } in updateFileBrowserBuffer g cb

  getOutFilePath g = fbFileOutPath (getSaveBrowser g)
  doCallBack a = do
    fbCallBack (getSaveBrowser gbs) a
    pure $ gbs { gbsBuffer     = fbFormerBufferState $ getSaveBrowser gbs
               , gbsRenderMode = MenuMode
               }
