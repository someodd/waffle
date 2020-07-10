{-# LANGUAGE OverloadedStrings #-}

-- | Dialog for opening a Gopher URI dialog/UI.
module UI.Goto
  ( gotoEventHandler
  , initGotoMode
  ) where

import           Data.Maybe
import           Control.Monad.IO.Class

import           Brick.Widgets.Core             ( txt )
import qualified Data.Text                     as T
import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Widgets.Edit            as E
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events      ( Event )
import           Network.URI

import           UI.Representation
import           UI.Progress

initGotoMode :: GopherBrowserState -> GopherBrowserState
initGotoMode gbs = gbs
  { gbsRenderMode = GotoMode
  , gbsStatus     = Just $ StatusEditor { seLabel = "Goto: "
                                        , seEditorState = E.editor (MyName EditorViewport) Nothing ""
                                        , seFormerMode = gbsRenderMode gbs
                                        }
  }

-- FIXME: huge pages are sooo slow in text mode
-- FIXME: if I go to sdf.org it's fine and the goto status bar disappears. if i go to
-- tilde.black:70/users/brool/stoned.txt it stays put (i hav eto also remov eport because that crashes because of read)
-- FIXME: what if bad input?! what if can't resolve? errors in network need better handling
-- FIXME: what if NOT a menu!
-- should this be a part of progress? or ge called by progress instead?
mkGotoResponseState :: GopherBrowserState -> IO GopherBrowserState
mkGotoResponseState gbs =
  -- get the host, port, selector
  let unparsedURI = T.filter (/= '\n')
        $ T.unlines (E.getEditContents $ seEditorState $ fromJust $ gbsStatus gbs)
      gbsNoStatus = gbs { gbsStatus = Nothing }
  in  either (errorPopup gbs unparsedURI) (initProgressMode gbsNoStatus Nothing) (tryLocationOrFail unparsedURI)
 where
  prefixSchemeIfMissing :: T.Text -> T.Text
  prefixSchemeIfMissing potentialURI
    | "gopher://" `T.isPrefixOf` potentialURI = potentialURI
    | otherwise                               = "gopher://" <> potentialURI

  errorPopup :: GopherBrowserState -> T.Text -> T.Text -> IO GopherBrowserState
  errorPopup gbs' someBadURI message =
    let pop = Popup
                { pLabel   = "Goto input error!"
                , pWidgets = [txt message]
                , pHelp    = "Invalid:" <> someBadURI
                }
    in  pure $ gbs' { gbsPopup = Just pop }

  -- | Try to parse a `Location` from `Text` (which is hopefully
  -- some kind of valid URI), or give back an error message.
  tryLocationOrFail :: T.Text -> Either T.Text (T.Text, Int, T.Text, RenderMode)
  tryLocationOrFail potentialURI = do
    parsedURI <- case (parseURI . T.unpack $ prefixSchemeIfMissing potentialURI) of
      Just uri -> Right uri
      Nothing  -> Left "Cannot even begin to parse supplied URI!"
    authority' <- case uriAuthority parsedURI of
      Just auth -> Right auth
      Nothing   -> Left $ "Invalid URI (no authority)."
    regName    <- case uriRegName authority' of
      ""      -> Left "Invalid URI (no regname/host)."
      rn      -> Right rn
    port <- case uriPort authority' of
      ""     -> Right 70
      ':':p  -> Right (read p :: Int)
      _      -> Left $ "Invalid URI (bad port)." -- I don' think this ever can occur with Network.URI...
    let resource = uriPath parsedURI
    Right (T.pack regName, port, T.pack resource, guessMode $ T.pack resource)

-- | The Brick application event handler for search mode. See: UI.appEvent and
--- Brick.Main.appHandleEvent.
gotoEventHandler
  :: GopherBrowserState -> Event -> T.EventM AnyName (T.Next GopherBrowserState)
gotoEventHandler gbs e = case e of
    -- FIXME: esc quits! Change key...
  V.EvKey V.KEsc   [] -> M.continue $ formerMode gbs
  V.EvKey V.KEnter [] -> liftIO (mkGotoResponseState gbs) >>= M.continue
  _                   -> M.continue =<< editorEventHandler gbs e
 where
  -- FIXME: should also reset status
  formerMode g = g { gbsRenderMode = seFormerMode $ fromJust $ gbsStatus g, gbsStatus = Nothing }

  -- | A modification of the default Brick.Widgets.Edit event handler; changed to
  -- return a GopherBrowserState instead of just an editor state.
  editorEventHandler
    :: GopherBrowserState -> Event -> T.EventM AnyName GopherBrowserState
  -- TODO: e' is unused!
  editorEventHandler _ e' =
    -- Maybe this should be a general function in Representation.
    let updateEditorInStatus x = gbs { gbsStatus = Just $ (fromJust $ gbsStatus gbs) { seEditorState = x } }
    in  updateEditorInStatus
          <$> E.handleEditorEvent e' (seEditorState $ fromJust $ gbsStatus gbs)
