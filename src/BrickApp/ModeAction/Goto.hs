{-# LANGUAGE OverloadedStrings #-}

-- | The heavy-lifting/actual actions of `GotoMode`.
module BrickApp.ModeAction.Goto where

import           Data.Maybe
import qualified Data.Text                     as T

import           Brick.Widgets.Core             ( txt )
import           Brick.Widgets.Edit            as E
import           Network.URI

import BrickApp.Types
import BrickApp.Types.Names
import BrickApp.Utils
import BrickApp.ModeAction.Progress
import Gopher

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
      formerGbs = formerMode gbs
  in  either (errorPopup gbs unparsedURI) (initProgressMode formerGbs Nothing) (tryLocationOrFail unparsedURI)
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
  tryLocationOrFail :: T.Text -> Either T.Text (T.Text, Int, T.Text, RenderMode, Maybe T.Text)
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
    Right (T.pack regName, port, removeGopherType $ T.pack resource, selectorToRenderMode $ T.pack resource, Nothing)

-- | Revert to mode prior to `GotoMode` being initiated.
formerMode :: GopherBrowserState -> GopherBrowserState
formerMode g = g { gbsRenderMode = seFormerMode $ fromJust $ gbsStatus g, gbsStatus = Nothing }
