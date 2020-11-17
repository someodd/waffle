{-# LANGUAGE OverloadedStrings #-}

-- | The heavy-lifting/actual actions of `GotoMode`.
module BrickApp.ModeAction.Goto where

import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text                     as T

import qualified Brick.Types                   as B
import qualified Brick.Main                    as B
import           Brick.Widgets.Edit            as E

import BrickApp.Types
import BrickApp.Types.Names
import BrickApp.Utils.WaffleAddresses

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
--mkGotoResponseState :: GopherBrowserState -> IO GopherBrowserState
mkGotoResponseState :: GopherBrowserState -> B.EventM AnyName (B.Next GopherBrowserState)
mkGotoResponseState gbs =
  -- get the host, port, selector
  let unparsedURI = T.filter (/= '\n')
        $ T.unlines (E.getEditContents $ seEditorState $ fromJust $ gbsStatus gbs)
      formerGbs = formerMode gbs
  -- here is where i detect type first
  -- I should modularize this to be used elsewhere like home or follow links?
  in  liftIO (loadAddress formerGbs unparsedURI Nothing) >>= B.continue

-- | Revert to mode prior to `GotoMode` being initiated.
formerMode :: GopherBrowserState -> GopherBrowserState
formerMode g = g { gbsRenderMode = seFormerMode $ fromJust $ gbsStatus g, gbsStatus = Nothing }
