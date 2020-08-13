module BrickApp.ModeAction.Search where

import qualified Data.Text                     as T

import           Brick.Widgets.Edit            as E

import Gopher
import BrickApp.Types
import BrickApp.Types.Helpers
import BrickApp.ModeAction.Progress

-- | Form a new application state based on a Gopher search request.
mkSearchResponseState :: GopherBrowserState -> IO GopherBrowserState
mkSearchResponseState gbs = do
  let host     = sbHost $ getSearch gbs
      port     = sbPort $ getSearch gbs
      resource = sbSelector $ getSearch gbs
      query    = T.unlines (E.getEditContents $ sbEditorState $ getSearch gbs)
      selector = searchSelector resource query
  initProgressMode gbs Nothing (host, port, selector, MenuMode, Nothing)
