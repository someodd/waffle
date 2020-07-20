-- | Access and update helpers for handling types from `UI.Types`.
-- Functions for navigating `Ui.Types`.

module UI.Types.Helpers where

import Data.Maybe                               ( isJust )

import qualified Brick.Focus as F
import qualified Brick.Types                   as T

import UI.Types
import UI.Types.Names


-- | Return the `focusRing` from the `GopherBrowserState`'s
-- `OpenConfigState`.
getFocusRing :: GopherBrowserState -> F.FocusRing AnyName
getFocusRing gbs =
  let (OpenConfigBuffer openConfigState) = gbsBuffer gbs
  in  focusRing openConfigState

-- | Construct a new `MenuBuffer` in the `GopherBrowserState` using
-- the supplied `Menu`.
newMenuBuffer :: GopherBrowserState -> Menu -> GopherBrowserState
newMenuBuffer gbs menu = gbs { gbsBuffer = MenuBuffer $ menu }

-- | Get `Menu` out of the buffer in `GopherBrowserState`.
getMenu :: GopherBrowserState -> Menu
getMenu gbs = let (MenuBuffer m) = gbsBuffer gbs in m

getOpenConfig :: GopherBrowserState -> OpenConfigState
getOpenConfig gbs = let (OpenConfigBuffer openConfig) = gbsBuffer gbs in openConfig

-- help file should have title "Help" FIXME
-- Could use with below TODO NOTE
getHelp :: GopherBrowserState -> Help
getHelp gbs = let (HelpBuffer help) = gbsBuffer gbs in help

getHelpTextFileContents :: GopherBrowserState -> T.Widget AnyName -- txt
getHelpTextFileContents gbs = let (HelpBuffer help) = gbsBuffer gbs in tfContents $ hText help

updateFileBrowserBuffer :: GopherBrowserState -> (SaveBrowser -> SaveBrowser) -> GopherBrowserState
updateFileBrowserBuffer gbs f =
  let (FileBrowserBuffer sb) = gbsBuffer gbs
  in  gbs { gbsBuffer = FileBrowserBuffer (f sb) }

getProgress :: GopherBrowserState -> Progress
getProgress gbs = let (ProgressBuffer p) = gbsBuffer gbs in p

updateProgressBuffer :: GopherBrowserState -> (Progress -> Progress) -> GopherBrowserState
updateProgressBuffer gbs f =
  let (ProgressBuffer p) = gbsBuffer gbs
  in  gbs { gbsBuffer = ProgressBuffer (f p) }

-- | Get the SaveBrowser from Buffer
getSaveBrowser :: GopherBrowserState -> SaveBrowser
getSaveBrowser gbs = let (FileBrowserBuffer sb) = gbsBuffer gbs in sb

-- | Get the TextFile from Buffer.
getTextFile :: GopherBrowserState -> TextFile
getTextFile gbs = let (TextFileBuffer tf) = gbsBuffer gbs in tf

getSearch :: GopherBrowserState -> Search
getSearch gbs = let (SearchBuffer s) = gbsBuffer gbs in s

updateSearchBuffer :: GopherBrowserState -> (Search -> Search) -> GopherBrowserState
updateSearchBuffer gbs f =
  let (SearchBuffer sb) = gbsBuffer gbs
  in  gbs { gbsBuffer = SearchBuffer (f sb) }

isStatusEditing :: GopherBrowserState -> Bool
isStatusEditing gbs = case gbsStatus gbs of
  -- Is this pattern even right? FIXME
  (Just StatusEditor {}) -> True
  _                      -> False

-- Should this go in Popup.hs? NOTE
hasPopup :: GopherBrowserState -> Bool
hasPopup gbs = isJust $ gbsPopup gbs

-- NOTE same as above: should be in Popup.hs probably!
closePopup :: GopherBrowserState -> GopherBrowserState
closePopup gbs = gbs { gbsPopup = Nothing }
