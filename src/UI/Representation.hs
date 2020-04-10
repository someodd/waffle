-- | Data types representing different RenderModes.
module UI.Representation where

import Data.Maybe

import qualified Brick.Types                   as T
import qualified Brick.BChan
import qualified Brick.Widgets.List            as BrickList -- (List)? FIXME
import           Brick.Widgets.FileBrowser      ( FileBrowser )
import           Brick.Widgets.Edit            as E

import           GopherClient

-- This is used to indicate how many bytes have been downloaded
-- of a menu or a save a text file etc, anything!
data Progress = Progress { pbBytesDownloaded :: Int
                         , pbMessage :: String -- BETTER NAME NEEDED
                         , pbInitGbs :: GopherBrowserState
                         , pbConnected :: Bool
                         }

data SaveBrowser = SaveBrowser { fbFileBrowser :: FileBrowser MyName
                               , fbCallBack :: String -> IO ()
                               , fbIsNamingFile :: Bool
                               , fbFileOutPath :: String
                               , fbFormerBufferState :: Buffer
                               , fbOriginalFileName :: String
                               }

data Search = Search { sbQuery :: String
                     , sbFormerBufferState :: Buffer
                     , sbSelector :: String
                     , sbPort :: Int
                     , sbHost :: String
                     , sbEditorState :: EditorState
                     }

data Goto = Goto { gFormerBufferState :: Buffer
                 , gEditorState :: EditorState
                 }

data Help = Help { hText :: TextFile
                 , hFormerGbs :: GopherBrowserState
                 }

-- | For MenuBuffer...
-- Simply used to store the current GopherMenu when viewing one during MenuMode.
-- The second element is the widget which is used when rendering a GopherMenu.
-- Simply used to store the current GopherMenu when viewing one during MenuMode.
newtype Menu = Menu (GopherMenu, BrickList.List MyName String, FocusLines)

-- FIXME: why not just type?
-- | This is for the contents of a File to be rendered when in TextFileMode.
-- this should be a combination of things. it should have the addres of the temporary file
-- which should then be moved to the picked location
newtype TextFile = TextFile String

-- | The data from which a UI is rendered.
data Buffer
  = MenuBuffer Menu
  | TextFileBuffer TextFile
  | FileBrowserBuffer SaveBrowser
  | SearchBuffer Search
  | ProgressBuffer Progress
  | HelpBuffer Help
  | GotoBuffer Goto

getGoto :: GopherBrowserState -> Goto
getGoto gbs = let (GotoBuffer goto) = gbsBuffer gbs in goto

-- Could use with below TODO NOTE
getHelp :: GopherBrowserState -> Help
getHelp gbs = let (HelpBuffer help) = gbsBuffer gbs in help

getHelpTextFileContents :: GopherBrowserState -> String
getHelpTextFileContents gbs = let (HelpBuffer help) = gbsBuffer gbs in getHelpContents $ hText help
  where
    getHelpContents (TextFile htf) = htf

updateFileBrowserBuffer :: GopherBrowserState -> (SaveBrowser -> SaveBrowser) -> GopherBrowserState
updateFileBrowserBuffer gbs f =
  let (FileBrowserBuffer sb) = gbsBuffer gbs
  in  gbs { gbsBuffer = FileBrowserBuffer (f sb) }

getMenu :: GopherBrowserState -> Menu
getMenu gbs = let (MenuBuffer m) = gbsBuffer gbs in m

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

updateGotoBuffer :: GopherBrowserState -> (Goto -> Goto) -> GopherBrowserState
updateGotoBuffer gbs f =
  let (GotoBuffer sb) = gbsBuffer gbs
  in  gbs { gbsBuffer = GotoBuffer (f sb) }

-- | The line #s which have linkable entries. Used for jumping by number and n and p hotkeys and display stuff.
-- use get elemIndex to enumerate
type FocusLines = [Int]

-- | The HistoryIndex is the index in the list of locations in the history.
-- 0 is the oldest location in history. See also: GopherBrowserState.
type HistoryIndex = Int

-- TODO: what if history also saved which line # you were on in cas eyou go back and forward
-- The history is a list of locations, where 0th element is the oldest and new
-- locations are appended. See also: newChangeHistory.
type History = ([Location], HistoryIndex)

-- | Gopher location in the form of domain, port, resource/magic string,
-- and the BrowserMode used to render it.
type Location = (String, Int, String, RenderMode)

-- | Carries through the entire state I guess!
-- think of this right now as a progress event
newtype CustomEvent = NewStateEvent GopherBrowserState

data Popup = Popup
  { pLabel :: String
  , pWidgets :: [T.Widget MyName]
  , pHelp :: String
  }

-- | The application state for Brick.
data GopherBrowserState = GopherBrowserState
  { gbsBuffer :: Buffer
  -- | The current location.
  , gbsLocation :: Location
  , gbsRenderMode :: RenderMode
  -- See: History
  , gbsHistory :: History
  , gbsChan :: Brick.BChan.BChan CustomEvent
  , gbsPopup :: Maybe Popup
  }

-- Should this go in Popup.hs? NOTE
hasPopup :: GopherBrowserState -> Bool
hasPopup gbs = isJust $ gbsPopup gbs

-- NOTE same as above: should be in Popup.hs probably!
closePopup :: GopherBrowserState -> GopherBrowserState
closePopup gbs = gbs { gbsPopup = Nothing }

data MyName = MyViewport | MyWidget
  deriving (Show, Eq, Ord)

data EditName = Edit1 deriving (Ord, Show, Eq)
type EditorState = E.Editor String MyName

-- TODO: maybe rename filebrowsermode to SaveMode or SaveFileMode
-- | Related to Buffer. Namely exists for History.
data RenderMode = MenuMode
                | TextFileMode
                | FileBrowserMode
                | SearchMode
                | ProgressMode
                | HelpMode
                | GotoMode
                deriving (Eq, Show)
