-- | Data types representing different RenderModes.
module UI.Representation where

import qualified Brick.BChan
import qualified Brick.Widgets.List            as BrickList -- (List)? FIXME
import           Brick.Widgets.FileBrowser      ( FileBrowser )
import           Brick.Widgets.Edit            as E

import           GopherClient

-- This is used to indicate how many bytes have been downloaded
-- of a menu or a save a text flie etc, anything!
data Progress = Progress { pbBytesDownloaded :: Int
                         , pbMessage :: String -- BETTER NAME NEEDED
                         , pbInitGbs :: GopherBrowserState
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

-- | For MenuBuffer...
-- Simply used to store the current GopherMenu when viewing one during MenuMode.
-- The second element is the widget which is used when rendering a GopherMenu.
-- Simply used to store the current GopherMenu when viewing one during MenuMode.
data Menu = Menu (GopherMenu, BrickList.List MyName String, FocusLines)

-- FIXME: why not just type?
-- | This is for the contents of a File to be rendered when in TextFileMode.
-- this should be a combination of things. it should have the addres of the temporary file
-- which should then be moved to the picked location
data TextFile = TextFile String

-- | The data from which a UI is rendered.
data Buffer
  = MenuBuffer Menu
  | TextFileBuffer TextFile
  | FileBrowserBuffer SaveBrowser
  | SearchBuffer Search
  | ProgressBuffer Progress

updateFileBrowserBuffer gbs f =
  let (FileBrowserBuffer sb) = gbsBuffer gbs
  in  gbs { gbsBuffer = FileBrowserBuffer (f sb) }

getMenu gbs = let (MenuBuffer m) = gbsBuffer gbs in m

getProgress gbs = let (ProgressBuffer p) = gbsBuffer gbs in p

updateProgressBuffer gbs f =
  let (ProgressBuffer p) = gbsBuffer gbs
  in  gbs { gbsBuffer = ProgressBuffer (f p) }

-- | Get the SaveBrowser from Buffer
getSaveBrowser gbs = let (FileBrowserBuffer sb) = gbsBuffer gbs in sb

-- | Get the TextFile from Buffer.
getTextFile gbs = let (TextFileBuffer tf) = gbsBuffer gbs in tf

getSearch gbs = let (SearchBuffer s) = gbsBuffer gbs in s

-- FIXME: could update to use getSaveBrowser
updateSearchBuffer gbs f =
  let (SearchBuffer sb) = gbsBuffer gbs
  in  gbs { gbsBuffer = SearchBuffer (f sb) }

-- OLD STUFF NOT USED... DELETE FIXME
menuFromBuffer :: Buffer -> Menu
menuFromBuffer (MenuBuffer s) = s

textFileFromBuffer :: Buffer -> TextFile
textFileFromBuffer (TextFileBuffer s) = s

saveBrowserFromBuffer :: Buffer -> SaveBrowser
saveBrowserFromBuffer (FileBrowserBuffer s) = s

searchFromBuffer :: Buffer -> Search
searchFromBuffer (SearchBuffer s) = s

progressFromBuffer :: Buffer -> Progress
progressFromBuffer (ProgressBuffer s) = s

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
data CustomEvent = NewStateEvent GopherBrowserState

-- | The application state for Brick.
data GopherBrowserState = GopherBrowserState
  { gbsBuffer :: Buffer
  -- | The current location.
  , gbsLocation :: Location
  , gbsRenderMode :: RenderMode
  -- See: History
  , gbsHistory :: History
  , gbsChan :: Brick.BChan.BChan CustomEvent
  }

data MyName = MyViewport | MyWidget
  deriving (Show, Eq, Ord)

data EditName = Edit1 deriving (Ord, Show, Eq)
type EditorState = E.Editor String MyName

-- TODO: maybe rename filebrowsermode to SaveMode or SaveFileMode
-- | Related to Buffer. Namely exists for History.
data RenderMode = MenuMode | TextFileMode | FileBrowserMode | SearchMode | ProgressMode
  deriving (Eq, Show)
