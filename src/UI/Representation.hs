-- | Data types representing different RenderModes.
module UI.Representation
  (
  -- * UI representations/models/types
    Progress(..)
  , SaveBrowser(..)
  , Search(..)
  , Help(..)
  , emptyCache
  , GopherBrowserState(..)
  , TextFile(..)
  , Menu(..)
  , Buffer(..)
  , StatusEditor(..)
  , Popup(..)
  , MyName(..)
  , RenderMode(..)
  , Location
  , Cache
  , History
  -- * Events
  , CustomEvent(..)
  -- * Helper utilities for UI representations
  , newMenuBuffer
  , closePopup
  , getHelp
  , getHelpTextFileContents
  , hasPopup
  , updateFileBrowserBuffer
  , isStatusEditing
  , getMenu
  , getSaveBrowser
  , getTextFile
  , getSearch
  , updateSearchBuffer
  , getProgress
  , updateProgressBuffer
  , guessMode
  ) where

import qualified Data.Text                     as T
import           Data.Maybe
import qualified Data.Map as Map

import qualified Brick.Types                   as T
import qualified Brick.BChan
import qualified Brick.Widgets.List            as BrickList -- (List)? FIXME
import           Brick.Widgets.FileBrowser      ( FileBrowser )
import           Brick.Widgets.Edit            as E

import           Gopher

-- This is used to indicate how many bytes have been downloaded
-- of a menu or a save a text file etc, anything!
data Progress = Progress { pbBytesDownloaded :: Int
                         , pbMessage :: T.Text -- BETTER NAME NEEDED
                         , pbInitGbs :: GopherBrowserState
                         , pbConnected :: Bool
                         , pbIsFromCache :: Bool
                         }

data SaveBrowser = SaveBrowser { fbFileBrowser :: FileBrowser MyName
                               , fbCallBack :: FilePath -> IO ()
                               , fbIsNamingFile :: Bool
                               , fbFileOutPath :: FilePath
                               , fbFormerBufferState :: Buffer
                               , fbOriginalFileName :: FilePath
                               }

data Search = Search { sbQuery :: T.Text
                     , sbFormerBufferState :: Buffer
                     , sbSelector :: T.Text
                     , sbPort :: Int
                     , sbHost :: T.Text
                     , sbEditorState :: EditorState
                     }

data Help = Help { hText :: TextFile
                 , hFormerGbs :: GopherBrowserState
                 }

-- The first string is the locationAsString and the second is filepath to the
-- tempfile. Maybe I should define "String" type as synonym CacheKey? or...
-- LocationString?
type Cache = Map.Map T.Text FilePath

emptyCache :: Cache
emptyCache = Map.empty

-- | For MenuBuffer...
-- Simply used to store the current GopherMenu when viewing one during MenuMode.
-- The second element is the widget which is used when rendering a GopherMenu.
-- Simply used to store the current GopherMenu when viewing one during MenuMode.
newtype Menu = Menu (GopherMenu, BrickList.List MyName T.Text, FocusLines)

-- | Construct a new `MenuBuffer` in the `GopherBrowserState` using
-- the supplied `Menu`.
newMenuBuffer gbs menu = gbs { gbsBuffer = MenuBuffer $ menu }

-- | Get `Menu` out of the buffer in `GopherBrowserState`.
getMenu :: GopherBrowserState -> Menu
getMenu gbs = let (MenuBuffer m) = gbsBuffer gbs in m

-- | This is for the contents of a File to be rendered when in TextFileMode.
-- this should be a combination of things. it should have the addres of the temporary file
-- which should then be moved to the picked location
data TextFile = TextFile { tfContents :: T.Text
                         , tfTitle :: T.Text
                         }

-- TODO: maybe break down buffer items into separate rep file and make rep subpackage...
-- | The data from which a UI is rendered.
data Buffer
  = MenuBuffer Menu
  | TextFileBuffer TextFile
  | FileBrowserBuffer SaveBrowser
  | SearchBuffer Search
  | ProgressBuffer Progress
  | HelpBuffer Help

-- help file should have title "Help" FIXME
-- Could use with below TODO NOTE
getHelp :: GopherBrowserState -> Help
getHelp gbs = let (HelpBuffer help) = gbsBuffer gbs in help

getHelpTextFileContents :: GopherBrowserState -> T.Text
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

-- TODO: there's an actual HostName, ServiceName in Network.TCP.Simple
-- | Gopher location in the form of domain, port, resource/magic string,
-- and the BrowserMode used to render it.
type Location = (T.Text, Int, T.Text, RenderMode)

-- FinalNewStateEvent is used for transition handlers and for sending the new state (like the new page; setting it as the new gbs)
-- | Carries through the entire state I guess!
-- think of this right now as a progress event
data CustomEvent = NewStateEvent GopherBrowserState | FinalNewStateEvent GopherBrowserState

-- FIXME: But what if we don't want a label, widgets, help? maybe there should be different
-- types of popups!
data Popup = Popup
  { pLabel :: T.Text
  , pWidgets :: [T.Widget MyName]
  , pHelp :: T.Text
  }

-- Works in conjunction with other modes like GotoMode which handles editing the statusEditor state
-- and using that to go to a certain URL.
-- Note that GotoMode isn't necessarily a "rendermode" but an event mode...
data StatusEditor = StatusEditor { seLabel :: T.Text, seEditorState :: EditorState, seFormerMode :: RenderMode }

isStatusEditing :: GopherBrowserState -> Bool
isStatusEditing gbs = case gbsStatus gbs of
  -- Is this pattern even right? FIXME
  (Just StatusEditor {}) -> True
  _                      -> False

-- TODO: maybe define an empty gbs?
-- | The application state for Brick.
data GopherBrowserState = GopherBrowserState
  { gbsBuffer :: Buffer
  -- | The current location.
  , gbsLocation :: Location
  , gbsRenderMode :: RenderMode -- Should just be "gbsMode" and "Mode" FIXME
  -- See: History
  , gbsHistory :: History
  , gbsChan :: Brick.BChan.BChan CustomEvent
  , gbsPopup :: Maybe Popup
  , gbsCache :: Cache
  , gbsStatus :: Maybe StatusEditor
  }

-- Should this go in Popup.hs? NOTE
hasPopup :: GopherBrowserState -> Bool
hasPopup gbs = isJust $ gbsPopup gbs

-- NOTE same as above: should be in Popup.hs probably!
closePopup :: GopherBrowserState -> GopherBrowserState
closePopup gbs = gbs { gbsPopup = Nothing }

data MyName = MyViewport | MainViewport | EditorViewport | MyWidget | TextViewport | MenuViewport
  deriving (Show, Eq, Ord)

data EditName = Edit1 deriving (Ord, Show, Eq)
type EditorState = E.Editor T.Text MyName

-- FIXME: maybe "rendermode" is bad now and should jsut be called "mode"
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

itemTypeToRenderMode :: ItemType -> RenderMode
itemTypeToRenderMode itemType = case itemType of
  Canonical Directory -> MenuMode
  Canonical File      -> TextFileMode
  _                   -> FileBrowserMode


guessMode :: Selector -> RenderMode
guessMode resource =
  itemTypeToRenderMode $ fromMaybe (Canonical Directory) (selectorItemType resource)
