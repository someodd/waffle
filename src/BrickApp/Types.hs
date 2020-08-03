-- | UI models/types used in rendering the TUI. For the names see
-- `UI.Types.Names`. Some of these models can be complex, so there
-- are helper functions at `UI.Types.Helpers`.

module BrickApp.Types where

import qualified Data.Map as Map
import qualified Data.Text                     as T

import           Brick.Widgets.FileBrowser      ( FileBrowser )
import           Brick.Widgets.Edit            as E
import qualified Brick.BChan
import qualified Brick.Types                   as T
import qualified Brick.Widgets.List            as BrickList -- (List)? FIXME
import qualified Brick.Focus as F

import Gopher                                   ( GopherMenu )
import BrickApp.Types.Names

-- | Used for rendering the TUI that changes the commands associated with
-- opening specific menu items.
data OpenConfigState =
  OpenConfigState { formerState :: GopherBrowserState
                  , focusRing :: F.FocusRing AnyName
                  , editFile :: E.Editor String AnyName
                  , editDirectory :: E.Editor String AnyName
                  , editCsoPhoneBookServer :: E.Editor String AnyName
                  , editError :: E.Editor String AnyName
                  , editBinHexedMacintoshFile :: E.Editor String AnyName
                  , editDosBinaryArchive :: E.Editor String AnyName
                  , editUnixUuencodedFile :: E.Editor String AnyName
                  , editIndexSearchServer :: E.Editor String AnyName
                  , editTextBasedTelnetSession :: E.Editor String AnyName
                  , editBinaryFile :: E.Editor String AnyName
                  , editRedundantServer :: E.Editor String AnyName
                  , editGifFile :: E.Editor String AnyName
                  , editImageFile :: E.Editor String AnyName
                  , editTn3270Session :: E.Editor String AnyName
                  , editDoc :: E.Editor String AnyName
                  , editHtmlFile :: E.Editor String AnyName
                  , editInformationalMessage :: E.Editor String AnyName
                  , editSoundFile :: E.Editor String AnyName
                  }

-- This is used to indicate how many bytes have been downloaded
-- of a menu or a save a text file etc, anything!
data Progress = Progress { pbBytesDownloaded :: Int
                         , pbMessage :: T.Text -- BETTER NAME NEEDED
                         , pbInitGbs :: GopherBrowserState
                         , pbConnected :: Bool
                         , pbIsFromCache :: Bool
                         }

data SaveBrowser = SaveBrowser { fbFileBrowser :: FileBrowser AnyName
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
newtype Menu = Menu (GopherMenu, BrickList.List AnyName T.Text, FocusLines)

-- | This is for the contents of a File to be rendered when in TextFileMode.
-- this should be a combination of things. it should have the addres of the temporary file
-- which should then be moved to the picked location
data TextFile = TextFile { tfContents :: T.Widget AnyName-- Should actually be txt
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
  | OpenConfigBuffer OpenConfigState

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

-- FIXME, TODO: there's an actual HostName, ServiceName in Network.TCP.Simple
-- | Gopher location in the form of domain, port, resource/magic string,
-- and the BrowserMode used to render it.
type Location = (T.Text, Int, T.Text, RenderMode)

-- FinalNewStateEvent is used for transition handlers and for sending the new state (like the new page; setting it as the new gbs)
-- | Carries through the entire state I guess!
-- think of this right now as a progress event
data CustomEvent = NewStateEvent GopherBrowserState | FinalNewStateEvent GopherBrowserState | ClearCacheEvent (T.EventM AnyName ())

-- FIXME: But what if we don't want a label, widgets, help? maybe there should be different
-- types of popups!
data Popup = Popup
  { pLabel :: T.Text
  , pWidgets :: [T.Widget AnyName]
  , pHelp :: T.Text
  }

-- Works in conjunction with other modes like GotoMode which handles editing the statusEditor state
-- and using that to go to a certain URL.
-- Note that GotoMode isn't necessarily a "rendermode" but an event mode...
data StatusEditor = StatusEditor { seLabel :: T.Text, seEditorState :: EditorState, seFormerMode :: RenderMode }

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

type EditorState = E.Editor T.Text AnyName

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
                | OpenConfigMode
                | MenuJumpMode
                | BookmarksMode
                | AddBookmarkMode
                deriving (Eq, Show)
