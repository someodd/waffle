module UI.Util where

import qualified Data.Vector as Vector

import Brick.Main (ViewportScroll, viewportScroll)
import qualified Brick.Widgets.List as BrickList -- (List)? FIXME
import Brick.Widgets.FileBrowser (FileBrowser)

import GopherClient

-- FIXME: more like makeState from menu lol. maybe can make do for any state
-- based on passing it the mode and other info! newStateForMenu?
newStateForMenu :: GopherMenu -> Location -> History -> GopherBrowserState
newStateForMenu gm@(GopherMenu ls) location history = GopherBrowserState
  { gbsBuffer = MenuBuffer (gm, BrickList.list MyViewport glsVector 1, mkFocusLinesIndex gm)
  , gbsLocation = location
  , gbsHistory = history
  , gbsRenderMode = MenuMode
  }
  where
    glsVector = Vector.fromList $ map lineShow ls
    mkFocusLinesIndex (GopherMenu m) = map fst $ filter (not . isInfoMsg . snd) (zip [0..] m)

    -- | Used for filling up a list with display strings.
    lineShow :: Either GopherLine MalformedGopherLine -> String
    lineShow line = case line of
      -- It's a GopherLine
      (Left gl) -> case glType gl of
        -- Canonical type
        (Left _) -> clean $ glDisplayString gl
        -- Noncanonical type
        (Right nct) -> if nct == InformationalMessage && clean (glDisplayString gl) == "" then " " else clean $ glDisplayString gl
      -- It's a MalformedGopherLine
      (Right mgl) -> clean $ show mgl

-- FIXME: Is this appropriate for here? maybe another module?
-- | Replaces certain characters to ensure the Brick UI doesn't get "corrupted."
clean :: String -> String
clean = replaceTabs . replaceReturns
  where
    replaceTabs = map (\x -> if x == '\t' then ' ' else x)
    replaceReturns = map (\x -> if x == '\r' then ' ' else x)

-- this is for text mode scrolling
data MyName = MyViewport
  deriving (Show, Eq, Ord)

-- this is for text mode scrolling
myNameScroll :: ViewportScroll MyName
myNameScroll = viewportScroll MyViewport

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

-- The types of contents for being rendered
data Buffer =
  -- Would it be worth doing it like this:
  --MenuBuffer {mbGopherMenu :: GopherMenu, mbList :: L.List MyName String, mbFocusLines :: FocusLines}
  -- | Simply used to store the current GopherMenu when viewing one during MenuMode.
  -- The second element is the widget which is used when rendering a GopherMenu.
  MenuBuffer (GopherMenu, BrickList.List MyName String, FocusLines) |
  -- ^ Simply used to store the current GopherMenu when viewing one during MenuMode.
  TextFileBuffer String |
  -- ^ This is for the contents of a File to be rendered when in TextFileMode.
  -- this should be a combination of things. it should have the addres of the temporary file
  -- which should then be moved to the picked location
  FileBrowserBuffer { fbFileBrowser :: FileBrowser MyName
                    , fbCallBack :: String -> IO ()
                    , fbIsNamingFile :: Bool
                    , fbFileOutPath :: String
                    , fbFormerBufferState :: Buffer
                    , fbOriginalFileName :: String
                    } |
  -- FIXME: needs to be FileBrowserBuffer (FB.FIleBrowser MyName, funcToAcceptSelectedFileString, previousBufferToRestore)
  SearchBuffer { sbQuery :: String
               , sbFormerBufferState :: Buffer
               , sbSelector :: String
               , sbPort :: Int
               , sbHost :: String
               }

-- | Related to Buffer. Namely exists for History.
data RenderMode = MenuMode | TextFileMode | FileBrowserMode | SearchMode
  deriving (Eq, Show)

-- | The application state for Brick.
data GopherBrowserState = GopherBrowserState
  { gbsBuffer :: Buffer
  -- | The current location.
  , gbsLocation :: Location
  , gbsRenderMode :: RenderMode
  -- See: History
  , gbsHistory :: History
  }
