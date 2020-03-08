-- | Brick application state and functions manipulating said state(s).

module UI.StateUI where

import Data.Maybe
import Data.List as Lis
import qualified Data.Vector as Vec
import qualified Data.ByteString as BS

import Lens.Micro ((^.))
import Web.Browser
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.Main as M
import System.FilePath

import GopherClient

selectedMenuLine :: GopherBrowserState -> Either GopherLine MalformedGopherLine
selectedMenuLine gbs =
  -- given the scope of this function, i believe this error message is not horribly accurate in all cases where it might be used
  let lineNumber = fromMaybe (error "Hit enter, but nothing was selected to follow! I'm not sure how that's possible!") (l^.L.listSelectedL)
  in menuLine menu lineNumber
  where
    (MenuBuffer (menu, l, _)) = gbsBuffer gbs

-- | Change the state to the parent menu by network request.
goParentDirectory :: GopherBrowserState -> IO GopherBrowserState
goParentDirectory gbs = do
  let (host, port, magicString, _) = gbsLocation gbs
      parentMagicString = fromMaybe ("/") (parentDirectory magicString)
  o <- gopherGet host (show port) parentMagicString
  let newMenu = makeGopherMenu o
      newLocation = (host, port, parentMagicString, MenuMode)
  pure $ newStateForMenu newMenu newLocation (newChangeHistory gbs newLocation)

-- FIXME: can get an index error! should resolve with a dialog box.
-- Shares similarities with menu item selection
goHistory :: GopherBrowserState -> Int -> IO GopherBrowserState
goHistory gbs when = do
  let (history, historyMarker) = gbsHistory gbs
      newHistoryMarker = historyMarker + when
      location@(host, port, magicString, renderMode) = history !! newHistoryMarker
      newHistory = (history, newHistoryMarker)
  o <- gopherGet host (show port) magicString
  case renderMode of
    MenuMode ->
      let newMenu = makeGopherMenu o
      in pure $ newStateForMenu newMenu location newHistory
    TextFileMode -> pure $ gbs
      { gbsBuffer = TextFileBuffer $ clean o
      , gbsHistory = newHistory
      , gbsRenderMode = TextFileMode
      }
    m -> error $ "Should not be able to have a history item in the mode: " ++ show m

-- | Create a new history after visiting a new page.
--
-- The only way to change the list of locations in history. Everything after
-- the current location is dropped, then the new location is appended, and
-- the history index increased. Thus, the new location is as far "forward"
-- as the user can now go.
--
-- See also: GopherBrowserState.
newChangeHistory :: GopherBrowserState -> Location -> History
newChangeHistory gbs newLoc =
  let (history, historyMarker) = gbsHistory gbs
      newHistory = (take (historyMarker+1) history) ++ [newLoc]
      newHistoryMarker = historyMarker + 1
  in (newHistory, newHistoryMarker)


-- Inefficient
jumpNextLink :: GopherBrowserState -> GopherBrowserState
jumpNextLink gbs = updateMenuList (L.listMoveTo next l)
  where
    (MenuBuffer (_, l, focusLines)) = gbsBuffer gbs
    currentIndex = fromJust $ L.listSelected l
    next = fromMaybe (focusLines !! 0) (find (>currentIndex) focusLines)
    -- FIXME: repeated code
    updateMenuList ls =
      let (MenuBuffer (gm, _, fl)) = gbsBuffer gbs
      in gbs {gbsBuffer=MenuBuffer (gm, ls, fl)}

-- Inefficient
jumpPrevLink :: GopherBrowserState -> GopherBrowserState
jumpPrevLink gbs = updateMenuList (L.listMoveTo next l)
  where
    (MenuBuffer (_, l, focusLines)) = gbsBuffer gbs
    currentIndex = fromJust $ L.listSelected l
    next = fromMaybe (reverse focusLines !! 0) (find (<currentIndex) $ reverse focusLines)
    -- FIXME: repeated code
    updateMenuList ls =
      let (MenuBuffer (gm, _, fl)) = gbsBuffer gbs
      in gbs {gbsBuffer=MenuBuffer (gm, ls, fl)}

-- FIXME: more like makeState from menu lol. maybe can make do for any state
-- based on passing it the mode and other info! newStateForMenu?
newStateForMenu :: GopherMenu -> Location -> History -> GopherBrowserState
newStateForMenu gm@(GopherMenu ls) location history = GopherBrowserState
  { gbsBuffer = MenuBuffer (gm, L.list MyViewport glsVector 1, mkFocusLinesIndex gm)
  , gbsLocation = location
  , gbsHistory = history
  , gbsRenderMode = MenuMode
  }
  where
    glsVector = Vec.fromList $ map lineShow ls
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

-- | Make a request based on the currently selected Gopher menu item and change
-- the application state (GopherBrowserState) to reflect the change.
newStateFromSelectedMenuItem :: GopherBrowserState -> IO GopherBrowserState
newStateFromSelectedMenuItem gbs = do
  case lineType of
    (Left ct) -> case ct of
      Directory -> do
        o <- gopherGet host (show port) resource
        let newMenu = makeGopherMenu o
            location = mkLocation MenuMode
        pure $ newStateForMenu newMenu location (newChangeHistory gbs location)
      File -> do
        o <- gopherGet host (show port) resource
        let location = mkLocation TextFileMode
        pure gbs { gbsLocation = location
                 , gbsBuffer = TextFileBuffer $ clean o
                 , gbsRenderMode = TextFileMode
                 , gbsHistory = newChangeHistory gbs location
                 }
      IndexSearchServer -> pure gbs { gbsRenderMode = SearchMode, gbsBuffer = SearchBuffer { sbQuery = "", sbFormerBufferState = gbsBuffer gbs, sbSelector = resource, sbPort = port, sbHost = host } }
      ImageFile -> downloadState gbs host port resource
      _ -> error $ "Tried to open unhandled cannonical mode: " ++ show ct
    (Right nct) ->  case nct of
      HtmlFile -> openBrowser (drop 4 resource) >> pure gbs
      _ -> error $ "Tried to open unhandled noncannonical mode: " ++ show nct
   where
    (host, port, resource, lineType) = case selectedMenuLine gbs of
      -- GopherLine
      (Left gl) -> (glHost gl, glPort gl, glSelector gl, glType gl)
      -- Unrecognized line
      (Right _) -> error "Can't do anything with unrecognized line."
    mkLocation x = (host, port, resource, x)

downloadState :: GopherBrowserState -> String -> Int -> String -> IO GopherBrowserState
downloadState gbs host port resource =  do
  o <- downloadGet host (show port) resource
  --BS.writeFile "usefilechoserhere" o >> pure gbs-- XXX FIXME
  x <- FB.newFileBrowser selectNothing MyViewport Nothing
  pure $ gbs
    { gbsRenderMode = FileBrowserMode
    , gbsBuffer = FileBrowserBuffer { fbFileBrowser = x
                                    , fbCallBack = (`BS.writeFile` o)
                                    , fbIsNamingFile = False
                                    , fbFileOutPath = ""
                                    , fbOriginalFileName = takeFileName resource
                                    , fbFormerBufferState = gbsBuffer gbs
                                    }
    }
  where
    -- | This is for FileBrowser, because we don't want to overwrite anything,
    -- we want to browse through directories and then enter in a file name.
    selectNothing :: FB.FileInfo -> Bool
    selectNothing _ = False

-- XXX: how will location be done? FIXME this is broke currently...
mkSearchResponseState :: GopherBrowserState -> IO GopherBrowserState
mkSearchResponseState gbs = do
  let host = (sbHost $ gbsBuffer gbs)
      port = (sbPort $ gbsBuffer gbs)
      resource = (sbSelector $ gbsBuffer gbs)
      query = (sbQuery $ gbsBuffer gbs)
  (o, selector) <- searchGet host (show port) resource query
  let newMenu = makeGopherMenu o
      location = (host, port, selector, MenuMode)
  pure $ newStateForMenu newMenu location (newChangeHistory gbs location)
  -- XXX finish

-- this is for text mode scrolling
data MyName = MyViewport
  deriving (Show, Eq, Ord)

-- this is for text mode scrolling
myNameScroll :: M.ViewportScroll MyName
myNameScroll = M.viewportScroll MyViewport

-- | The HistoryIndex is the index in the list of locations in the history.
-- 0 is the oldest location in history. See also: GopherBrowserState.
type HistoryIndex = Int

-- TODO: what if history also saved which line # you were on in cas eyou go back and forward
-- The history is a list of locations, where 0th element is the oldest and new
-- locations are appended. See also: newChangeHistory.
type History = ([Location], HistoryIndex)

-- | The line #s which have linkable entries. Used for jumping by number and n and p hotkeys and display stuff.
-- use get elemIndex to enumerate
type FocusLines = [Int]

-- The types of contents for being rendered
data Buffer =
  -- Would it be worth doing it like this:
  --MenuBuffer {mbGopherMenu :: GopherMenu, mbList :: L.List MyName String, mbFocusLines :: FocusLines}
  -- | Simply used to store the current GopherMenu when viewing one during MenuMode.
  -- The second element is the widget which is used when rendering a GopherMenu.
  MenuBuffer (GopherMenu, L.List MyName String, FocusLines) |
  -- ^ Simply used to store the current GopherMenu when viewing one during MenuMode.
  TextFileBuffer String |
  -- ^ This is for the contents of a File to be rendered when in TextFileMode.
  -- this should be a combination of things. it should have the addres of the temporary file
  -- which should then be moved to the picked location
  FileBrowserBuffer { fbFileBrowser :: FB.FileBrowser MyName
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

-- | Gopher location in the form of domain, port, resource/magic string,
-- and the BrowserMode used to render it.
type Location = (String, Int, String, RenderMode)

-- | The application state for Brick.
data GopherBrowserState = GopherBrowserState
  { gbsBuffer :: Buffer
  -- | The current location.
  , gbsLocation :: Location
  , gbsRenderMode :: RenderMode
  -- See: History
  , gbsHistory :: History
  }

