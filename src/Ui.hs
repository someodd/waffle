-- TODO: center everything better!
-- TODO: hlint
-- TODO: just disable enter/color for certain lines like information but let you select duh
-- TODO: color/invert keys in status bar
-- TODO: some of this stuff deserves to go in gopherclient

-- | This module handles the Brick UI for the Gopher client.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Ui (uiMain) where

import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.List
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Control.Monad.IO.Class
import Data.Maybe

import Web.Browser
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (viewport, hLimitPercent, vLimitPercent, updateAttrMap, withBorderStyle, str, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

import GopherClient

-- | The UI for rendering and viewing a text file.
textFileModeUI :: GopherBrowserState -> [Widget MyName]
textFileModeUI gbs =
  let (TextFileBuffer tfb) = gbsBuffer gbs
      ui = viewport MyViewport T.Both $ vBox [str $ clean tfb]
  in [C.center $ B.border $ hLimitPercent 100 $ vLimitPercent 100 ui]

-- | The UI for rendering and viewing a menu.
menuModeUI :: GopherBrowserState -> [Widget MyName]
menuModeUI gbs = [C.hCenter $ C.vCenter view]
  where
    (MenuBuffer (_, l, _)) = gbsBuffer gbs
    label = str " Item " <+> cur <+> str " of " <+> total -- TODO: should be renamed
    (host, port, resource, _) = gbsLocation gbs
    title = " " ++ host ++ ":" ++ show port ++ if not $ null resource then " (" ++ resource ++ ") " else " "
    cur = case l^.L.listSelectedL of
      Nothing -> str "-"
      Just i  -> str (show (i + 1))
    total = str $ show $ Vec.length $ l^.L.listElementsL
    box = updateAttrMap (A.applyAttrMappings borderMappings) $ withBorderStyle customBorder $ B.borderWithLabel (withAttr titleAttr $ str title) $ L.renderListWithIndex (listDrawElement gbs) True l
    view = vBox [ box, vLimit 1 $ str "Esc to exit. Vi keys to browse. Enter to open." <+> label]

-- | The draw handler which will choose a UI based on the browser's mode.
drawUI :: GopherBrowserState -> [Widget MyName]
drawUI gbs
  | renderMode == MenuMode = menuModeUI gbs
  | renderMode == TextFileMode = textFileModeUI gbs
  | otherwise = error "Cannot draw the UI for this unknown mode!"
  where renderMode = gbsRenderMode gbs

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

-- | Make a request based on the currently selected Gopher menu item and change
-- the application state (GopherBrowserState) to reflect the change.
newStateFromSelectedMenuItem :: GopherBrowserState -> IO GopherBrowserState
newStateFromSelectedMenuItem gbs = do
  o <- gopherGet host (show port) resource
  case lineType of
    (Left ct) -> case ct of
      Directory ->
        let newMenu = makeGopherMenu o
            location = mkLocation MenuMode
        in pure $ newStateForMenu newMenu location (newChangeHistory gbs location)
      File ->
        let location = mkLocation TextFileMode
        in pure gbs { gbsLocation = location
                    , gbsBuffer = TextFileBuffer $ clean o
                    , gbsRenderMode = TextFileMode
                    , gbsHistory = newChangeHistory gbs location
                    }
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

selectedMenuLine :: GopherBrowserState -> Either GopherLine MalformedGopherLine
selectedMenuLine gbs =
  -- given the scope of this function, i believe this error message is not horribly accurate in all cases where it might be used
  let lineNumber = fromMaybe (error "Hit enter, but nothing was selected to follow! I'm not sure how that's possible!") (l^.L.listSelectedL)
  in menuLine menu lineNumber
  where
    (MenuBuffer (menu, l, _)) = gbsBuffer gbs

menuLine :: GopherMenu -> Int -> Either GopherLine MalformedGopherLine
menuLine (GopherMenu ls) indx = ls !! indx

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

-- TODO: implement backspace as back in history which trims it
appEvent :: GopherBrowserState -> T.BrickEvent MyName e -> T.EventM MyName (T.Next GopherBrowserState)
appEvent gbs (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt gbs
-- This should be backspace
appEvent gbs (T.VtyEvent (V.EvKey (V.KChar 'b') [])) = liftIO (goHistory gbs (-1)) >>= M.continue
appEvent gbs (T.VtyEvent (V.EvKey (V.KChar 'f') [])) = liftIO (goHistory gbs 1) >>= M.continue
appEvent gbs (T.VtyEvent (V.EvKey (V.KChar 'u') [])) = liftIO (goParentDirectory gbs) >>= M.continue
-- check gbs if the state says we're handling a menu (list) or a text file (viewport)
appEvent gbs (T.VtyEvent e)
  | gbsRenderMode gbs == MenuMode = case e of
      V.EvKey V.KEnter [] -> liftIO (newStateFromSelectedMenuItem gbs) >>= M.continue
      V.EvKey (V.KChar 'n') [] -> M.continue $ jumpNextLink gbs
      V.EvKey (V.KChar 'p') [] -> M.continue $ jumpPrevLink gbs
      ev -> M.continue =<< updateMenuList <$> L.handleListEventVi L.handleListEvent ev (getMenuList gbs)
  -- viewport stuff here
  | gbsRenderMode gbs == TextFileMode = case e of
    V.EvKey (V.KChar 'j')  [] -> M.vScrollBy myNameScroll 1 >> M.continue gbs
    V.EvKey (V.KChar 'k')  [] -> M.vScrollBy myNameScroll (-1) >> M.continue gbs
    _ -> M.continue gbs
  | otherwise = error "Unrecognized mode in event."
  -- TODO FIXME: the MenuBuffer should be record syntax
  where
    getMenuList x =
      let (MenuBuffer (_, gl, _)) = gbsBuffer x
      in gl
    updateMenuList x =
      let (MenuBuffer (gm, _, fl)) = gbsBuffer gbs
      in gbs {gbsBuffer=MenuBuffer (gm, x, fl)}
appEvent gbs _ = M.continue gbs

-- FIXME: this is messy! unoptimized!
listDrawElement :: GopherBrowserState -> Int -> Bool -> String -> Widget MyName
listDrawElement gbs indx sel a =
  cursorRegion <+> possibleNumber <+> withAttr lineColor (lineDescriptorWidget (menuLine (gmenu) indx) <+> selStr a)
  where
    selStr s
      | sel && isInfoMsg (selectedMenuLine gbs) = withAttr custom2Attr (str s)
      | sel = withAttr customAttr $ str s
      | otherwise = str s

    (MenuBuffer (gmenu, mlist, focusLines)) = gbsBuffer gbs

    cursorRegion = if sel then withAttr asteriskAttr $ str " ➤ " else str "   "
    isLink = indx `elem` focusLines
    lineColor = if isLink then linkAttr else textAttr
    biggestIndexDigits = length $ show (Vec.length $ mlist^.L.listElementsL)
    curIndexDigits = length $ show $ fromJust $ indx `elemIndex` focusLines

    possibleNumber = if isLink then withAttr numberPrefixAttr $ str $ numberPad $ show (fromJust $ indx `elemIndex` focusLines) ++ ". " else str "" 
      where
      numberPad = (replicate (biggestIndexDigits - curIndexDigits) ' ' ++)

    lineDescriptorWidget :: Either GopherLine MalformedGopherLine -> Widget n
    lineDescriptorWidget line = case line of
      -- it's a gopherline
      (Left gl) -> case glType gl of
        -- Cannonical type
        (Left ct) -> case ct of
          Directory -> withAttr directoryAttr $ str "📂 [Directory] "
          File -> withAttr fileAttr $ str "📄 [File] "
          IndexSearchServer -> withAttr indexSearchServerAttr $ str "🔎 [IndexSearchServer] "
          _ -> withAttr genericTypeAttr $ str $ "[" ++ show ct ++ "] "
        -- Noncannonical type
        (Right nct) -> case nct of
          InformationalMessage -> str $ replicate (biggestIndexDigits+2) ' '
          HtmlFile -> withAttr directoryAttr $ str "🌐 [HTMLFile] "
          _ -> withAttr genericTypeAttr $ str $ "[" ++ show nct ++ "] "
      -- it's a malformed line
      (Right _) -> str ""

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

-- FIXME: didn't this get moved to GopherClient? need to do that
-- | Replaces certain characters to ensure the Brick UI doesn't get "corrupted."
clean :: String -> String
clean = replaceTabs . replaceReturns
  where
    replaceTabs = map (\x -> if x == '\t' then ' ' else x)
    replaceReturns = map (\x -> if x == '\r' then ' ' else x)

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

-- TODO: this all feels very messy
customAttr :: A.AttrName
customAttr = "custom"

custom2Attr :: A.AttrName
custom2Attr = "custom2"

directoryAttr :: A.AttrName
directoryAttr = "directoryAttr"

fileAttr :: A.AttrName
fileAttr = "fileAttr"

indexSearchServerAttr :: A.AttrName
indexSearchServerAttr = "indexSearchServerAttr"

genericTypeAttr :: A.AttrName
genericTypeAttr = "genericTypeAttr"

numberPrefixAttr :: A.AttrName
numberPrefixAttr = "numberPrefixAttr"

linkAttr :: A.AttrName
linkAttr = "linkAttr"

textAttr :: A.AttrName
textAttr = "textAttr"

-- TODO: bad name now...
asteriskAttr :: A.AttrName
asteriskAttr = "asteriskAttr"

titleAttr :: A.AttrName
titleAttr = "titleAttr"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr,                V.yellow `on` V.rgbColor (0 :: Int) (0 :: Int) (0 :: Int))
  , (L.listSelectedAttr,        (V.defAttr `V.withStyle` V.bold) `V.withForeColor` V.white)
  , (directoryAttr,             fg V.red)
  , (fileAttr,                  fg V.cyan)
  , (indexSearchServerAttr,     fg V.magenta)
  , (linkAttr,                  fg (V.rgbColor (28 :: Int) (152 :: Int) (255 :: Int)))
  , (textAttr,                  fg (V.rgbColor (255 :: Int) (255 :: Int) (0 :: Int)))
  , (genericTypeAttr,           fg V.green)
  , (numberPrefixAttr,          fg (V.rgbColor (252 :: Int) (40 :: Int) (254 :: Int)))
  , (customAttr,                (V.defAttr `V.withStyle` V.bold) `V.withForeColor` V.white)
  , (custom2Attr,               fg V.yellow)
  , (titleAttr,                 (V.defAttr `V.withStyle` V.reverseVideo) `V.withStyle` V.bold `V.withForeColor` V.white)
  , (asteriskAttr,              fg V.white)
  ]

customBorder :: BS.BorderStyle
customBorder = BS.BorderStyle
  { BS.bsCornerTL = '▚'
  , BS.bsCornerTR = '▚'
  , BS.bsCornerBR = '▚'
  , BS.bsCornerBL = '▚'
  , BS.bsIntersectFull = ' '
  , BS.bsIntersectL = ' '
  , BS.bsIntersectR = ' '
  , BS.bsIntersectT = ' '
  , BS.bsIntersectB = ' '
  , BS.bsHorizontal = '▚'
  , BS.bsVertical = ' '
  }

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings = [(B.borderAttr, V.cyan `on` V.rgbColor (0 :: Int) (0 :: Int) (0 :: Int))]

theApp :: M.App GopherBrowserState e MyName
theApp =
  M.App { M.appDraw = drawUI
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return
        , M.appAttrMap = const theMap
        }

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
  TextFileBuffer String
  -- ^ This is for the contents of a File to be rendered when in TextFileMode.

-- | Related to Buffer. Namely exists for History.
data RenderMode = MenuMode | TextFileMode
  deriving (Eq)

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

-- FIXME: isn't there a way to infer a location's type? Assuming first
-- link is a menu is a horrible hack...
-- | This is called in order to start the UI.
uiMain :: GopherMenu -> (String, Int, String) -> IO ()
uiMain gm (host, port, magicString) =
  let trueLocationType = (host, port, magicString, MenuMode)
  in void $ M.defaultMain theApp (newStateForMenu gm trueLocationType ([trueLocationType], 0))
