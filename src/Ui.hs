-- TODO: center everything better!
-- TODO: hlint
-- TODO: just disable enter/color for certain lines like information but let you select duh
--- TODO: use INI theme function so people can edit INI file to set
-- TODO: color/invert keys in status bar
-- TODO: some of this stuff deserves to go in gopherclient
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Ui where

import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.List
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Control.Monad.IO.Class
import Data.Maybe

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

textFileModeUI :: GopherBrowserState -> [Widget MyName]
textFileModeUI gbs =
  let ui = viewport MyViewport T.Both $ vBox [str $ clean $ gbsText gbs]
  in [C.center $ B.border $ hLimitPercent 100 $ vLimitPercent 100 ui]

menuModeUI :: GopherBrowserState -> [Widget MyName]
menuModeUI gbs = [C.hCenter $ C.vCenter view]
  where
    l = gbsList gbs
    label = str " Item " <+> cur <+> str " of " <+> total -- TODO: should be renamed
    (host, port, resource) = gbsLocation gbs
    title = " " ++ host ++ ":" ++ show port ++ if not $ null resource then " (" ++ resource ++ ") " else " "
    cur = case l^.L.listSelectedL of
      Nothing -> str "-"
      Just i  -> str (show (i + 1))
    total = str $ show $ Vec.length $ l^.L.listElementsL
    box = updateAttrMap (A.applyAttrMappings borderMappings) $ withBorderStyle customBorder $ B.borderWithLabel (withAttr titleAttr $ str title) $ L.renderListWithIndex (listDrawElement gbs) True l
    view = vBox [ box, vLimit 1 $ str "Esc to exit. Vi keys to browse. Enter to open." <+> label]

drawUI :: GopherBrowserState -> [Widget MyName]
drawUI gbs
  | gbsMode gbs == MenuMode = menuModeUI gbs
  | gbsMode gbs == TextFileMode = textFileModeUI gbs
  | otherwise = error "Cannot draw the UI for this unknown mode!"

-- NOTE: should parts of this go in gopherclient? the problem currently is that it
-- returns a gopher browser state, but otherwise the dependence on gbs can be easily
-- avoided.
-- | Make a Gopher Protocol request based on the information in state and update
-- the state based on said request.
requestNewState :: GopherBrowserState -> IO GopherBrowserState
requestNewState gbs = do
  o <- gopherGet host (show port) resource
  let newMode = decipherMode lineType
  if newMode == MenuMode then
    let newMenu = makeGopherMenu o
    in pure $ makeState newMenu location
  else if newMode == TextFileMode then
    pure gbs { gbsLocation = location
             , gbsText = o
             , gbsMode = TextFileMode
             }
  else
    error "nop"
  where
  (host, port, resource, lineType) = case selectedMenuLine gbs of
    -- GopherLine
    (Left gl) -> (glHost gl, glPort gl, glSelector gl, glType gl)
    -- Unrecognized line
    (Right _) -> error "Can't do anything with unrecognized line."
  location = (host, port, resource)
  decipherMode someType = case someType of
    -- canonical type
    (Left ct) -> case ct of
      Directory -> MenuMode
      File -> TextFileMode
      _ -> error $ "Tried to open unhandled cannonical mode: " ++ show ct
    (Right nct) -> error $ "Tried to open unhandled noncannonical mode: " ++ show nct

selectedMenuLine :: GopherBrowserState -> Either GopherLine MalformedGopherLine
selectedMenuLine gbs = menuLine (gbsMenu gbs) lineNumber
  where 
  lineNumber = fromMaybe (error "Hit enter, but nothing was selected to follow! I'm not sure how that's possible!") (gbsList gbs^.L.listSelectedL)

menuLine :: GopherMenu -> Int -> Either GopherLine MalformedGopherLine
menuLine (GopherMenu ls) indx = ls !! indx

appEvent :: GopherBrowserState -> T.BrickEvent MyName e -> T.EventM MyName (T.Next GopherBrowserState)
appEvent gbs (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt gbs
-- check gbs if the state says we're handling a menu (list) or a text file (viewport)
appEvent gbs (T.VtyEvent e)
  | gbsMode gbs == MenuMode = case e of
      V.EvKey V.KEnter [] -> liftIO (requestNewState gbs) >>= M.continue
      ev -> M.continue =<< (\x -> gbs {gbsList=x}) <$> L.handleListEventVi L.handleListEvent ev (gbsList gbs)
  -- viewport stuff here
  | gbsMode gbs == TextFileMode = case e of
    V.EvKey (V.KChar 'j')  [] -> M.vScrollBy myNameScroll 1 >> M.continue gbs
    _ -> error "woop"
  | otherwise = error "Unrecognized mode in event."
appEvent gbs _ = M.continue gbs

-- FIXME: this is messy! unoptimized!
listDrawElement :: GopherBrowserState -> Int -> Bool -> String -> Widget MyName
listDrawElement gbs indx sel a =
  cursorRegion <+> possibleNumber <+> withAttr lineColor (lineDescriptorWidget (menuLine (gbsMenu gbs) indx) <+> selStr a)
  where
  selStr s
    | sel && isInfoMsg (selectedMenuLine gbs) = withAttr custom2Attr (str s)
    | sel = withAttr customAttr $ str s
    | otherwise = str s

  cursorRegion = if sel then withAttr asteriskAttr $ str " ➤ " else str "   "
  isLink = indx `elem` gbsFocusLines gbs
  lineColor = if isLink then linkAttr else textAttr
  biggestIndexDigits = length $ show (Vec.length $ gbsList gbs^.L.listElementsL)
  curIndexDigits = length $ show $ fromJust $ indx `elemIndex` gbsFocusLines gbs

  possibleNumber = if isLink then withAttr numberPrefixAttr $ str $ numberPad $ show (fromJust $ indx `elemIndex` gbsFocusLines gbs) ++ ". " else str "" 
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

clean :: String -> String
clean = replaceTabs . replaceReturns
  where
    replaceTabs = map (\x -> if x == '\t' then ' ' else x)
    replaceReturns = map (\x -> if x == '\r' then ' ' else x)

-- FIXME: more like makeState from menu lol. maybe can make do for any state
-- based on passing it the mode and other info!
makeState :: GopherMenu -> Location -> GopherBrowserState
makeState gm@(GopherMenu ls) location = GopherBrowserState
  { gbsList = L.list MyViewport glsVector 1
  , gbsMenu = gm
  , gbsFocusLines = mkFocusLinesIndex gm
  , gbsLocation = location
  , gbsMode = MenuMode
  , gbsText = ""
  }
  where
  glsVector = Vec.fromList $ map lineShow ls
  mkFocusLinesIndex (GopherMenu m) = map fst $ filter (not . isInfoMsg . snd) (zip [0..] m)

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

asteriskAttr :: A.AttrName
asteriskAttr = "asteriskAttr"

titleAttr :: A.AttrName
titleAttr = "titleAttr"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr,            V.yellow `on` V.rgbColor (0 :: Int) (0 :: Int) (0 :: Int))
  , (L.listSelectedAttr,            (V.defAttr `V.withStyle` V.bold) `V.withForeColor` V.white)
  , (directoryAttr,         fg V.red)
  , (fileAttr,         fg V.cyan)
  , (indexSearchServerAttr,         fg V.magenta)
  , (linkAttr,            fg (V.rgbColor (28 :: Int) (152 :: Int) (255 :: Int)))
  , (textAttr,            fg (V.rgbColor (255 :: Int) (255 :: Int) (0 :: Int)))
  , (genericTypeAttr,         fg V.green)
  , (numberPrefixAttr,            fg (V.rgbColor (252 :: Int) (40 :: Int) (254 :: Int)))
  , (customAttr,            (V.defAttr `V.withStyle` V.bold) `V.withForeColor` V.white)
  , (custom2Attr,           fg V.yellow)
  , (titleAttr,           (V.defAttr `V.withStyle` V.reverseVideo) `V.withStyle` V.bold `V.withForeColor` V.white)
  , (asteriskAttr,           fg V.white)
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

data BrowserMode = MenuMode | TextFileMode
  deriving (Show, Eq, Ord)

-- this is for text mode scrolling
data MyName = MyViewport
  deriving (Show, Eq, Ord)

-- this is for text mode scrolling
myNameScroll :: M.ViewportScroll MyName
myNameScroll = M.viewportScroll MyViewport

data GopherBrowserState = GopherBrowserState
  { gbsMenu :: GopherMenu
  , gbsList :: L.List MyName String
  -- | The line #s which have linkable entries. Used for jumping by number and n and p hotkeys and display stuff.
  , gbsFocusLines  :: [Int]  -- NOTE: use get elemIndex to enumerate
  , gbsLocation :: Location
  , gbsMode :: BrowserMode
  , gbsText :: String
  }

-- | Gopher location in the form of domain, port, resource/magic string.
type Location = (String, Int, String)

uiMain :: GopherMenu -> Location -> IO ()
uiMain gm location = void $ M.defaultMain theApp (makeState gm location)
