-- TODO: use INI theme function so people can edit INI file to set
-- various attribute styles/colors!
-- FIXME: the gopherplus stuff needs to be havin its tabs removed yarrr
module Ui where

import Brick
import qualified Graphics.Vty as Vty
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Graphics.Vty
import Graphics.Vty.Attributes

import qualified Data.Text as T
import Control.Monad
import Data.List
import Data.Maybe
import Control.Monad.IO.Class

import GopherClient

data MyState = MyState
  { msMenu :: GopherMenu
  , msLinkIndexMarker :: Int -- msFocusIndex?
  , msLinkIndices :: FocusList -- msFocusList?
  , msFocused :: Bool
  } deriving (Show)

data MyName = MyViewport
  deriving (Show, Eq, Ord)

type MyApp = App MyState () MyName

-- | Replace the \t (tabs) and \r (returns) with spaces, because otherwise
-- they'll break Brick!
clean :: String -> String
clean = replaceTabs . replaceReturns
  where
    replaceTabs = map (\x -> if x == '\t' then ' ' else x)
    replaceReturns = map (\x -> if x == '\r' then ' ' else x)

menuStr :: GopherMenu -> Widget n
menuStr m = str . clean $ show m

globalDefault :: Attr
globalDefault = white `on` black

theMap :: AttrMap
theMap = attrMap globalDefault
  [ (attrName "link", fg brightBlue)
  , (attrName "activeLink", (defAttr `withStyle` bold) `withForeColor` brightYellow)
  ]
-- theMap = attrMap globalDefault [ (attrName "link", fg yellow <> underline (W.Word8 "blink")) ]

gopherMenuWidgets (GopherMenu m) activeLine = vBox $ map gopherLineWidget numberedLines
  where
  -- FIXME: should just bring back giving each line their own number
  numberedLines = zip [0..] m

  gopherLineWidget :: (Int, Either GopherLine MalformedGopherLine) -> Widget n
  gopherLineWidget (lineNumber, l) = case l of
    -- GopherLine
    -- FIXME: should also see if not info line by noncanon or canon and mark link
    (Left gl) ->
      case glType gl of
        -- Canonical
        (Left _) ->
          if lineNumber == activeLine then
            withAttr (attrName "activeLink") $ (visible . str $ " --> ") <+> (str $ show gl)
          else
            withAttr (attrName "link") $ str "     " <+> (str $ show gl)
        -- Noncanon
        (Right t) ->
          if t == InformationalMessage then
            str "     " <+> (str $ show gl)
          else
            withAttr (attrName "link") $ str "     " <+> (str $ show gl)
    -- MalformedLine
    (Right ml) -> str "" <+> (str $ show ml)


-- I get the feeling that there's a far more efficient way to do this in brick lmao
buildLinkIndices :: GopherMenu -> [Int]
buildLinkIndices (GopherMenu ls) = findIndices isNotInfoLine ls
  where
  isNotInfoLine l = case l of
    -- GopherLine
    (Left gl) -> case glType gl of
      -- Canonical type
      (Left c) -> True
      -- Noncanonical type
      (Right nc) -> nc /= InformationalMessage
    -- MalformedGopherLine
    (Right _) -> False

myNameScroll :: M.ViewportScroll MyName
myNameScroll = M.viewportScroll MyViewport

drawUi :: MyState -> [Widget MyName]
drawUi s = [C.center $ B.border $ hLimitPercent 100 $ vLimitPercent 100 $ ui]
  where
  --ui = viewport MyViewport Both $ vBox [menuStr $ msMenu s]
  ui = viewport MyViewport Both $ gopherMenuWidgets (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)

type FocusList = [Int]
-- type FocusIndex?

-- | Give the new focus index after moving focus down one, wrapping
-- to top if already at bottom.
scrollFocusDown :: FocusList -> Int -> Int
scrollFocusDown fl i = if i+1 > length fl - 1 then 0 else i+1
-- i == the focusIndex

-- | Give the new focus index after moving focus up one, wrapping
-- to bottom if already at top.
scrollFocusUp :: FocusList -> Int -> Int
scrollFocusUp fl i = if i-1 < 0 then length fl - 1 else i-1

-- | Follow active menu item and thus refresh the state
requestNewState :: MyState -> IO MyState
requestNewState s = do
  let gopherMenuList = fromMenu $ msMenu s
  -- NOTE: This would've worked if could applicative it..
  --let currentItem = fmap (!!) (msMenu s) <*> (msLinkIndices s !! msLinkIndexMarker s)
  -- Both feel hacky tho like I can't use !! hrm i don't wanna use type synonym tho
  let currentItem = gopherMenuList !! (msLinkIndices s !! msLinkIndexMarker s)
  case currentItem of
    -- valid gopher line
    (Left gl) -> do
      let host = glHost gl
          port = show $ glPort gl
          selector = glSelector gl
      o <- gopherGet host port selector
      let initialMenu = makeGopherMenu o
          linkIndices = buildLinkIndices initialMenu
      pure $ MyState {msMenu = initialMenu, msLinkIndexMarker = 0, msLinkIndices=linkIndices}
    -- malformed gopher line
    (Right _) -> error "Should be impossible!"

-- NOTE: when scrolling text we disable visible/active so we can actually move beyond the actively selected link
handleEvent :: MyState -> BrickEvent MyName () -> EventM MyName (Next MyState)
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
-- ^ quit
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'j')  [])) =
  let newFocusPos = scrollFocusDown (msLinkIndices s) (msLinkIndexMarker s)
  in M.continue $ s {msLinkIndexMarker=newFocusPos}
  --let (_, newIndexPos, newMenu) = updateMenuNewLinkPos s 1
  --in M.continue $ s {msMenu=newMenu, msLinkIndexMarker=newIndexPos}-- no need for index marker anymore?
-- ^ move link cursor down
{-
handleEvent s (VtyEvent (Vty.EvKey (Vty.KDown)  [])) =
  M.vScrollBy myNameScroll 1 >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ scroll down, disable active
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'k')  [])) =
  let (_, newIndexPos, newMenu) = updateMenuNewLinkPos s (-1)
  in M.continue $ s {msMenu=newMenu, msLinkIndexMarker=newIndexPos}-- no need for index marker anymore?
handleEvent s (VtyEvent (Vty.EvKey (Vty.KUp)  [])) =
  M.vScrollBy myNameScroll (-1) >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ scroll text up
handleEvent s (VtyEvent (Vty.EvKey (Vty.KHome)  [])) =
  M.vScrollToBeginning myNameScroll >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ return text home
handleEvent s (VtyEvent (Vty.EvKey (Vty.KEnd)  [])) =
  M.vScrollToEnd myNameScroll >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ end of text
handleEvent s (VtyEvent (Vty.EvKey (Vty.KPageDown)  [])) =
  M.vScrollPage myNameScroll Down >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ page down text
handleEvent s (VtyEvent (Vty.EvKey (Vty.KPageUp)  [])) =
  M.vScrollPage myNameScroll Up >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ page up text
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'l') [])) = M.hScrollBy myNameScroll 1 >> M.continue s
-- ^ scroll text right
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'h')  [])) = M.hScrollBy myNameScroll (-1) >> M.continue s
-- ^ scroll text left
handleEvent s (VtyEvent (Vty.EvKey (Vty.KEnter)  [])) = liftIO (requestNewState s) >>= M.continue
-- ^ enter/follow link
-}
handleEvent s _ = continue s

myApp :: MyApp
myApp = App
  { appDraw = drawUi
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure
  , appAttrMap = const theMap
  }

uiMain :: GopherMenu -> IO ()
uiMain initialMenu =
  let linkIndices = buildLinkIndices initialMenu
  in void $ defaultMain myApp $ MyState {msMenu = initialMenu, msLinkIndexMarker = 0, msLinkIndices=linkIndices, msFocused=True}
